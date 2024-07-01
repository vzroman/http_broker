-module(broker_analytics).

-include("http_broker.hrl").
-include("analytics.hrl").

-export([start/0, collect_data/0, convert_to_json/4]).

start() ->
    schedule(),
    collect_data().

schedule() ->
    erlang:send_after(?ENV(schedule_timeout, 10000), self(), collect_data).

collect_data() ->
    QueueDB = persistent_term:get(?DB_REF),
    {QueueCount, ItemCount, AttemptsCount} = get_queue_stats(QueueDB),
    SystemInfo = get_system_info(QueueCount, QueueDB),
    analytics:update_endpoint(request_data, SystemInfo),
    JsonData = convert_to_json(SystemInfo),
    log_data(JsonData),
    {QueueCount, ItemCount, AttemptsCount}.

get_system_info(QueueCount, QueueDB) ->
    Endpoints = get_endpoints(QueueDB),
    EndpointsInfo = maps:from_list([
        {Endpoint, #endpoint_info{
            total_queue_count = get_queue_count_for(Endpoint, QueueDB),
            error_queue_counts = find_error_queue_counts(Endpoint, QueueDB),
            targets = get_targets_for(Endpoint, QueueDB)
        }} || Endpoint <- Endpoints
    ]),
    #system_info{
        total_queue_count = QueueCount,
        error_queues_counts = find_all_error_queue_count(QueueDB),
        endpoints = EndpointsInfo
    }.

find_all_error_queue_count(QueueDB) ->
    fold_with_target_key(QueueDB,
        fun(_Endpoint, ErrorCounts) ->
            ErrorCounts + 1
        end,
        0,
        fun(Key) -> get_attempts_count(QueueDB, Key) > 0 end).

% Helper function for common folding pattern
fold_with_target_key(QueueDB, Fun, InitAcc) ->
    fold_with_target_key(QueueDB, Fun, InitAcc, fun(_) -> true end).

fold_with_target_key(QueueDB, Fun, InitAcc, Condition) ->
    Query = #{},
    zaya_rocksdb:foldl(QueueDB,
        Query,
        fun({Key, Value}, Acc) ->
            case Key of
                {target, Endpoint, Service, _Ref} ->
                    case Condition(Key) of
                        true -> Fun(Endpoint, Service, Value, Acc);
                        false -> Acc
                    end;
                _ -> Acc
            end
        end,
        InitAcc).

get_queue_count_for(Endpoint, QueueDB) ->
    fold_with_target_key(QueueDB, 
        fun(FoundEndpoint, Acc) when FoundEndpoint =:= Endpoint -> 
            Acc + 1;
           (_, Acc) -> 
            Acc 
        end, 
        0).

get_endpoints(QueueDB) -> % todo: check it
    Endpoints = fold_with_target_key(QueueDB, 
                       fun(Endpoint, Acc) ->
                          case lists:member(Endpoint, Acc) of
                              false -> [Endpoint | Acc];
                              _ -> Acc
                          end
                       end,
                       []),
    lists:reverse(Endpoints).

get_targets_for(Endpoint, QueueDB) ->
    Map = fold_with_target_key(QueueDB,
        fun(FoundEndpoint, Service, Value, Acc) when FoundEndpoint =:= Endpoint ->
            maps:update_with(Service,
                             fun(List) -> [Value | List] end,
                             [Value],
                             Acc);
           (_, _, _, Acc) ->
            Acc
        end,
        #{},
        fun(_Key, Value) -> Value end),
    maps:map(fun(_, List) -> lists:reverse(List) end, Map).


find_error_queue_counts(Endpoint, QueueDB) ->
    fold_with_target_key(QueueDB,
        fun(FoundEndpoint, ErrorCount) when FoundEndpoint =:= Endpoint ->
            ErrorCount + 1;
           (_, ErrorCount) ->
            ErrorCount
        end,
        0,
        fun(Key) -> get_attempts_count(QueueDB, Key) > 0 end).


get_queue_stats(QueueDB) ->
    QueueCount = count_targets(QueueDB),
    ItemCount = count_queue_items(QueueDB),
    AttemptsCount = get_attempts_count(QueueDB),
    {QueueCount, ItemCount, AttemptsCount}.

count_targets(QueueDB) ->
    GroupedList = fold_with_target_key(QueueDB,
        fun(Endpoint, Service, Acc) ->
            case lists:member({Endpoint, Service}, Acc) of
                false -> [{Endpoint, Service} | Acc];
                true -> Acc
            end
        end,
        []),
    maps:from_list([{Key, 1} || Key <- GroupedList]).


count_queue_items(QueueDB) ->
    fold_with_target_key(QueueDB, fun(_, Acc) -> Acc + 1 end, 0).

get_attempts_count(QueueDB) ->
    case zaya_rocksdb:last(QueueDB) of
        {Key, _Value} -> get_attempts_count(QueueDB, Key);
        _ -> 0
    end. 

get_attempts_count(QueueDB, Key) ->
    case zaya_rocksdb:read(QueueDB, [Key]) of
        [{_, Value}] ->
            case binary_to_term(Value) of
                {_, Attempts} when is_integer(Attempts) ->
                    Attempts;
                _ ->
                    0
            end;
        _ ->
            0
    end.

log_data(JsonData) ->
    % log to file with write_file
    write_to_file(JsonData),
    ?LOGINFO("Analytics Data: ~s~n", [JsonData]).

write_to_file(JsonData) ->
    {ok, File} = file:open("queue_stats.json", [write]),
    io:format(File, "~s", [JsonData]),
    file:close(File).

convert_to_json(SystemInfo) ->
    #system_info{
        total_queue_count = TotalQueueCount,
        error_queues_counts = ErrorQueuesCounts,
        endpoints = Endpoints
    } = SystemInfo,

    EndpointsJson = maps:map(fun(Endpoint, EndpointInfo) ->
        #endpoint_info{
            total_queue_count = EndpointTotalQueueCount,
            error_queue_counts = EndpointErrorQueueCounts,
            targets = Targets
        } = EndpointInfo,

        TargetsJson = maps:map(fun(Target, TargetInfo) ->
            #{
                queue_count => maps:get(queue_count, TargetInfo, 0),
                state => maps:get(state, TargetInfo, <<"unknown">>),
                attempts => maps:get(attempts, TargetInfo, 0),
                average_10request_duration => get_average_10request_duration(Endpoint, Target),
                last_error => case maps:get(last_error, TargetInfo, undefined) of
                    undefined -> null;
                    LastError -> #{
                        time => format_datetime(maps:get(time, LastError, erlang:system_time(millisecond))),
                        text => maps:get(text, LastError, <<"unknown error">>)
                    }
                end
            }
        end, Targets),

        #{
            total_queue_count => EndpointTotalQueueCount,
            error_queue_counts => EndpointErrorQueueCounts,
            targets => TargetsJson
        }
    end, Endpoints),

    JsonData = #{
        total_queue_count => TotalQueueCount,
        error_queues_counts => ErrorQueuesCounts,
        endpoints => EndpointsJson
    },

    jsx:encode(JsonData).


-spec get_average_10request_duration(Endpoint :: term(), Target :: term()) -> float().
get_average_10request_duration(Endpoint, Target) ->
    Key = {request_times, Endpoint, Target},
    case persistent_term:get(Key, undefined) of
        undefined -> 0.0;
        Times ->
            Length = length(Times),
            case Length of
                0 -> 0.0;
                _ -> lists:sum(Times) / Length
            end
    end.
    
format_datetime(Timestamp) when is_integer(Timestamp) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:system_time_to_universal_time(Timestamp, millisecond),
    Millisecond = Timestamp rem 1000,
    list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B:~3..0B", 
                                    [Year, Month, Day, Hour, Minute, Second, Millisecond])).
    
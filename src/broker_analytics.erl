-module(broker_analytics).

-include("http_broker.hrl").
-include("analytics.hrl").

-export([start/0, collect_data/0, convert_to_json/1]).
-export([log_data/1]).
% TEST EXPORT
-export([count_queues/1, get_attempts_count/2, get_average_10request_duration/2]).
% END TEST EXPORT


%% Types
-type queue_db() :: term().
-type endpoint() :: binary().
-type service() :: binary().
-type key() :: {target, endpoint(), service(), term()}.
-type queue_value() :: binary().

%% API

-spec start() -> ok.
start() ->
    schedule(),
    collect_data().

-spec log_data(binary()) -> ok.
log_data(JsonData) ->
    % log to file with write_file
    write_to_file(JsonData),
    ?LOGINFO("Analytics Data: ~s~n", [JsonData]).

-spec convert_to_json(#system_info{}) -> binary().
convert_to_json(SystemInfo) ->
    #system_info{
        total_queue_count = TotalQueueCount,
        error_queues_counts = ErrorQueuesCounts,
        endpoints = Endpoints
    } = SystemInfo,

    EndpointsJson = maps:map(fun(_Endpoint, EndpointInfo) ->
        endpoint_info_to_map(EndpointInfo)
        end, Endpoints),

    JsonData = #{
        total_queue_count => TotalQueueCount,
        error_queues_counts => ErrorQueuesCounts,
        endpoints => EndpointsJson
    },
    jsx:encode(JsonData).

-spec collect_data() -> #system_info{}.
collect_data() ->
    QueueDB = persistent_term:get(?DB_REF),
    QueueCount = count_queues(QueueDB),
    SystemInfo = get_system_info(QueueCount, QueueDB),
    analytics:update_endpoint(request_data, SystemInfo),
    SystemInfo.

%% Internal functions

-spec schedule() -> reference().
schedule() ->
    erlang:send_after(?ENV(schedule_timeout, 10000), self(), collect_data).


-spec get_system_info(non_neg_integer(), queue_db()) -> #system_info{}.
get_system_info(QueueCount, QueueDB) ->
    Endpoints = get_endpoints(QueueDB),
    EndpointsInfo = create_endpoints_info(Endpoints, QueueDB),
    #system_info{
        total_queue_count = QueueCount,
        error_queues_counts = find_all_error_queue_count(QueueDB),
        endpoints = EndpointsInfo
    }.


-spec create_endpoints_info([endpoint()], queue_db()) -> map().
create_endpoints_info(Endpoints, QueueDB) ->
    maps:from_list([
        {Endpoint, #endpoint_info{
            total_queue_count = get_queue_count_for(Endpoint, QueueDB),
            error_queue_counts = find_error_queue_counts(Endpoint, QueueDB),
            targets = get_targets_for(Endpoint, QueueDB)
        }} || Endpoint <- Endpoints
    ]).


-spec find_all_error_queue_count(queue_db()) -> non_neg_integer().
find_all_error_queue_count(QueueDB) ->
    fold_with_target_key(QueueDB,
        fun(_Endpoint, _Service, _Value, ErrorCounts) ->
            ErrorCounts + 1
        end,
        0,
        fun(Key) -> get_attempts_count(QueueDB, Key) > 0 end).

% Helper function for common folding pattern
-spec fold_with_target_key(queue_db(), fun((endpoint(), service(), queue_value(), Acc) -> Acc), Acc) -> Acc.
fold_with_target_key(QueueDB, Fun, InitAcc) ->
    fold_with_target_key(QueueDB, Fun, InitAcc, fun(_) -> true end).


-spec fold_with_target_key(queue_db(), fun((endpoint(), service(), queue_value(), Acc) -> Acc), Acc, fun((key()) -> boolean())) -> Acc.
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


-spec get_queue_count_for(endpoint(), queue_db()) -> non_neg_integer().
get_queue_count_for(Endpoint, QueueDB) ->
    fold_with_target_key(QueueDB, 
        fun(FoundEndpoint, _Service, _Value, Acc) when FoundEndpoint =:= Endpoint -> 
            Acc + 1;
           (_, _, _ , Acc) -> 
            Acc 
        end, 
        0).


-spec get_endpoints(queue_db()) -> [endpoint()].
get_endpoints(QueueDB) -> 
    Endpoints = fold_with_target_key(QueueDB, 
                       fun(Endpoint, _Service, _Value, Acc) ->
                          case lists:member(Endpoint, Acc) of
                              false -> [Endpoint | Acc];
                              _ -> Acc
                          end
                       end,
                       []),
    lists:reverse(Endpoints).


-spec get_targets_for(endpoint(), queue_db()) -> map().
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
        fun({target, E, _, _}) -> E =:= Endpoint end),
    maps:map(fun(_, List) -> lists:reverse(List) end, Map).


-spec find_error_queue_counts(endpoint(), queue_db()) -> non_neg_integer().
find_error_queue_counts(Endpoint, QueueDB) ->
    fold_with_target_key(QueueDB,
        fun(FoundEndpoint, _Service, _Value, ErrorCount) when FoundEndpoint =:= Endpoint ->
            ErrorCount + 1;
           (_,_,_, ErrorCount) ->
            ErrorCount
        end,
        0,
        fun(Key) -> get_attempts_count(QueueDB, Key) > 0 end).


-spec count_queues(queue_db()) -> non_neg_integer().
count_queues(QueueDB) ->
    GroupedList = fold_with_target_key(QueueDB,
        fun(Endpoint, Service, _Value, Acc) ->
            case lists:member({Endpoint, Service}, Acc) of
                false -> [{Endpoint, Service} | Acc];
                true -> Acc
            end
        end,
        []),
    length(GroupedList).


-spec get_attempts_count(queue_db(), key()) -> non_neg_integer().
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


-spec write_to_file(binary()) -> ok.
write_to_file(JsonData) ->
    {ok, File} = file:open("queue_stats.json", [write]),
    io:format(File, "~s", [JsonData]),
    file:close(File).


-spec endpoint_info_to_map(#system_info{}) -> map().
endpoint_info_to_map(#endpoint_info{
                        total_queue_count = TotalQueueCount,
                        error_queue_counts = ErrorQueueCounts,
                        targets = Targets
                       }) ->

    TargetsJson = maps:map(
            fun(_Target, TargetInfo) ->
                target_info_to_map(TargetInfo) 
            end, 
            Targets),

    #{
        total_queue_count => TotalQueueCount,
        error_queue_counts => ErrorQueueCounts,
        targets => TargetsJson
    }.


-spec target_info_to_map(#target_info{}) -> map().
target_info_to_map(
    #target_info{
        queue_count = QueueCount,
        state = State,
        attempts = Attempts,
        average_10request_duration = AvgDuration,
        last_error = LastError
    }) ->
    #{
        queue_count => QueueCount,
        state => atom_to_binary(State),
        attempts => Attempts,
        average_10request_duration => AvgDuration,
        last_error => error_info_to_map(LastError)    
    }.


-spec error_info_to_map(undefined | #error_info{}) -> map() | null.
error_info_to_map(undefined) -> null;
error_info_to_map(#error_info{time = Time, text = Text}) ->
    #{
        time => list_to_binary(Time),
        text => list_to_binary(Text)
    }.



-spec get_average_10request_duration(Endpoint :: term(), Target :: term()) -> float().
get_average_10request_duration(Endpoint, Target) ->
    Key = {request_times, Endpoint, Target},
    case ets:lookup(request_times_table, Key) of
        [] -> 0.0;
        [{_, Times}] ->
            Length = length(Times),
            case Length of
                0 -> 0.0;
                _ -> lists:sum(Times) / Length
            end
    end.
    
    
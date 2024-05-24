-module(broker_analytics).

-include("http_broker.hrl").

-export([start/0, collect_data/0, convert_to_json/3]).


-define(DECODE_KEY(K), sext:decode(K) ).
-define(DECODE_VALUE(V), binary_to_term(V) ).


JsonData = broker_analytics:convert_to_json(QueueCount, ItemCount, AttemptsCount),
start() ->
	schedule(),
	collect_data()
	.

schedule() ->
	timer:apply_interval(10000, ?MODULE, collect_data, [])
	.

collect_data() ->
  QueueDB = persistent_term:get(?DB_REF),
  % get count of queues
  % get count of items in queue
  % get attempts count for latest item in queue
	{QueueCount, ItemCount, AttemptsCount} = get_queue_stats(QueueDB),
	% log the collected data
	log_data(QueueCount, ItemCount, AttemptsCount),
	{QueueCount, ItemCount, AttemptsCount}
  .


get_queue_stats(QueueDB) ->
  print_queue_contents(QueueDB),
  QueueCount = count_targets(QueueDB),
  ItemCount = count_queue_items(QueueDB),
  AttemptsCount = get_attempts_count(QueueDB),
  {QueueCount, ItemCount, AttemptsCount}
  .

print_queue_contents({ref, Ref, _RefA, _Params2, _Params, "QUEUE"}) ->
	io:format("QueueDB ref: ~p~n", [Ref]),
	io:format("Iterating through the database...~n"),
	case rocksdb:iterator(Ref, []) of
	{ok, Itr} ->
		iterate_print(Itr);
	{error, Reason} ->
		io:format("Failed to create iterator: ~p~n", [Reason])
	end.

iterate_print(Itr) ->
    case rocksdb:iterator_move(Itr, first) of
        {ok, K, V} ->
            print_all_keys({ok, K, V}, Itr, next);
        {error, _Reason} ->
            io:format("No keys found in the database.~n")
    end.

print_all_keys({ok, K, V}, Itr, Next) ->
    io:format("Key: ~p, Value: ~p~n", [?DECODE_KEY(K), ?DECODE_VALUE(V)]),
    case rocksdb:iterator_move(Itr, Next) of
        {ok, K1, V1} ->
            print_all_keys({ok, K1, V1}, Itr, Next);
        {error, _Reason} ->
            io:format("End of iteration.~n")
    end;
print_all_keys(_, _, _) ->
    io:format("No more keys found.~n").

count_targets(QueueDB) ->
	Query = #{},
	IsTargetCheckFun = fun({Key, _Value}) ->
		case Key of
			{target, Endpoint, Service, _Ref} ->
				{true, {Endpoint, Service}};
			_ ->
				false
		end
	end,
	GroupedMap = zaya_rocksdb:foldl(QueueDB, Query, fun({Key, Value}, Acc) ->
		case IsTargetCheckFun({Key, Value}) of
			{true, GroupKey} ->
				maps:update_with(GroupKey, fun(List) -> [Value | List] end, [Value], Acc);
			false ->
				Acc
		end
	end, #{}),
	maps:map(fun(_Key, Val) -> length(Val) end, GroupedMap).

count_queue_items(QueueDB) ->
	Query = #{},
    UserFun = fun({_Key, _Value}, Acc) -> Acc + 1 end,
    zaya_rocksdb:foldl(QueueDB, Query, UserFun, 0)
	.

get_attempts_count(QueueDB) ->
	case zaya_rocksdb:last(QueueDB) of
        {Key, _Value} ->
            case zaya_rocksdb:read(QueueDB, [Key]) of
                [{_, Attempts}] when is_integer(Attempts)->
                    Attempts;
                _ ->
                    0
            end;
        _ ->
            0
    end.

log_data(QueueCount, ItemCount, AttemptsCount) ->
  % log to file with write_file
  write_to_file(QueueCount, ItemCount, AttemptsCount),
  io:format("Queue Count: ~p~nItem Count: ~p~nAttempts Count: ~p~n", [QueueCount, ItemCount, AttemptsCount])
  .

write_to_file(QueueCount, ItemCount, AttemptsCount) ->
	{ok, File} = file:open("queue_stats.json", [write]),
	Json = convert_to_json(QueueCount, ItemCount, AttemptsCount),
	io:format(File, "~s", [Json]),
 	file:close(File)
  .

convert_to_json(QueueCount, ItemCount, AttemptsCount) ->
	ConvertedQueueCount = maps:fold(
		fun({Endpoint, Service}, Val, Acc) ->
			KeyStr = io_lib:format("~s-~s",[binary_to_list(Endpoint), binary_to_list(Service)]),
			maps:put(lists:flatten(KeyStr), Val, Acc)
		end,
		#{},
		QueueCount
	),
	JsonReadyQueueCount = maps:from_list(lists:map(fun({K,V}) -> {list_to_binary(K),V} end, maps:to_list(ConvertedQueueCount))),
	Data = #{
		<<"queue_count">> => JsonReadyQueueCount, 
		<<"item_count">> => ItemCount, 
		<<"attempts_count">> => AttemptsCount
	},
	io:format("Count: ~p Data: ~p ~n", [ConvertedQueueCount, Data]),
	jsx:encode(Data)
	.

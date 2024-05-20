-module(broker_analytics).

-include("http_broker.hrl").

-export([start/0, collect_data/0]).


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
  QueueCount = count_targets(QueueDB),
  ItemCount = count_queue_items(QueueDB),
  AttemptsCount = get_attempts_count(QueueDB),
  {QueueCount, ItemCount, AttemptsCount}
  .


count_targets(QueueDB) ->
	% Todo: implement count_targets
-1
  .

count_queue_items(QueueDB) ->
	Query = #{},
    UserFun = fun({_Key, _Value}, Acc) -> Acc + 1 end,
    zaya_rocksdb:foldl(QueueDB, Query, UserFun, 0)
	.

get_attempts_count(QueueDB, Endpoint, Service, Ref, Target) ->
  case zaya_rocksdb:next(QueueDB, ?TARGET('_','_','_')) of
	{?TARGET(Endpoint, Service, Ref)=Target, Attempts} ->
	  Attempts;
	_ ->
	  0
  end
  .

log_data(QueueCount, ItemCount, AttemptsCount) ->
  % log to file with write_file
  write_to_file(QueueCount, ItemCount, AttemptsCount),
  io:format("Queue Count: ~p~nItem Count: ~p~nAttempts Count: ~p~n", [QueueCount, ItemCount, AttemptsCount])
  .

write_to_file(QueueCount, ItemCount, AttemptsCount) ->
  {ok, File} = file:open("queue_stats.txt", [write]),
  io:format(File, "Queue Count: ~p~nItem Count: ~p~nAttempts Count: ~p~n", [QueueCount, ItemCount, AttemptsCount]),
  file:close(File)
  .

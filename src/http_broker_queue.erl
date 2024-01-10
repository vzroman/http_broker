
-module(http_broker_queue).

-include("http_broker.hrl").

%% API
-export([
  on_init/0,

  enqueue/3,
  next_queue/2,
  remove_queue/3,

  invalid_attempt/3
]).

on_init() ->

  %------------------Queue Dir-----------------------------
  DirectoryName = ?ENV(queue_dir, "QUEUE"),
  Ref =
    case filelib:is_dir(DirectoryName) of
      true ->
        zaya_rocksdb:open(#{dir => DirectoryName});
      false ->
        zaya_rocksdb:create(#{dir => DirectoryName})
    end,

  persistent_term:put(?DB_REF, Ref),
  ok.


enqueue(Request, Endpoint, Targets)->

  QueueDB = persistent_term:get(?DB_REF),
  EndpointRef = {erlang:system_time(millisecond), make_ref()},

  ok = zaya_rocksdb:write(QueueDB,
    [
      {?QUEUE(Endpoint, EndpointRef), Request} |
      [{?TARGET(Endpoint, Service, EndpointRef), _Attempts = 0 } || {_, Group} <- Targets, {Service, _} <- Group]
    ]
  ),

  esubscribe:notify(?SUBSCRIPTIONS_SCOPE, Endpoint, enqueue_request),

  ok.

next_queue( Endpoint, Service )->

  QueueDB = persistent_term:get(?DB_REF),
  case zaya_rocksdb:next(QueueDB, ?TARGET(Endpoint, Service, '_')) of
    {?TARGET(Endpoint, Service, Ref), _Attempts} ->
      case zaya_rocksdb:read( QueueDB, [?QUEUE(Endpoint, Ref)] ) of
        [{_, Request}] ->
          {Ref, Request};
        _->
          undefined
      end;
    _->
      undefined
  end.

remove_queue( Endpoint, Service, Ref )->
  QueueDB = persistent_term:get(?DB_REF),
  zaya_rocksdb:delete( QueueDB, [?TARGET(Endpoint, Service, Ref)] ).

invalid_attempt( Endpoint, Service, Ref )->
  QueueDB = persistent_term:get(?DB_REF),
  case zaya_rocksdb:read( QueueDB, [?TARGET(Endpoint, Service, Ref)] ) of
    [{_, Attempts}] ->
      zaya_rocksdb:write(QueueDB, [ {?TARGET(Endpoint, Service, Ref), Attempts + 1} ]);
    _->
      ok
  end.

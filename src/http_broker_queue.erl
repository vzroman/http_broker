
-module(http_broker_queue).

-include("http_broker.hrl").

%% API
-export([
  on_init/0,

  enqueue/3
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
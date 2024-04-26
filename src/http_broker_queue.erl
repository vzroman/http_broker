
-module(http_broker_queue).

-include("http_broker.hrl").

%% API
-export([
  on_init/0,

  enqueue/3,
  next_queue/2,
  remove_queue/3,

  invalid_attempt/3,

  purge/0,
  purge_until/1
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

purge()->
  DB = persistent_term:get(?DB_REF),
  Endpoints = ?ENV(endpoints, #{}),
  purge( DB, zaya_rocksdb:next(DB, ?QUEUE('_', '_')), Endpoints ).
purge( DB, {?QUEUE(Endpoint, _Ref) = Queue, _Request}, Endpoints )->
  case Endpoints of
    #{ Endpoint := #{ targets := Targets } } ->
      case check_targets( DB, Queue, zaya_rocksdb:next( DB, ?TARGET(Endpoint, '_', '_') ), Targets, _Count = 0 ) of
        TargetsCount when TargetsCount > 0->
          % there are still not sent targets in the queue for the request
          keep_the_request;
        0 ->
          % The request is delivered, it can be purged
          zaya_rocksdb:delete( DB, [Queue] )
      end;
    _->
      % The endpoint doesn't exist in the config
      purge_queue(DB, Queue, zaya_rocksdb:next( DB, ?TARGET(Endpoint, '_', '_') ))
  end,

  purge( DB, zaya_rocksdb:next(DB, Queue), Endpoints );
purge( _DB, _Other, _Endpoints )->
  % There are no more tasks in the queue
  ok.

purge_until( Timestamp )->
  DB = persistent_term:get(?DB_REF),
  purge_until( DB, zaya_rocksdb:next(DB, ?QUEUE('_', '_')), Timestamp ).
purge_until( DB, {?QUEUE(Endpoint, {TS, _Ref}) = Queue, _Request}, Timestamp )->
  if
    TS < Timestamp ->
      % The request is older than the max age
      purge_queue(DB, Queue, zaya_rocksdb:next( DB, ?TARGET(Endpoint, '_', '_') ));
    true ->
      ignore
  end,
  purge_until( DB, zaya_rocksdb:next(DB, Queue), Timestamp );
purge_until( _DB, _Other, _Timestamp )->
  ok.


check_targets( DB, ?QUEUE(Endpoint, Ref)=Queue, {?TARGET(Endpoint, Service, Ref) = Target, _Attempts}, Targets, Count )->

  Count1 =
    case Targets of
      #{ Service := _} ->
        Count + 1;
      _->
        % The target is not in the config any more
        zaya:delete(DB, [Target]),
        Count
    end,

  check_targets( DB, Queue, zaya_rocksdb:next( DB, Target ), Targets, Count1 );

check_targets( _DB, _Queue, _Other, _Targets, Count )->
  Count.

purge_queue(DB, ?QUEUE(Endpoint, Ref)=Queue, {?TARGET(Endpoint, _Service, Ref) = Target, _Attempts})->
  zaya:delete(DB, [Target]),
  purge_queue(DB, Queue, zaya_rocksdb:next( DB, Target ));
purge_queue(DB, Queue, _Other)->
  zaya:delete(DB, [Queue]).
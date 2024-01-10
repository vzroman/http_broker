%% +--------------------------------------------------------------+
%% | Copyright (c) 2023, Faceplate LTD. All Rights Reserved.      |
%% | Author: Karimov Adilbek, @berikka10@gmail.com                |
%% +--------------------------------------------------------------+
-module(http_broker_max_age_service).
-behaviour(gen_server).

-include("http_broker.hrl").

-export([
  start_link/0,
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

%% ====================================================================
%% Gen server functions
%% ====================================================================
start_link() ->
  PID = gen_server:start_link(?MODULE, [], []),
  PID.

init([]) ->
  TimerRef = erlang:start_timer(1000, self(), {max_age, self()}),
  {ok, TimerRef}.

handle_call(_Request, _From, TimerRef) ->
  {reply, ok, TimerRef}.

handle_info({timeout, _TimerRef, {max_age, _PID}}, _) ->
  collect_data(),
  NewTimerRef = erlang:start_timer(1000, self(), {max_age, self()}),
  {noreply, NewTimerRef}.

handle_cast(_Msg, TimerRef) ->
  {noreply, TimerRef}.

%%======================================================================
%%  Stopping
%%======================================================================
terminate(_Reason, _TimerRef) ->
  ok.

code_change(_OldVsn, TimerRef, _Extra) ->
  {ok, TimerRef}.

%% ====================================================================
%% Internal functions
%% ====================================================================
collect_data() ->
  QueueDB = persistent_term:get(db_ref),
  process_queue(QueueDB, ?ENV(endpoints, #{})),
  process_targets(QueueDB).

process_queue(QueueDB, Config) ->
  case zaya_rocksdb:next(QueueDB, ?QUEUE('_', -1)) of
    {?QUEUE(Endpoint, Ref = {TS, _TargetRef}), _Message} ->

      % Calculate age
      MaxAge = ?ENV(max_age, #{}),
      MaxAgeMilliSec = MaxAge * 24 * 60 * 60 * 1000,
      case MaxAgeMilliSec > TS of
        true -> zaya_rocksdb:delete(QueueDB, [?QUEUE(Endpoint, Ref)]);
        false -> ok
      end,

      % Check if Endpoint is present in the configuration
      case maps:find(Endpoint, Config) of
        error -> zaya_rocksdb:delete(QueueDB, [?QUEUE(Endpoint, Ref)]);
        {ok, _Result} -> ok
      end;
    _ ->
      ok
  end.

process_targets(QueueDB) ->
  case zaya_rocksdb:next(QueueDB, ?TARGET('_', '_', -1)) of
    {?TARGET(Endpoint, Service, Ref), _Message} ->
      case zaya_rocksdb:next(QueueDB, {queue, Endpoint, Ref}) of
        undefined -> zaya_rocksdb:delete(QueueDB, ?TARGET(Endpoint, Service, Ref));
        _ -> ok
      end;
    _ -> ok
  end.
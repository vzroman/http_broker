%% +--------------------------------------------------------------+
%% | Copyright (c) 2023, Faceplate LTD. All Rights Reserved.      |
%% | Author: Karimov Adilbek, @berikka10@gmail.com                |
%% +--------------------------------------------------------------+
-module(http_broker_queue_cleanup_service).
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
  TimerRef = erlang:start_timer(1000, self(), {cleanup, self()}),
  {ok, TimerRef}.

handle_call(_Request, _From, TimerRef) ->
  {reply, ok, TimerRef}.

handle_info({timeout, _TimerRef, {cleanup, _PID}}, _) ->
  cleanup_queue(),
  NewTimerRef = erlang:start_timer(1000, self(), {cleanup, self()}),
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
cleanup_queue() ->
  QueueDB = persistent_term:get(db_ref),
  case zaya_rocksdb:next(QueueDB, ?QUEUE('_', -1)) of
    {?QUEUE(Endpoint, Ref), _Message} ->
      case zaya_rocksdb:next(QueueDB, ?TARGET(Endpoint, '_', Ref)) of
        undefined -> zaya_rocksdb:delete(QueueDB, ?QUEUE(Endpoint, Ref));
        _ -> ok
      end;
    undefined -> ok
  end.
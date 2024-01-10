%% +--------------------------------------------------------------+
%% | Copyright (c) 2023, Faceplate LTD. All Rights Reserved.      |
%% | Author: Karimov Adilbek, @berikka10@gmail.com                |
%% +--------------------------------------------------------------+
-module(http_broker_queue_service).
-behaviour(gen_server).

-include("http_broker.hrl").

-export([
  start_link/2,
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
start_link(Endpoint, Target) ->
  PID = gen_server:start_link(?MODULE, [Endpoint, Target], []),
  PID.

init([Endpoint, Target]) ->
  TimerRef = erlang:start_timer(1000, self(), {send_message, Endpoint, Target}),
  {ok, TimerRef}.

handle_call(_Request, _From, TimerRef) ->
  {reply, ok, TimerRef}.

handle_info({timeout, _TimerRef, {send_message, Endpoint, Target}}, _TimerRef) ->
  collect_data(Endpoint, Target),
  NewTimerRef = erlang:start_timer(?ENV(retry_cycle, #{}), self(), {send_message, Endpoint, Target}),
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
collect_data(Endpoint, Target) ->
  QueueDB = persistent_term:get(db_ref),
  case zaya_rocksdb:next(QueueDB, ?TARGET(Endpoint, Target, '_')) of
    {?TARGET(Endpoint, Target, {TS, Ref}), Attempts} ->

      case zaya_rocksdb:next(QueueDB, {queue, {-1, Ref}}) of
        {{queue, Endpoint, {TS, Ref}}, {request, Method, Headers, Body}} ->
          send_message_to_target(QueueDB, {?TARGET(Endpoint, Target, {TS, Ref}), Attempts}, {Method, Headers, Body});
        _ ->
          ?LOGINFO("No more messages in the queue for target ~p.", [Endpoint ++ "->" ++ Target]),
          zaya_rocksdb:delete(QueueDB, [?TARGET(Endpoint, Target, {TS, Ref})])
      end;
    _ -> ok
  end.

send_message_to_target(QueueDB, {?TARGET(Endpoint, Service, {TS, EndpointRef}), Attempts}, {Method, HTTPHeaders, HTTPBody}) ->
  case http_broker_acceptor:send_to_target(list_to_binary(Service), {Method, HTTPHeaders, HTTPBody}) of
    {ok, _Response} ->
      zaya_rocksdb:delete(QueueDB, [{?TARGET(Endpoint, Service, {TS, EndpointRef})}]);
    {error, _Error} ->
      zaya_rocksdb:write(QueueDB, {?TARGET(Endpoint, Service, {TS, EndpointRef}), Attempts + 1})
  end.
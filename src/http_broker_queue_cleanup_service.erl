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

-record(state, {db_ref, timer_ref}).

%% ====================================================================
%% Gen server functions
%% ====================================================================
start_link() ->
  PID = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
  PID.

init([]) ->
  DBRef = http_broker_lib:queue_directory(),
  TimerRef = erlang:start_timer(1000, self(), {cleanup, self()}),
  {ok, #state{db_ref = DBRef, timer_ref = TimerRef}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_info({timeout, _TimerRef, {cleanup, _Pid}}, State) ->
  scan_queue(State#state.db_ref),
  erlang:cancel_timer(State#state.timer_ref),
  NewTimerRef = erlang:start_timer(1000, self(), {cleanup, self()}),
  {noreply, State#state{timer_ref = NewTimerRef}}.

handle_cast(_Msg, State) ->
  {noreply, State}.

%%======================================================================
%%  Stopping
%%======================================================================
terminate(_Reason, State) ->
  zaya_rocksdb:close(State#state.db_ref),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================
scan_queue(DBRef) ->
  Endpoints = http_broker_lib:get_endpoints(),
  Queues = maps:keys(Endpoints),
  lists:foreach(
    fun(EndpointName) ->
      collect_data(DBRef, EndpointName)
    end,
    Queues
  ).

collect_data(DBRef, Endpoint) ->
  case zaya_rocksdb:next(DBRef, {queue, Endpoint, '_'}) of
    {{_Type, EndpointName, {TS, Ref}}, _Message} ->
      case zaya_rocksdb:next(DBRef, {target, "_", "_", {-1, Ref}}) of
        {{_Type, _EndpointName, _Service, {_TargetTS, _TargetRef}}, _Attempts} ->
          ok;
        _L ->
          ?LOGINFO("No more targets in the queue for ~p.", [EndpointName]),
          ?LOGINFO("L: ~p", [_L]),
          delete_from_queue(DBRef, {?QUEUE(EndpointName, {TS, Ref})})
      end;
    _ ->
      ok
  end.

delete_from_queue(DBRef, {?QUEUE(EndpointName, {TS, EndpointRef})}) ->
  zaya_rocksdb:delete(DBRef, [{?QUEUE(EndpointName, {TS, EndpointRef})}]).
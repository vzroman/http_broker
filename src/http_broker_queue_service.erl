%% +--------------------------------------------------------------+
%% | Copyright (c) 2023, Faceplate LTD. All Rights Reserved.      |
%% | Author: Karimov Adilbek, @berikka10@gmail.com                |
%% +--------------------------------------------------------------+
-module(http_broker_queue_service).
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
  TimerRef = erlang:start_timer(1000, self(), {send_message, self()}),
  {ok, #state{db_ref = DBRef, timer_ref = TimerRef}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_info({timeout, _TimerRef, {send_message, _Pid}}, State) ->
  scan_queue(State#state.db_ref),
  erlang:cancel_timer(State#state.timer_ref),
  NewTimerRef = erlang:start_timer(1000, self(), {send_message, self()}),
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
  Targets = http_broker_lib:get_names_and_urls(Endpoints),
  lists:foreach(
    fun({EndpointName, URL}) ->
      collect_data(DBRef, {EndpointName, URL})
    end,
    Targets
  ).

collect_data(DBRef, {EndpointName, URL}) ->
  case zaya_rocksdb:next(DBRef, {target, EndpointName, URL, '_'}) of
    {{_Type, EndpointName, Service, {TS, Ref}}, Attempts} ->
      case zaya_rocksdb:next(DBRef, {queue, {-1, Ref}}) of
        {_MessageKey, [HTTPHeaders, HTTPBody]} ->
          send_message_to_target(DBRef, {?TARGET(EndpointName, Service, {TS, Ref}), Attempts}, [HTTPHeaders, HTTPBody]);
        _ ->
          ?LOGINFO("No more messages in the queue for target ~p.", [{EndpointName, URL}]),
          delete_from_queue(DBRef, [{?TARGET(EndpointName, Service, Ref)}])
      end;
    _ ->
      ok
  end.

send_message_to_target(DBRef, {?TARGET(EndpointName, Service, {TS, EndpointRef}), Attempts}, [HTTPHeaders, HTTPBody]) ->
  case http_broker_lib:send_request_to_target(list_to_binary(Service), HTTPHeaders, HTTPBody) of
    {ok, _Response} ->
      ?LOGINFO("ALL STRATEGY: Successfully sent to target: ~p~n", [{EndpointName, Service}]),
      delete_from_queue(DBRef, {?TARGET(EndpointName, Service, {TS, EndpointRef})});
    {error, _Error} ->
      ?LOGINFO("ALL STRATEGY: Failed to send to target: ~p~n", [{EndpointName, Service}]),
      update_attempts(DBRef, {?TARGET(EndpointName, Service, {TS, EndpointRef}), Attempts + 1})
  end.

delete_from_queue(DBRef, {?TARGET(EndpointName, Service, {TS, EndpointRef})}) ->
  zaya_rocksdb:delete(DBRef, [{?TARGET(EndpointName, Service, {TS, EndpointRef})}]).

update_attempts(DBRef, {?TARGET(EndpointName, Service, {TS, EndpointRef}), Attempts}) ->
  zaya_rocksdb:write(DBRef, [{?TARGET(EndpointName, Service, {TS, EndpointRef}), Attempts}]).


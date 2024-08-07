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

-define(DEFAULT_CYCLE, 3600).
-define(DEFAULT_MAX_AGE, 30).

-record(state,{ cycle }).

%% ====================================================================
%% Gen server functions
%% ====================================================================
start_link() ->
  gen_server:start_link(?MODULE, [], []).

init([]) ->
  Cycle = ?ENV(max_age_cycle, ?DEFAULT_CYCLE) * 1000,
  self() ! on_cycle,
  {ok, #state{ cycle = Cycle}}.

handle_call(Request, _From, State) ->
  ?LOGWARNING("unexpected call request to max age service: ~p",[ Request ]),
  {noreply, State}.

handle_cast(Message, State) ->
  ?LOGWARNING("unexpected cast message to max age service: ~p",[ Message ]),
  {noreply, State}.

handle_info(on_cycle, #state{ cycle = Cycle } = State) ->

  timer:send_after(Cycle, on_cycle),

  MaxAge = ?ENV( max_age, ?DEFAULT_MAX_AGE ) * 86400000,
  try http_broker_queue:purge_until( erlang:system_time(millisecond) - MaxAge )
  catch
    _:E:S -> ?LOGERROR("error on purging queue on max age, error ~p, stack ~p",[ E, S ])
  end,

  {noreply, State};

handle_info(Message, State) ->
  ?LOGWARNING("unexpected info message to max age service: ~p",[ Message ]),
  {noreply, State}.

%%======================================================================
%%  Stopping
%%======================================================================
terminate(_Reason, _TimerRef) ->
  ok.

code_change(_OldVsn, TimerRef, _Extra) ->
  {ok, TimerRef}.

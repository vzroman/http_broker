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

-define(MIN_CYCLE, 1).
-define(MAX_CYCLE, 3600).
-define(DEFAULT_CYCLE, 3600).

-record(state,{ cycle }).

%% ====================================================================
%% Gen server functions
%% ====================================================================
start_link() ->
  gen_server:start_link(?MODULE, [], []).

init([]) ->
  Cycle = ?ENV(purge_cycle, ?DEFAULT_CYCLE),
  CorrectedCycle = http_broker_queue:check_settings(Cycle, ?MIN_CYCLE, ?MAX_CYCLE),
  CycleMilliseconds = CorrectedCycle * 1000,

  self() ! on_cycle,
  {ok, #state{ cycle = CycleMilliseconds }}.

handle_call(Request, _From, State) ->
  ?LOGWARNING("unexpected call request to cleanup service: ~p",[ Request ]),
  {noreply, State}.

handle_cast(Message, State) ->
  ?LOGWARNING("unexpected cast message to cleanup service: ~p",[ Message ]),
  {noreply, State}.

handle_info(on_cycle, #state{ cycle = Cycle } =State) ->

  timer:send_after(Cycle, on_cycle),

  try http_broker_queue:purge()
  catch
    _:E:S -> ?LOGERROR("error on purging queue, error ~p, stack ~p",[ E, S ])
  end,

  {noreply, State};

handle_info(Message, State) ->
  ?LOGWARNING("unexpected info message to cleanup service: ~p",[ Message ]),
  {noreply, State}.

%%======================================================================
%%  Stopping
%%======================================================================
terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

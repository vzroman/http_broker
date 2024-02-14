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

-define(MIN_CYCLE, 1).
-define(MAX_CYCLE, 3600000).
-define(DEFAULT_CYCLE, 1000).

-record(state,{ endpoint, target, cycle }).
%% ====================================================================
%% Gen server functions
%% ====================================================================
start_link(Endpoint, Target) ->
  gen_server:start_link(?MODULE, [Endpoint, Target], []).

init([Endpoint, Target]) ->

  Cycle = http_broker_queue:check_settings(retry_cycle, ?ENV(retry_cycle, ?DEFAULT_CYCLE), ?MIN_CYCLE, ?MAX_CYCLE),
  esubscribe:subscribe(?SUBSCRIPTIONS_SCOPE, Endpoint, _PID=self(), _Nodes = [node()]),

  self() ! on_cycle,

  {ok, #state{ endpoint = Endpoint, target = Target, cycle = Cycle }}.

handle_call(Request, _From, State) ->
  ?LOGWARNING("unexpected call request to send target service: ~p",[ Request ]),
  {noreply, State}.

handle_cast(Message, State) ->
  ?LOGWARNING("unexpected cast message to send target service: ~p",[ Message ]),
  {noreply, State}.

%%---------------THE LOOP----------------------------------------------
handle_info(on_cycle, #state{cycle = Cycle, endpoint = Endpoint, target = Target} = State) ->
  timer:send_after(Cycle, on_cycle),

  try handle_queue(Endpoint, Target)
  catch
    _:E:S->
      ?LOGERROR("unable to handle queue ~p:~p, error ~p, stack ~p",[ Endpoint, Target, E, S ])
  end,

  {noreply, State};

handle_info({?SUBSCRIPTIONS_SCOPE, Endpoint, enqueue_request, _Node, _Actor}, #state{
  endpoint = Endpoint,
  target = Target
} = State) ->

  try handle_queue(Endpoint, Target)
  catch
    _:E:S->
      ?LOGERROR("unable to handle queue ~p:~p, error ~p, stack ~p",[ Endpoint, Target, E, S ])
  end,

  {noreply, State};

handle_info(Message, State) ->

  ?LOGWARNING("unexpected info message to send target service: ~p",[ Message ]),

  {noreply, State}.




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
handle_queue(Endpoint, {Service, _} = Target) ->
  case http_broker_queue:next_queue( Endpoint, Service ) of
    undefined ->
      ok;
    {Ref, Request}->
      case http_broker_acceptor:send_to_target(Target, Request) of
        {ok, _Response} ->
          ?LOGDEBUG("~p:~p ref ~p success",[ Endpoint, Service, Ref ]),
          http_broker_queue:remove_queue( Endpoint, Service, Ref ),
          handle_queue( Endpoint, Target );
        {error, Error, _Response} ->
          ?LOGWARNING("~p:~p ref ~p error ~p",[ Endpoint, Service, Ref, Error ]),
          http_broker_queue:invalid_attempt( Endpoint, Service, Ref )
      end
  end.

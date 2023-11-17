%%-----------------------------------------------------------------
%% Copyright (c) 2018, Vozzhenikov Roman. All Rights Reserved.
%% Author: Vozzhenikov Roman, vzroman@gmail.com
%%-----------------------------------------------------------------

-module(http_broker_accepter).

-behaviour(cowboy_handler).

%%=================================================================
%%	Cowboy behaviour
%%=================================================================
-export([
  init/2,
  terminate/3
]).

init(Req0, Opts) ->
  Req = cowboy_req:reply(200, #{}, <<"TestResponse">>, Req0),
  {ok,Body,_} = cowboy_req:read_body(Req0),
  io:format("~nDEBUG: Request~p", [Req]),
  io:format("~nDEBUG: Body ~p", [Body]),
  {ok, Req, Opts}.

terminate(_Reason, _Req, _State) ->
  ok.

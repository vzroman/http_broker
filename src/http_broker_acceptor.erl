%% +--------------------------------------------------------------+
%% | Copyright (c) 2023, Faceplate LTD. All Rights Reserved.      |
%% | Author: Karimov Adilbek, @berikka10@gmail.com                |
%% +--------------------------------------------------------------+

-module(http_broker_acceptor).

-include("http_broker.hrl").

-behaviour(cowboy_handler).

%%=================================================================
%%	Cowboy behaviour
%%=================================================================
-export([
  init/2,
  terminate/3
]).

init(Req, Opts) ->
  Req0 = cowboy_req:reply(200, #{}, <<"TestResponse222">>, Req),
  {ok,Body,_} = cowboy_req:read_body(Req0),
  io:format("~nDEBUG: Request~p", [Req0]),
  io:format("~nDEBUG: Body ~p", [Body]),

  http_broker_sender:send_request("endpoint1", Body),

  {ok, Req0, Opts}.


terminate(_Reason, _Req, _State) ->
  ok.
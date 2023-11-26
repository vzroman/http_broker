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
  Response = cowboy_req:reply(200, #{}, <<"ONE_STRATEGY">>, Req),

  HTTPBody = http_broker_lib:get_http_body(Response),
  HTTPHeaders = http_broker_lib:get_http_headers(Response),

  http_broker_sender:send_request(HTTPHeaders, HTTPBody),

  {ok, Response, Opts}.


terminate(_Reason, _Req, _State) ->
  ok.
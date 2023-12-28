%% +--------------------------------------------------------------+
%% | Copyright (c) 2023, Faceplate LTD. All Rights Reserved.      |
%% | Author: Karimov Adilbek, @berikka10@gmail.com                |
%% +--------------------------------------------------------------+

-module(http_broker_acceptor).

-include("http_broker.hrl").

-behaviour(cowboy_handler).

-record(request,{ method, headers, body }).
-record(response,{ code, headers, body }).

%%=================================================================
%%	Cowboy behaviour
%%=================================================================
-export([
  init/2,
  terminate/3
]).

init(Request, Config) ->


  Response = handle_req( Config, Request ),

%%  Response = cowboy_req:reply(200, #{}, <<"">>, Req),
%%
%%  HTTPBody = http_broker_lib:get_http_body(Response),
%%  HTTPHeaders = http_broker_lib:get_http_headers(Response),
%%%%  A = zaya_rocksdb:find(http_broker_lib:queue_directory(), {{target, '_', '_'}, 1}),
%%  Req0 = http_broker_sender:send_request(HTTPHeaders, HTTPBody),
%%  HTTPBody0 = http_broker_lib:get_http_body(Response),
%%  HTTPHeaders0 = http_broker_lib:get_http_headers(Response),
%%  Response2 = cowboy_req:reply(200, HTTPBody0, HTTPHeaders0, Req0),
  {ok, Response, Config}.


terminate(_Reason, _Req, _State) ->
  ok.

handle_req( #{ strategy := one, targets := Targets}, Request )->


  { Body, Request1} = read_body( Request ),

  TargetsByOrder =
    maps:groups_from_list(fun({ _Target, Config })->maps:get( order, Config, 0 )  end , maps:to_list(Targets) ),

  try_send(maps:to_list( TargetsByOrder ), #request{
    method = cowboy_req:method(Request1),
    headers = cowboy_req:headers(Request1),
    body = Body
  }).



read_body( Req )->
  read_body( Req, <<>> ).
read_body( Req, Body )->
  case cowboy_req:read_body( Req ) of
    {ok, Data, Req1}->
      { <<Body/binary, Data/binary>>, Req1};
    {more, Data, Req1}->
      read_body( Req1, <<Body/binary, Data/binary >> )
  end.


try_send( Targets, Request)->
  {Target, RestTargets } =

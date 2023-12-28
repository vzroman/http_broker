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

init(CowboyRequest, #{
  strategy := Strategy,
  targets := Targets
} = Config) ->

  { Body, CowboyRequest1} = read_body( CowboyRequest ),

  Request = #request{
    method = cowboy_req:method(CowboyRequest1),
    headers = cowboy_req:headers(CowboyRequest1),
    body = Body
  },

  {Target, Response} = handle_req( Targets, Request),

  #response{
    code = ResponseCode,
    headers = ResponseHeaders,
    body = ResponseBody
  } = Response,

  if
    Strategy =:= all andalso (ResponseCode >= 200 orelse ResponseCode =< 299)  ->
      queue_request(Request, rest_targets( Target, Targets ) );
    true ->
      ignore
  end,


  CowboyResponse = cowboy_req:reply(ResponseCode, ResponseHeaders, ResponseBody, CowboyRequest1),

%%  Response = cowboy_req:reply(200, #{}, <<"">>, Req),
%%
%%  HTTPBody = http_broker_lib:get_http_body(Response),
%%  HTTPHeaders = http_broker_lib:get_http_headers(Response),
%%%%  A = zaya_rocksdb:find(http_broker_lib:queue_directory(), {{target, '_', '_'}, 1}),
%%  Req0 = http_broker_sender:send_request(HTTPHeaders, HTTPBody),
%%  HTTPBody0 = http_broker_lib:get_http_body(Response),
%%  HTTPHeaders0 = http_broker_lib:get_http_headers(Response),
%%  Response2 = cowboy_req:reply(200, HTTPBody0, HTTPHeaders0, Req0),
  {ok, CowboyResponse, Config}.


terminate(_Reason, _Req, _State) ->
  ok.

handle_req( Targets, Request )->
  {Target, RestTargets } = pick_target( Targets ),
  case send_to_target( Target, Request ) of
    {ok, Response} ->
      {Target, Response};
    {error, Response}->
      if
        map_size( RestTargets ) =:= 0 ->
          {Target, Response};
        true ->
          try_send( RestTargets, Request )
      end
  end.


read_body( Req )->
  read_body( Req, <<>> ).
read_body( Req, Body )->
  case cowboy_req:read_body( Req ) of
    {ok, Data, Req1}->
      { <<Body/binary, Data/binary>>, Req1};
    {more, Data, Req1}->
      read_body( Req1, <<Body/binary, Data/binary >> )
  end.



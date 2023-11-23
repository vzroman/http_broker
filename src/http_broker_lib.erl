%% +--------------------------------------------------------------+
%% | Copyright (c) 2023, Faceplate LTD. All Rights Reserved.      |
%% | Author: Karimov Adilbek, @berikka10@gmail.com                |
%% +--------------------------------------------------------------+

-module(http_broker_lib).

-include("http_broker.hrl").

-export([
  get_http_headers/1,
  get_http_body/1,
  get_endpoint_names/0,
  get_targets/0,
  get_strategy/1
]).

get_http_body(Response) ->
  {ok, HTTPBody, _} = cowboy_req:read_body(Response),
  HTTPBody.

get_http_headers(Response) ->
  HTTPHeaders = cowboy_req:headers(Response),
  lists:map(fun({Key, Value}) -> {binary_to_list(Key), binary_to_list(Value)} end, maps:to_list(HTTPHeaders)).

get_endpoint_names() ->
  Endpoints = ?ENV(endpoints, #{}),
  maps:keys(Endpoints).

get_targets() ->
  Maps = maps:values(?ENV(endpoints, #{})),
  lists:flatmap(fun(#{ targets := TargetMap })
    -> maps:keys(TargetMap);
    (_) -> []
  end, Maps).


get_strategy(Endpoint) ->
  case maps:find(Endpoint, ?ENV(endpoints, #{})) of
    {ok, #{strategy := Strategy}} ->
      Strategy;
    _ ->
      undefined
  end.
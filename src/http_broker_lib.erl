%% +--------------------------------------------------------------+
%% | Copyright (c) 2023, Faceplate LTD. All Rights Reserved.      |
%% | Author: Karimov Adilbek, @berikka10@gmail.com                |
%% +--------------------------------------------------------------+

-module(http_broker_lib).

-include("http_broker.hrl").

-export([
  get_http_headers/1,
  get_http_body/1,
  get_endpoints/0,
  sort_endpoints/1
]).

get_http_headers(Response) ->
  HTTPHeaders = cowboy_req:headers(Response),
  lists:map(fun({Key, Value}) -> {binary_to_list(Key), binary_to_list(Value)} end, maps:to_list(HTTPHeaders)).

get_http_body(Response) ->
  {ok, HTTPBody, _} = cowboy_req:read_body(Response),
  HTTPBody.

get_endpoints() ->
  ?ENV(endpoints, #{}).

sort_endpoints(Endpoints) ->
  EndpointsWithDefaults = maps:fold(fun default_order/3, [], Endpoints),
  GroupedEndpoints = maps:groups_from_list(fun get_order/1, EndpointsWithDefaults),
  maps:values(GroupedEndpoints).

default_order(_Key, #{strategy := Strategy, targets := Targets}, Acc) ->
  case maps:to_list(Targets) of
    [{Target, #{order := Order}}] when is_integer(Order) ->
      [{Order, Target, Strategy} | Acc];
    [{Target, _}] ->
      [{0, Target, Strategy} | Acc];
    _ -> throw(bad_arg)
  end.

get_order({Order, _Target, _Strategy}) -> Order.
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
  combine_endpoints/1,
  group_orders/1,
  sort_grouped_orders/1
]).

get_http_headers(Response) ->
  HTTPHeaders = cowboy_req:headers(Response),
  lists:map(fun({Key, Value}) -> {binary_to_list(Key), binary_to_list(Value)} end, maps:to_list(HTTPHeaders)).

get_http_body(Response) ->
  {ok, HTTPBody, _} = cowboy_req:read_body(Response),
  HTTPBody.

get_endpoints() ->
  ?ENV(endpoints, #{}).

combine_endpoints(Endpoints) ->
  lists:flatten([
    [{Value1, URL, Strategy} || {_, #{strategy := Strategy, targets := Targets}} <- maps:to_list(Endpoints),
      {URL, #{order := Value1}} <- maps:to_list(Targets)]
  ]).

group_orders(OrdersURLs) ->
  lists:foldl(
    fun({Order, URL, Strategy}, Acc) ->
      case lists:keyfind(Order, 1, Acc) of
        false ->
          [{Order, [{URL, Strategy}]} | Acc];
        {Order, URLStrategies} ->
          UpdatedURLStrategies = lists:keyreplace(Order, 1, Acc, {Order, [{URL, Strategy} | URLStrategies]}),
          UpdatedURLStrategies
      end
    end, [], OrdersURLs).

sort_grouped_orders(GroupedOrders) ->
  lists:sort(fun({Order1, _}, {Order2, _}) -> Order1 =< Order2 end, GroupedOrders).

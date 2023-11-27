%% +--------------------------------------------------------------+
%% | Copyright (c) 2023, Faceplate LTD. All Rights Reserved.      |
%% | Author: Karimov Adilbek, @berikka10@gmail.com                |
%% +--------------------------------------------------------------+

-module(http_broker_sender).

-include("http_broker.hrl").

-export([
  send_request/2
]).

send_request(Headers, Body) ->
  Endpoints = http_broker_lib:get_endpoints(),
  SortedInfo = http_broker_lib:sort_endpoints(Endpoints),
  io:format("OrdersURLs: ~p ~n", [SortedInfo]),
  handle_sorted_info(Headers, Body, SortedInfo).

handle_sorted_info(Headers, Body, SortedOrders) ->
  lists:foreach(
    fun(Order) ->
      random_pick(Headers, Body, Order)
    end, SortedOrders).

random_pick(Headers, Body, [{_, URL, Strategy}]) ->
  handle_strategy(Strategy, [URL], Headers, Body),
  ok;
random_pick(Headers, Body, Orders) ->
  RandomIndex = rand:uniform(length(Orders)),
  {_, URL, Strategy} = RandomOrder = lists:nth(RandomIndex, Orders),
  handle_strategy(Strategy, [URL], Headers, Body),
  UpdatedOrders = lists:delete(RandomOrder, Orders),
  random_pick(Headers, Body, UpdatedOrders).

handle_strategy(one, URL, Headers, Body) ->
  case send_request_to_target(list_to_binary(URL), Headers, Body) of
    {ok, Response} ->
      io:format("Successfully sent to ~p: ~p~n", [URL, Response]);
    {error, Reason} ->
      io:format("Failed to send to ~p: ~p~n", [URL, Reason])
  end;
handle_strategy(all, _URL, _Headers, _Body) ->
  io:format("TODO: all strategy ~n");
handle_strategy(_E , _, _, _) ->
  io:format("Unsupported strategy: ~p ~n", [_E]).


send_request_to_target(<<"http", _/binary>> = URL, HTTPHeaders, HTTPBody) ->
  HTTPMethod  = post,
  HTTPRequest = {URL, HTTPHeaders, "application/json", HTTPBody},
  HTTPOptions = [],
  Options     = [{body_format, binary}],

  try
    case httpc:request(HTTPMethod, HTTPRequest, HTTPOptions, Options) of
      {ok, {{_, Code, _}, _ResponseHeaders, _ResponseBody}} when Code >= 200, Code =< 299 ->
        jsx:from_json(_ResponseBody);
      {ok, {{_, Code, _}, _, _ResponseBody}} ->
        throw({error, {unexpected_response_code, Code}});
      {error, Error} ->
        throw({error, Error})
    end
  catch
    _:HTTPError ->
      {error, HTTPError}
  end.
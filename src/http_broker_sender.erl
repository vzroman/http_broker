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

  CombinedInfo = http_broker_lib:combine_endpoints(Endpoints),
  io:format("OrdersURLs: ~p ~n", [CombinedInfo]),

  GroupedByOrders = http_broker_lib:group_orders(CombinedInfo),
  io:format("GroupedOrders: ~p ~n", [GroupedByOrders]),

  SortedInfo = http_broker_lib:sort_grouped_orders(GroupedByOrders),
  io:format("SortedOrders: ~p ~n", [SortedInfo]),

  handle_sorted_info(Headers, Body, SortedInfo).

handle_sorted_info(Headers, Body, SortedOrders) ->
  lists:foreach(
    fun({Order, URLs}) ->
      io:format("~nOrder: ~p ~n", [Order]),
      io:format("~nTargetURLs : ~p ~n", [URLs]),

      % Recursively handle each URL in the order
      random_pick(Headers, Body, URLs)

    end, SortedOrders).

random_pick(_Headers, _Body, []) ->
  ok;
random_pick(Headers, Body, URLs) ->
  {URL, Strategy} = lists:nth(rand:uniform(length(URLs)), URLs),
  handle_strategy(Strategy, [URL], Headers, Body),

  UpdatedTargetURLs = lists:keydelete(URL, 1, URLs),
  random_pick(Headers, Body, UpdatedTargetURLs).


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
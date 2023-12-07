%% +--------------------------------------------------------------+
%% | Copyright (c) 2023, Faceplate LTD. All Rights Reserved.      |
%% | Author: Karimov Adilbek, @berikka10@gmail.com                |
%% +--------------------------------------------------------------+

-module(http_broker_sender).

-include("http_broker.hrl").

-export([
  send_request/2
]).

send_request(HTTPHeaders, HTTPBody) ->
  Endpoints = http_broker_lib:get_endpoints(),
  Targets = http_broker_lib:group_endpoints(Endpoints),
  ?LOGINFO("Targets: ~p ~n", [Targets]),
  handle_sorted_info(HTTPHeaders, HTTPBody, Targets).

handle_sorted_info(HTTPHeaders, HTTPBody, Targets) ->
  lists:foreach(
    fun(Target) ->
      random_pick(HTTPHeaders, HTTPBody, Target)
    end, Targets).

random_pick(HTTPHeaders, HTTPBody, [{_, URL, Strategy}]) ->
  handle_strategy(Strategy, [URL], HTTPHeaders, HTTPBody),
  ok;
random_pick(HTTPHeaders, HTTPBody, Orders) ->
  RandomIndex = rand:uniform(length(Orders)),
  {_, URL, Strategy} = RandomOrder = lists:nth(RandomIndex, Orders),

  handle_strategy(Strategy, [URL], HTTPHeaders, HTTPBody),
  UpdatedOrders = lists:delete(RandomOrder, Orders),

  random_pick(HTTPHeaders, HTTPBody, UpdatedOrders).

handle_strategy(one, URL, HTTPHeaders, HTTPBody) ->
  case send_request_to_target(list_to_binary(URL), HTTPHeaders, HTTPBody) of
    {ok, Response} ->
      ?LOGINFO("Successfully sent to ~p: ~p~n", [URL, Response]);
    {error, Reason} ->
      ?LOGINFO("Failed to send to ~p: ~p~n", [URL, Reason])
  end;
handle_strategy(all, _URL, _HTTPHeaders, _HTTPBody) ->
  ?LOGINFO("TODO: all strategy ~n");
handle_strategy(InvalidStrategy, _, _, _) ->
  ?LOGINFO("Unsupported strategy: ~p ~n", [InvalidStrategy]).

send_request_to_target(<<"http", _/binary>> = URL, HTTPHeaders, HTTPBody) ->
  HTTPMethod  = post,
  HTTPRequest = {URL, HTTPHeaders, "application/json", HTTPBody},
  HTTPOptions = [],
  Options     = [{body_format, binary}],

  try
    case httpc:request(HTTPMethod, HTTPRequest, HTTPOptions, Options) of
      {ok, {{_, Code, _}, _ResponseHeaders, _ResponseBody}} when Code >= 200, Code =< 299 ->
        ?LOGINFO("REQUEST: ~p ~n", [HTTPRequest]),
        {ok, HTTPRequest};
    {ok, {{_, Code, _}, _, _ResponseBody}} ->
        throw({error, {unexpected_response_code, Code}});
      {error, Error} ->
        throw({error, Error})
    end
  catch
    _:HTTPError ->
      {error, HTTPError}
  end.
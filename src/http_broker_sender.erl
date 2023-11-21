%% +--------------------------------------------------------------+
%% | Copyright (c) 2023, Faceplate LTD. All Rights Reserved.      |
%% | Author: Karimov Adilbek, @berikka10@gmail.com                |
%% +--------------------------------------------------------------+

-module(http_broker_sender).

-include("http_broker.hrl").

-export([init/2, terminate/3]).

-export([send_request/2, send_request_one/2, send_request_all/0]).

init(Req0, Opts) ->
  {ok, Req0, Opts}.

terminate(_Reason, _Req, _State) ->
  ok.

send_request(Endpoint, Body) ->
  Targets = http_broker_lib:get_targets(),
  Strategy = http_broker_lib:get_strategy(Endpoint),
  io:format("Targets: ~p~n", [Targets]),
  io:format("Strategy: ~p~n", [Strategy]),
  case Strategy of
    one ->
      send_request_one(<<"http://127.0.0.1:7000/endpoint3">>, Body);
    all ->
      io:format("~nDEBUG: Unsupported strategy: ~p~n", [Strategy]);
    _ ->
      io:format("~nERROR: Unsupported strategy: ~p~n", [Strategy])
  end.


send_request_one(<<"http", _/binary>> = URL, Data) ->
  HTTPMethod  = post,
  HTTPHeaders = [{"Content-Type", "application/json"} | []],
  HTTPRequest = {URL, HTTPHeaders, "application/json", Data},
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


send_request_all() ->
  %TODO: ask about the need for this function
  ok.
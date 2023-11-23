  %% +--------------------------------------------------------------+
  %% | Copyright (c) 2023, Faceplate LTD. All Rights Reserved.      |
  %% | Author: Karimov Adilbek, @berikka10@gmail.com                |
  %% +--------------------------------------------------------------+

  -module(http_broker_sender).

  -include("http_broker.hrl").

  -export([
    send_request/2,
    send_request_one/3,
    send_request_all/0
  ]).

  send_request(Headers, Body) ->
    Endpoints = http_broker_lib:get_endpoint_names(),
    TargetURLs = http_broker_lib:get_targets(),

    lists:foreach(
      fun(Endpoint) ->
        Strategy = http_broker_lib:get_strategy(Endpoint),
        case Strategy of
          one ->
            case send_request_one(TargetURLs, Headers, Body) of
              {ok, _Connected} ->
                io:format("Connection successful. Stopping further attempts.~n"),
                ok;
              _NotConnected ->
                ok
            end;
          all ->
            io:format("~nDEBUG: TODO: ~p~n"), [Strategy];
          _ ->
            io:format("~nERROR: Unsupported strategy ~p for endpoint ~p ~n", [Strategy, Endpoint])
        end
      end,
      Endpoints
    ).

  send_request_one([URL | RestTargetURL], Headers, Body) ->
    case send_request_to_target(list_to_binary(URL), Headers, Body) of
      {ok, Response} ->
        io:format("Successfully sent to ~p: ~p~n", [URL, Response]);
      {error, Reason} ->
        io:format("Failed to send to ~p: ~p~n", [URL, Reason]),
        send_request_one(RestTargetURL, Headers, Body)
    end;
  send_request_one([], _, _) ->
    ok.

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


  send_request_all() ->
    %TODO: ask about the need for this function
    ok.
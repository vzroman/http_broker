%%-----------------------------------------------------------------
%% Copyright (c) 2018, Vozzhenikov Roman. All Rights Reserved.
%% Author: Vozzhenikov Roman, vzroman@gmail.com
%%-----------------------------------------------------------------

-module(http_broker_acceptor).

-include("http_broker.hrl").

-behaviour(cowboy_handler).

%%=================================================================
%%	Cowboy behaviour
%%=================================================================
-export([
  init/2,
  terminate/3
]).

init(Req0, Opts) ->
%%  Req = cowboy_req:reply(200, #{}, <<"TestResponse">>, Req0),
  {ok,Body,_} = cowboy_req:read_body(Req0),
  io:format("~nDEBUG: Request~p", [Req0]),
  io:format("~nDEBUG: Body ~p", [Body]),

%%  Env = http_broker_sup:get_targets(?ENV(endpoints, #{})),
  send_request(<<"http://127.0.0.1:7000/endpoint2">>, Body),

  {ok, Req0, Opts}.


terminate(_Reason, _Req, _State) ->
  ok.

send_request(<<"http", _/binary>> = URL, Data) ->
  HTTPMethod  = post,
  HTTPHeaders = [{"Content-Type", "application/json"} | []],
  HTTPRequest = {URL, HTTPHeaders, "application/json", Data},
  HTTPOptions = [],
  Options     = [{body_format, binary}],

  try
    case httpc:request(HTTPMethod, HTTPRequest, HTTPOptions, Options) of
      {ok, {{_, Code, _}, _ResponseHeaders, _ResponseBody}} when Code >= 200, Code =< 299 ->
        fp_lib:from_json(_ResponseBody);
      {ok, {{_, Code, _}, _, _ResponseBody}} ->
        throw({error, {unexpected_response_code, Code}});
      {error, Error} ->
        throw({error, Error})
    end
  catch
    _:HTTPError ->
      {error, HTTPError}
  end.
%% +--------------------------------------------------------------+
%% | Copyright (c) 2023, Faceplate LTD. All Rights Reserved.      |
%% | Author: Karimov Adilbek, @berikka10@gmail.com                |
%% +--------------------------------------------------------------+

-module(http_broker_acceptor).

-include("http_broker.hrl").

-behaviour(cowboy_handler).

-record(request, {method, headers, body}).
-record(response, {code, headers, body}).

-define(RAND(List),
        begin
          _@I = erlang:phash2(make_ref(), length(List)),
          lists:nth(_@I + 1, List)
        end).

%%=================================================================
%%        Cowboy behaviour
%%=================================================================
-export([init/2, terminate/3, send_to_target/2]).

init(CowboyRequest, Config) ->
  case cowboy_req:path(CowboyRequest) of
    <<"/queue_stats">> ->
      handle_queue_stats(CowboyRequest, Config);
    _ ->
      handle_request(CowboyRequest, Config)
  end.

handle_queue_stats(CowboyRequest, State) ->
  {QueueCount, ItemCount, AttemptsCount} = broker_analytics:collect_data(),
  JsonData = broker_analytics:convert_to_json(QueueCount, ItemCount, AttemptsCount),
  Headers = #{<<"content-type">> => <<"application/json">>},
  CowboyResponse = cowboy_req:reply(200, Headers, JsonData, CowboyRequest),
  {ok, CowboyResponse, State}.

handle_request(CowboyRequest,
               #{strategy := Strategy,
                 targets := Targets,
                 endpoint := Endpoint} =
                 Config) ->
  {Body, CowboyRequest1} = read_body(CowboyRequest),

  Request =
    #request{method = parse_method(cowboy_req:method(CowboyRequest1)),
             headers = cowboy_req:headers(CowboyRequest1),
             body = Body},

  {Target, Response} = try_send(Targets, Request),
  #response{code = ResponseCode,
            headers = ResponseHeaders,
            body = ResponseBody} =
    Response,

  if Strategy =:= all andalso (ResponseCode >= 200 orelse ResponseCode =< 299) ->
       % If the process crashes here then the client will receive
       %   500 Server Internal Error
       % and the request should be handled as not successful
       http_broker_queue:enqueue(Request, Endpoint, rest_targets(Target, Targets));
     true ->
       ignore
  end,

  AdaptedHeaders =
    maps:from_list([{unicode:characters_to_binary(Header),
                     unicode:characters_to_binary(Value)}
                    || {Header, Value} <- ResponseHeaders]),
  CowboyResponse =
    cowboy_req:reply(ResponseCode, AdaptedHeaders, ResponseBody, CowboyRequest1),
  {ok, CowboyResponse, Config}.

terminate(_Reason, _Req, _State) ->
  ok.

try_send(Targets, Request) ->
  {Target, RestTargets} = pick_target(Targets),
  case send_to_target(Target, Request) of
    {ok, Response} ->
      {Target, Response};
    {error, Error, Response} ->
      ?LOGWARNING("unable send request to target ~p, error ~p", [Target, Error]),
      if length(RestTargets) =:= 0 ->
           {Target, Response};
         true ->
           try_send(RestTargets, Request)
      end
  end.

pick_target([{Order, Targets} | Rest]) when length(Targets) > 0 ->
  Target = ?RAND(Targets),
  RestTargets =
    case lists:delete(Target, Targets) of
      [] ->
        Rest;
      RestOrderTargets ->
        [{Order, RestOrderTargets} | Rest]
    end,
  {Target, RestTargets}.

read_body(Req) ->
  read_body(Req, <<>>).

read_body(Req, Body) ->
  case cowboy_req:read_body(Req) of
    {ok, Data, Req1} ->
      {<<Body/binary, Data/binary>>, Req1};
    {more, Data, Req1} ->
      read_body(Req1, <<Body/binary, Data/binary>>)
  end.

send_to_target({URL, Params},
               #request{headers = Headers,
                        body = Body,
                        method = Method}) ->
  AdaptedHeaders = [{binary_to_list(K), V} || {K, V} <- maps:to_list(Headers)],

  ContentType =
    case Headers of
      #{<<"content-type">> := _CT} ->
        unicode:characters_to_list(_CT);
      _ ->
        "application/json"
    end,

  Request = {URL, AdaptedHeaders, ContentType, Body},
  HTTPOptions =
    case Params of
      #{ssl := SSL} ->
        [{ssl, SSL}];
      _ ->
        []
    end,

  try
    case httpc:request(Method, Request, HTTPOptions, [{body_format, binary}]) of
      {ok, {{_, Code, _}, ResponseHttpHeaders, ResponseBody}} when Code >= 200, Code =< 299 ->
        {ok,
         #response{code = Code,
                   headers = ResponseHttpHeaders,
                   body = ResponseBody}};
      {ok, {{_, Code, Reason}, ResponseHttpHeaders, ResponseBody}} ->
        {error,
         Reason,
         #response{code = Code,
                   headers = ResponseHttpHeaders,
                   body = ResponseBody}};
      {error, Error} ->
        {error,
         Error,
         #response{code = 503,
                   headers = [],
                   body = []}}
    end
  catch
    _:HTTPError ->
      {error,
       HTTPError,
       #response{code = 500,
                 headers = [],
                 body = []}}
  end.

rest_targets(Target, Targets) ->
  [{Key, lists:delete(Target, Sublist)} || {Key, Sublist} <- Targets].

parse_method(Method) ->
  list_to_atom(string:to_lower(binary_to_list(Method))).

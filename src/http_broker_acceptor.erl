%% +--------------------------------------------------------------+
%% | Copyright (c) 2023, Faceplate LTD. All Rights Reserved.      |
%% | Author: Karimov Adilbek, @berikka10@gmail.com                |
%% +--------------------------------------------------------------+

-module(http_broker_acceptor).

-include("http_broker.hrl").

-behaviour(cowboy_handler).

-record(request,{ method, headers, body }).
-record(response,{ code, headers, body }).

%%=================================================================
%%	Cowboy behaviour
%%=================================================================
-export([
  init/2,
  terminate/3,
  send_to_target/2
]).

init(CowboyRequest, #{
  strategy := Strategy,
  targets := Targets,
  endpoint := Endpoint
} = Config) ->

  { Body, CowboyRequest1} = read_body( CowboyRequest ),

  Request = #request{
    method = cowboy_req:method(CowboyRequest1),
    headers = cowboy_req:headers(CowboyRequest1),
    body = Body
  },

  {Target, Response} = try_send( Targets, Request),
  #response{
    code = ResponseCode,
    headers = ResponseHeaders,
    body = ResponseBody
  } = Response,

  if
    Strategy =:= all andalso  (ResponseCode >= 200 orelse ResponseCode =< 299)  ->
      queue_request(Request, rest_targets( Target, Targets ), Endpoint ),
      esubscribe:notify({?SUBSCRIPTIONS_SCOPE, Request, Target, Targets, Endpoint}, {response_code, ResponseCode});
    true ->
      ignore
  end,

  AdaptedHeaders = lists:foldl(fun({Key, Value}, Acc) -> maps:put(Key, Value, Acc) end, #{}, ResponseHeaders),
  CowboyResponse = cowboy_req:reply(ResponseCode, AdaptedHeaders, ResponseBody, CowboyRequest1),
  {ok, CowboyResponse, Config}.


terminate(_Reason, _Req, _State) ->
  ok.

pick_target([{Key, Values} | Rest]) when Values /= [] ->
  RandomOrder = lists:nth(rand:uniform(length(Values)), Values),
  UpdatedValues = lists:delete(RandomOrder, Values),
  {RandomOrder, [{Key, UpdatedValues} | Rest]};
pick_target([{_, []} | Rest]) ->
  pick_target(Rest);
pick_target([_|Rest]) ->
  pick_target(Rest).


try_send(Targets, Request) ->
  {{URL, _Order} = Target, RestTargets} = pick_target(Targets),
  case send_to_target(list_to_binary(URL), {Request#request.method, Request#request.headers, Request#request.body}) of
    {ok, Response} ->
      {Target, Response};
    {error, Response} ->
      if
        length(element(2, hd(RestTargets))) =:= 0 ->
          {Target, Response};
        true ->
          try_send(RestTargets, Request)
      end;
    {error, _Reason, Response} ->
      {Target, Response}
  end.


read_body( Req )->
  read_body( Req, <<>> ).
read_body( Req, Body )->
  case cowboy_req:read_body( Req ) of
    {ok, Data, Req1}->
      { <<Body/binary, Data/binary>>, Req1};
    {more, Data, Req1}->
      read_body( Req1, <<Body/binary, Data/binary >> )
  end.

send_to_target(<<"http", _/binary>> = URL, {_Method, Headers, Body}) ->
  AdaptedHeaders = [ {binary_to_list(K), V} || {K, V} <- maps:to_list(Headers) ],

  Request = {URL, AdaptedHeaders, "application/json", Body},
  HTTPOptions = [],

  try
    case httpc:request(post, Request, HTTPOptions, [{body_format, binary}]) of
      {ok, {{_, Code, _}, ResponseHttpHeaders, ResponseBody}} when Code >= 200, Code =< 299 ->
        {ok, #response{code = Code, headers = ResponseHttpHeaders, body = ResponseBody}};
      {ok, {{_, Code, _Reason}, ResponseHttpHeaders, ResponseBody}} when Code == 404 ->
        {ok, #response{code = Code, headers = ResponseHttpHeaders, body = ResponseBody}};
      {error, {failed_connect, Reason}} ->
        {error, #response{code = 404, headers = [], body = []}}
    end
catch
    _:HTTPError ->
      {error, HTTPError}
  end.

queue_request(Request, Targets, Endpoint) ->
  QueueDB = persistent_term:get(db_ref),
  EndpointRef = {erlang:system_time(millisecond), make_ref()},

  zaya_rocksdb:write(QueueDB,
    [
      {?QUEUE(Endpoint, EndpointRef), Request} |
      [{?TARGET(Endpoint, Service, EndpointRef), ?STARTING_ATTEMPT_VALUE } || {_, Group} <- Targets, {Service, _} <- Group]
    ]
  ).

rest_targets(Target, Targets) ->
  lists:map(fun({Key, Sublist}) ->
    {Key, lists:delete(Target, Sublist)}
  end, Targets).
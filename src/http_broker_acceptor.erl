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

-define(DEFAULT_HTTP_TIMEOUT, 30000).

%%=================================================================
%%        Cowboy behaviour
%%=================================================================
-export([init/2, terminate/3]).

%-ifdef(TEST).
-export([try_send/2, call_one/3, send_to_target/3, extract_endpoint/1, pick_target/1]).
%-endif.


init(CowboyRequest, Config) ->
  {Body, CowboyRequest1} = read_body(CowboyRequest),

  Request =
    #request{method = parse_method(cowboy_req:method(CowboyRequest1)),
             headers = cowboy_req:headers(CowboyRequest1),
             body = Body},

  Response = try_send( Request, Config),
  #response{
    code = ResponseCode,
    headers = ResponseHeaders,
    body = ResponseBody
  } = Response,

  AdaptedHeaders =
    maps:from_list( [ {
      unicode:characters_to_binary( Header ),
      unicode:characters_to_binary( Value )
    } || {Header, Value} <- ResponseHeaders ] ),
  CowboyResponse = cowboy_req:reply(ResponseCode, AdaptedHeaders, ResponseBody, CowboyRequest1),

  {ok, CowboyResponse, Config}.

terminate(_Reason, _Req, _State) ->
  ok.


try_send( Request, #{
  strategy := call_one,
  targets := Targets
}) ->
  {_Target, Response} = call_one( Request, Targets, undefined ),

  Response;

try_send( Request, #{
  strategy := call_all,
  targets := Targets,
  endpoint := Endpoint
}) ->

  {Target, Response} = call_one( Request, Targets, Endpoint ),

  #response{ code =  ResponseCode} = Response,

  if
    (ResponseCode >= 200 orelse ResponseCode =< 299)  ->
      % If the process crashes here then the client will receive
      %   500 Server Internal Error
      % and the request should be handled as not successful
      http_broker_queue:enqueue(Request, Endpoint, rest_targets( Target, Targets ));
    true ->
      ignore
  end,

  Response;

try_send( Request, #{
  strategy := cast_one
} = Config) ->

  spawn(fun()-> try_send( Request, Config#{ strategy => call_one }) end),

  #response{
    code = 200,
    headers = [],
    body = <<>>
  };

try_send( Request, #{
  strategy := cast_all,
  targets := Targets
}) ->

  AllTargets = all_targets( Targets ),
  Send =
    fun( Target )->
      Endpoint = extract_endpoint(Target),
      case send_to_target(Target, Request, Endpoint) of
        {ok, _Response} -> ok;
        {error, Error, _Response} ->
          ?LOGWARNING("unable send request to target ~p, error ~p",[ Target, Error ])
      end
    end,
  [ Send( T ) || T <- AllTargets],

  #response{
    code = 200,
    headers = [],
    body = <<>>
  }.

call_one(Request, Targets, Endpoint)->
  {Target, RestTargets} = pick_target( Targets ),

  % if not Endpoint 
  Endpoint1 = case Endpoint of
    undefined ->
      extract_endpoint(Target);
    _ -> Endpoint
  end,

  case send_to_target(Target, Request, Endpoint1) of
    {ok, Response} ->
      {Target, Response};
    {error, Error, Response} ->
      ?LOGWARNING("unable send request to target ~p, error ~p",[ Target, Error ]),
      if
        length(RestTargets) =:= 0 ->
          {Target, Response};
        true ->
          call_one(Request, RestTargets, Endpoint)
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

send_to_target({URL, Params} = Target,
               #request{headers = Headers,
                        body = Body,
                        method = Method},
                Endpoint) ->
  AdaptedHeaders = [{binary_to_list(K), V} || {K, V} <- maps:to_list(Headers)],

  ContentType =
    case Headers of
      #{<<"content-type">> := _CT} ->
        unicode:characters_to_list(_CT);
      _ ->
        "application/json"
    end,

  Request = {URL, AdaptedHeaders, ContentType, Body},

  Timeout = maps:get(timeout, Params, ?DEFAULT_HTTP_TIMEOUT),
  HTTPOptions0 = [
    {timeout, Timeout},
    {connect_timeout, Timeout div 10}
  ],

  HTTPOptions =
    case Params of
      #{ ssl := SSL } -> [{ssl,SSL}|HTTPOptions0];
      _-> HTTPOptions0
    end,

  StartTime = erlang:system_time(millisecond),

  Result = try
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
        store_request_error(Endpoint, Target, Error),
        {error,
         Error,
         #response{code = 503,
                   headers = [],
                   body = []}}
    end
  catch
    _:HTTPError ->
      store_request_error(Endpoint, Target, HTTPError),
      {error,
       HTTPError,
       #response{code = 500,
                 headers = [],
                 body = []}}
  end,
  EndTime = erlang:system_time(millisecond),
  ExecutionTime = EndTime - StartTime,
  store_request_time(Endpoint, Target, ExecutionTime),
  Result.


-spec store_request_time(Endpoint :: term(), Target :: term(), ExecutionTime :: integer()) -> ok.
store_request_time(Endpoint, Target, ExecutionTime) ->
    Key = {request_times, Endpoint, Target},
    Times = case ets:lookup(request_times_table, Key) of
        [] -> [];
        [{_, StoredTimes}] -> StoredTimes
    end,
    NewTimes = [ExecutionTime | Times],
    UpdatedTimes = lists:sublist(NewTimes, 10),
    ets:insert(request_times_table, {Key, UpdatedTimes}),
    ok.

-spec store_request_error(Endpoint :: term(), Target :: term(), Error :: term()) -> ok.
store_request_error(Endpoint, Target, Error) ->
  analytics:update_last_error(Endpoint, Target, lager_util:localtime_ms(), Error),
  ok
  .

rest_targets(Target, Targets) ->
  [{Key, lists:delete(Target, Sublist)} || {Key, Sublist} <- Targets].

parse_method(Method) ->
  list_to_atom(string:to_lower(binary_to_list(Method))).


all_targets(Targets) ->
  lists:append( [TargetsList || {_Key, TargetsList} <- Targets]).


% Helper function to extract endpoint (should be in the main module)
extract_endpoint(Input) ->
    case Input of
        {URL, _} when is_binary(URL) ->
            Parts = binary:split(URL, <<"/">>, [global]),
            case Parts of
                [<<"http:">>, _, Host | _] ->
                    Host;
                _ ->
                    ?LOGWARNING("Unable to extract endpoint from URL: ~p~n", [URL]),
                    undefined
            end;
        URL when is_binary(URL) ->
            Parts = binary:split(URL, <<"/">>, [global]),
            case Parts of
                [<<"http:">>, _, Host | _] ->
                    Host;
                _ ->
                    ?LOGWARNING("Unable to extract endpoint from URL: ~p~n", [URL]),
                    undefined
            end;
        _ ->
            ?LOGWARNING("Unexpected input: ~p~n", [Input]),
            undefined
    end.
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
  group_endpoints/1,
  get_datetime/0,
  generate_target_records/3,
  send_request_to_target/3,
  queue_directory/0,
  get_names_and_urls/1
]).

get_http_headers(Response) ->
  HTTPHeaders = cowboy_req:headers(Response),
  lists:map(fun({Key, Value}) -> {binary_to_list(Key), binary_to_list(Value)} end, maps:to_list(HTTPHeaders)).

get_http_body(Response) ->
  {ok, HTTPBody, _} = cowboy_req:read_body(Response),
  HTTPBody.

get_endpoints() ->
  ?ENV(endpoints, #{}).

group_endpoints(Endpoints) ->
  %% Inserting default values to the endpoints
  EndpointsWithDefaults = maps:fold(fun default_order/3, [], Endpoints),
  %% Sorting and grouping endpoints
  GroupedEndpoints = maps:groups_from_list(fun get_order/1, EndpointsWithDefaults),
  maps:values(GroupedEndpoints).

default_order(Key, #{strategy := Strategy, targets := Targets}, Acc) ->
  case maps:to_list(Targets) of
    [{Target, #{order := Order}}] when is_integer(Order) ->
      [{Key, Order, Target, Strategy} | Acc];
    %% If order is undefined then it will be 0
    [{Target, _}] ->
      [{Key, 0, Target, Strategy} | Acc];
    _ -> throw(bad_arg)
  end.

get_order({_Key, Order, _Target, _Strategy}) -> Order.

get_names_and_urls(Endpoints) ->
  [{Name, URL} || {Name, #{targets := TargetMap}} <- maps:to_list(Endpoints), {URL, _} <- maps:to_list(TargetMap)].

get_datetime()-> erlang:system_time(millisecond).

generate_target_records(Targets, EndpointRef, Attempts) ->
  {lists:flatmap(
    fun(Group) ->
      lists:map(
        fun({EndpointName, _, URL, _}) ->
          { ?TARGET(EndpointName, URL, EndpointRef), Attempts }
        end, Group)
    end, Targets), Attempts}.

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

queue_directory() ->
  DirectoryName = "QUEUE",
  case filelib:is_dir(DirectoryName) of
    true ->
      zaya_rocksdb:open(#{dir => DirectoryName});
    false ->
      zaya_rocksdb:create(#{dir => DirectoryName})
  end.
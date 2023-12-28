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
  Targets   = http_broker_lib:group_endpoints(Endpoints),
  ?LOGINFO("Targets: ~p ~n", [Targets]),
  handle_sorted_info(HTTPHeaders, HTTPBody, Targets).

handle_sorted_info(HTTPHeaders, HTTPBody, Targets) ->
  lists:foreach(
    fun(Target) ->
      random_pick(HTTPHeaders, HTTPBody, Target)
    end, Targets).

random_pick(HTTPHeaders, HTTPBody, [{EndpointName, _, URL, Strategy}]) ->
  handle_strategy(EndpointName, Strategy, [URL], HTTPHeaders, HTTPBody),
  ok;
random_pick(HTTPHeaders, HTTPBody, Orders) ->
  RandomIndex = rand:uniform(length(Orders)),
  {EndpointName, _, URL, Strategy} = RandomOrder = lists:nth(RandomIndex, Orders),

  case handle_strategy(EndpointName, Strategy, [URL], HTTPHeaders, HTTPBody) of
    stop_requests ->
      ok;
    _ ->
      UpdatedOrders = lists:delete(RandomOrder, Orders),
      random_pick(HTTPHeaders, HTTPBody, UpdatedOrders)
  end.

handle_strategy(_EndpointName, one, URL, HTTPHeaders, HTTPBody) ->
  case http_broker_lib:send_request_to_target(list_to_binary(URL), HTTPHeaders, HTTPBody) of
    {ok, Response} ->
      ?LOGINFO("Successfully sent to ~p: ~p~n", [URL, Response]),
      stop_requests; % Stop making requests
    {error, Reason} ->
      ?LOGINFO("Failed to send to ~p: ~p~n", [URL, Reason]),
      continue_requests
  end;
handle_strategy(EndpointName, all, URL, HTTPHeaders, HTTPBody) ->
%%  case http_broker_lib:send_request_to_target(list_to_binary(URL), HTTPHeaders, HTTPBody) of
%%    {ok, Response} ->
%%      ?LOGINFO("Successfully sent to ~p: ~p~n", [URL, Response]),
%%      ?LOGINFO("Creating Queue..."),
      create_queue(EndpointName, HTTPHeaders, HTTPBody);
%%      ?LOGINFO("Queue Created"),
%%      continue_requests;
%%    {error, _Reason} ->
%%      {error, all_services_failed}
%%  end;
handle_strategy(_, InvalidStrategy, _, _, _) ->
  ?LOGINFO("Unsupported strategy: ~p ~n", [InvalidStrategy]),
  continue_requests.

create_queue(EndpointName, HTTPHeaders, HTTPBody) ->
  DBRef = http_broker_lib:queue_directory(),

  EndpointRef = {http_broker_lib:get_datetime(), make_ref()},
  Message = [HTTPHeaders, HTTPBody],

  % Get endpoints and group them
  Endpoints = http_broker_lib:get_endpoints(),
  NewEndpointMap = maps:remove(EndpointName, Endpoints),
  Targets = http_broker_lib:group_endpoints(NewEndpointMap),

  %% Collect data for record in queue
  QueueRecord = {?QUEUE(EndpointName, EndpointRef), Message},
  {TargetRecords, _Attempts} = http_broker_lib:generate_target_records(Targets, EndpointRef, ?STARTING_ATTEMPT_VALUE),
  Record = [QueueRecord | TargetRecords],
  ?LOGINFO("Final Record =  ~p ", [Record]),
  zaya_rocksdb:write(DBRef, Record),
  zaya_rocksdb:close(DBRef).

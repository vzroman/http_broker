-module(analytics).

-include("analytics.hrl").

-export([init/0]).
-export([find_endpoint/1, find_endpoint_target/2]).
-export([update_endpoint/2, update_endpoint_target/3, update_last_error/4]).

init() ->
	case ets:info(request_stats) of 
		undefined ->
			ets:new(request_stats, [named_table, public, set]);
		_ ->
			ok
	end.

find_endpoint(Endpoint) ->
    ets:foldl(fun({_, #system_info{endpoints = Endpoints}}, Acc) ->
                 case maps:find(Endpoint, Endpoints) of
                     {ok, EndpointData} -> [EndpointData | Acc];
                     _ -> Acc
                 end
              end,
              [],
              request_stats).

find_endpoint_target(Endpoint, Target) ->
    ets:foldl(fun({_, #system_info{endpoints = Endpoints}}, Acc) ->
                 case maps:find(Endpoint, Endpoints) of
                     {ok, EndpointData} ->
                         case maps:find(Target, EndpointData#endpoint_info.targets) of
                             {ok, TargetData} -> [{Endpoint, TargetData} | Acc];
                             _ -> Acc
                         end;
                     _ -> Acc
                 end
              end,
              [],
              request_stats).

update_endpoint(Endpoint, EndpointData) ->
    [{request_data, Data}] = ets:lookup(request_stats, request_data),

    UpdatedData =
        case maps:is_key(Endpoint, Data#system_info.endpoints) of
            true ->
                Data#system_info{endpoints =
                                     maps:update(Endpoint,
                                                 EndpointData,
                                                 Data#system_info.endpoints)};
            false ->
                Data#system_info{endpoints =
                                     maps:put(Endpoint, EndpointData, Data#system_info.endpoints),
                                 total_queue_count = Data#system_info.total_queue_count,
                                 error_queues_counts = Data#system_info.error_queues_counts}
        end,
    ets:insert(request_stats, {request_data, UpdatedData}).

update_endpoint_target(Endpoint, Target, TargetData) ->
    [{request_data, Data}] = ets:lookup(request_stats, request_data),

    UpdatedData =
        case Endpoint of
            undefined ->
                % Handle undefined endpoint (global target)
                OldTargetData = maps:get(Target, Data#system_info.global_targets, #target_info{}),
                UpdatedGlobalTargets =
                    maps:put(Target, maps:merge(TargetData, OldTargetData), Data#system_info.global_targets),
                Data#system_info{global_targets = UpdatedGlobalTargets,
                                 error_queues_counts = Data#system_info.error_queues_counts + 1,
                                 total_queue_count = Data#system_info.total_queue_count + 1};
            _ ->
                % Found endpoint
                case maps:find(Endpoint, Data#system_info.endpoints) of
                    {ok, EndpointInfo} ->
                        UpdatedEndpointInfo =
                            EndpointInfo#endpoint_info{targets =
                                                           maps:put(Target,
                                                                    TargetData,
                                                                    EndpointInfo#endpoint_info.targets)},
                        UpdatedSystemInfo =
                            Data#system_info{endpoints =
                                                 maps:update(Endpoint,
                                                             UpdatedEndpointInfo,
                                                             Data#system_info.endpoints)},
                        UpdatedSystemInfo;
                    error ->
                        % If the endpoint doesn't exist, create it with the new target
                        NewEndpointInfo =
                            #endpoint_info{total_queue_count = TargetData#target_info.queue_count,
                                           targets = #{Target => TargetData}},
                        Data#system_info{endpoints =
                                             maps:put(Endpoint,
                                                      NewEndpointInfo,
                                                      Data#system_info.endpoints),
                                         total_queue_count = Data#system_info.total_queue_count,
                                         error_queues_counts = Data#system_info.error_queues_counts}
                end
        end,
    ets:insert(request_stats, {request_data, UpdatedData}).

update_last_error(Endpoint, Target, NewErrorTime, NewErrorText) ->
    [{request_data, Data}] = ets:lookup(request_stats, request_data),

    UpdatedData =
        case maps:find(Endpoint, Data#system_info.endpoints) of
            {ok, EndpointData} ->
                case maps:find(Target, EndpointData#endpoint_info.targets) of
                    {ok, TargetInfo} ->
                        UpdatedTargetInfo =
                            TargetInfo#target_info{last_error =
                                                       #error_info{time = NewErrorTime,
                                                                   text = NewErrorText}},
						UpdatedTargets = maps:update(Target, UpdatedTargetInfo, EndpointData#endpoint_info.targets),
                        UpdatedEndpointData = EndpointData#endpoint_info{targets = UpdatedTargets},
                        UpdatedEndpoints = maps:update(Endpoint, UpdatedEndpointData, Data#system_info.endpoints),
                        Data#system_info{endpoints = UpdatedEndpoints};
                    error ->
                        Data
                end;
            error ->
                Data
        end,

    ets:insert(request_stats, {request_data, UpdatedData}).

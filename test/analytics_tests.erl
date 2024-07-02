-module(analytics_tests).
-include_lib("eunit/include/eunit.hrl").
-include("analytics.hrl").

%% Setup and teardown
setup() ->
    analytics:init().

teardown(_) ->
    ets:delete(request_stats).

analytics_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
      ?_test(test_find_endpoint()),
      ?_test(test_find_endpoint_target()),
      ?_test(test_update_endpoint()),
      ?_test(test_update_endpoint_target()),
      ?_test(test_update_last_error())
     ]}.

test_find_endpoint() ->
    Endpoint = <<"test_endpoint">>,
    EndpointInfo = #endpoint_info{total_queue_count = 1, targets = #{}},
    ets:insert(request_stats, {request_data, #system_info{endpoints = #{Endpoint => EndpointInfo}}}),
    
    Result = analytics:find_endpoint(Endpoint),
    ?assertEqual([EndpointInfo], Result).

test_find_endpoint_target() ->
    Endpoint = <<"test_endpoint">>,
    Target = <<"test_target">>,
    TargetInfo = #target_info{queue_count = 1},
    EndpointInfo = #endpoint_info{total_queue_count = 1, targets = #{Target => TargetInfo}},
    ets:insert(request_stats, {request_data, #system_info{endpoints = #{Endpoint => EndpointInfo}}}),
    
    Result = analytics:find_endpoint_target(Endpoint, Target),
    ?assertEqual([{Endpoint, TargetInfo}], Result).

test_update_endpoint() ->
    Endpoint = <<"test_endpoint">>,
    EndpointInfo = #endpoint_info{total_queue_count = 1, targets = #{}},
    analytics:update_endpoint(Endpoint, EndpointInfo),
    
    [{request_data, Data}] = ets:lookup(request_stats, request_data),
    ?assertEqual(EndpointInfo, maps:get(Endpoint, Data#system_info.endpoints)).

test_update_endpoint_target() ->
    Endpoint = <<"test_endpoint">>,
    Target = <<"test_target">>,
    TargetInfo = #target_info{queue_count = 1},
    analytics:update_endpoint_target(Endpoint, Target, TargetInfo),
    
    [{request_data, Data}] = ets:lookup(request_stats, request_data),
    EndpointInfo = maps:get(Endpoint, Data#system_info.endpoints),
    ?assertEqual(TargetInfo, maps:get(Target, EndpointInfo#endpoint_info.targets)).

test_update_last_error() ->
    Endpoint = <<"test_endpoint">>,
    Target = <<"test_target">>,
    ErrorTime = erlang:system_time(second),
    ErrorText = <<"Test error">>,
    
    % First, insert some initial data
    InitialTargetInfo = #target_info{queue_count = 1, last_error = #error_info{}},
    InitialEndpointInfo = #endpoint_info{total_queue_count = 1, targets = #{Target => InitialTargetInfo}},
    ets:insert(request_stats, {request_data, #system_info{endpoints = #{Endpoint => InitialEndpointInfo}}}),
    
    % Now update the last error
    analytics:update_last_error(Endpoint, Target, ErrorTime, ErrorText),
    
    % Check the result
    [{request_data, UpdatedData}] = ets:lookup(request_stats, request_data),
    UpdatedEndpointInfo = maps:get(Endpoint, UpdatedData#system_info.endpoints),
    UpdatedTargetInfo = maps:get(Target, UpdatedEndpointInfo#endpoint_info.targets),
    ?assertEqual(#error_info{time = ErrorTime, text = ErrorText}, UpdatedTargetInfo#target_info.last_error).

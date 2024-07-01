-module(broker_analytics_tests).
-include_lib("eunit/include/eunit.hrl").
-include("http_broker.hrl").
-include("analytics.hrl").

% Mock the zaya_rocksdb module
-define(MOCK_DB, mock_db).

setup() ->
    meck:new(zaya_rocksdb, [non_strict]),
    meck:new(persistent_term, [non_strict]),
    meck:new(analytics, [non_strict]),
    meck:expect(persistent_term, get, fun(?DB_REF) -> ?MOCK_DB end),
    meck:expect(analytics, update_endpoint, fun(_, _) -> ok end).

cleanup(_) ->
    meck:unload(zaya_rocksdb),
    meck:unload(persistent_term),
    meck:unload(analytics).

broker_analytics_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"test get_endpoints", fun test_get_endpoints/0},
      {"test find_all_error_queue_count", fun test_find_all_error_queue_count/0},
      {"test get_queue_count_for", fun test_get_queue_count_for/0},
      {"test count_targets", fun test_count_targets/0},
      {"test count_queue_items", fun test_count_queue_items/0}
     ]}.

test_get_endpoints() ->
    MockData = [
        {{target, <<"endpoint1">>, <<"service1">>, <<"ref1">>}, <<"value1">>},
        {{target, <<"endpoint2">>, <<"service2">>, <<"ref2">>}, <<"value2">>},
        {{target, <<"endpoint1">>, <<"service3">>, <<"ref3">>}, <<"value3">>}
    ],
    meck:expect(zaya_rocksdb, foldl, 
        fun(?MOCK_DB, _, Fun, InitAcc) ->
            lists:foldl(fun({Key, Value}, Acc) -> Fun({Key, Value}, Acc) end, InitAcc, MockData)
        end),
    
    Result = broker_analytics:get_endpoints(?MOCK_DB),
    ?assertEqual([<<"endpoint1">>, <<"endpoint2">>], Result).

test_find_all_error_queue_count() ->
    MockData = [
        {{target, <<"endpoint1">>, <<"service1">>, <<"ref1">>}, <<"value1">>},
        {{target, <<"endpoint2">>, <<"service2">>, <<"ref2">>}, <<"value2">>},
        {{target, <<"endpoint1">>, <<"service3">>, <<"ref3">>}, <<"value3">>}
    ],
    meck:expect(zaya_rocksdb, foldl, 
        fun(?MOCK_DB, _, Fun, InitAcc) ->
            lists:foldl(fun({Key, Value}, Acc) -> Fun({Key, Value}, Acc) end, InitAcc, MockData)
        end),
    meck:expect(zaya_rocksdb, read, fun(?MOCK_DB, [_Key]) -> [{key, term_to_binary({value, 2})}] end),
    
    Result = broker_analytics:find_all_error_queue_count(?MOCK_DB),
    ?assertEqual(3, Result).

test_get_queue_count_for() ->
    MockData = [
        {{target, <<"endpoint1">>, <<"service1">>, <<"ref1">>}, <<"value1">>},
        {{target, <<"endpoint2">>, <<"service2">>, <<"ref2">>}, <<"value2">>},
        {{target, <<"endpoint1">>, <<"service3">>, <<"ref3">>}, <<"value3">>}
    ],
    meck:expect(zaya_rocksdb, foldl, 
        fun(?MOCK_DB, _, Fun, InitAcc) ->
            lists:foldl(fun({Key, Value}, Acc) -> Fun({Key, Value}, Acc) end, InitAcc, MockData)
        end),
    
    Result = broker_analytics:get_queue_count_for(<<"endpoint1">>, ?MOCK_DB),
    ?assertEqual(2, Result).

test_count_targets() ->
    MockData = [
        {{target, <<"endpoint1">>, <<"service1">>, <<"ref1">>}, <<"value1">>},
        {{target, <<"endpoint2">>, <<"service2">>, <<"ref2">>}, <<"value2">>},
        {{target, <<"endpoint1">>, <<"service1">>, <<"ref3">>}, <<"value3">>}
    ],
    meck:expect(zaya_rocksdb, foldl, 
        fun(?MOCK_DB, _, Fun, InitAcc) ->
            lists:foldl(fun({Key, Value}, Acc) -> Fun({Key, Value}, Acc) end, InitAcc, MockData)
        end),
    
    Result = broker_analytics:count_targets(?MOCK_DB),
    ExpectedResult = #{{<<"endpoint1">>, <<"service1">>} => 1,
                       {<<"endpoint2">>, <<"service2">>} => 1},
    ?assertEqual(ExpectedResult, Result).

test_count_queue_items() ->
    MockData = [
        {{target, <<"endpoint1">>, <<"service1">>, <<"ref1">>}, <<"value1">>},
        {{target, <<"endpoint2">>, <<"service2">>, <<"ref2">>}, <<"value2">>},
        {{target, <<"endpoint1">>, <<"service3">>, <<"ref3">>}, <<"value3">>}
    ],
    meck:expect(zaya_rocksdb, foldl, 
        fun(?MOCK_DB, _, Fun, InitAcc) ->
            lists:foldl(fun({Key, Value}, Acc) -> Fun({Key, Value}, Acc) end, InitAcc, MockData)
        end),
    
    Result = broker_analytics:count_queue_items(?MOCK_DB),
    ?assertEqual(3, Result).
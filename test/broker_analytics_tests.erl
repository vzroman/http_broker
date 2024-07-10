-module(broker_analytics_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/http_broker.hrl").
-include("../include/analytics.hrl").

broker_analytics_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
         ?_test(test_collect_data()),
         ?_test(test_convert_to_json()),
         ?_test(test_log_data()),
         ?_test(test_count_queues()),
         ?_test(test_get_attempts_count()),
         ?_test(test_get_average_10request_duration())
     ]
    }.

setup() ->
    meck:new(zaya_rocksdb, [passthrough]),

	meck:expect(zaya_rocksdb, foldl, 
		fun(_, _, Fun, Acc) -> 
			Acc1 = Fun({{target, <<"endpoint1">>, <<"service1">>, <<"ref1">>}, <<"value1">>}, Acc),
			Acc2 = Fun({{target, <<"endpoint1">>, <<"service2">>, <<"ref2">>}, <<"value2">>}, Acc1),
			Acc2
		end),

    meck:expect(zaya_rocksdb, read, 
        fun(_, [_Key]) -> 
            [{some_key, term_to_binary({some_value, 1})}]
        end),
    
    DB = #{},
    persistent_term:put(?DB_REF, DB),
    persistent_term:put({http_broker,'@db_ref@'}, DB),
    
	create_request_times_table(),
    ets:insert(request_times_table, {{request_times, <<"endpoint1">>, <<"service1">>}, [100, 200, 300]}),
    
	case application:start(http_broker) of
        ok -> ok;
        {error, Reason} -> io:format("Failed to start application: ~p~n", [Reason])
    end,
    ok.

create_request_times_table() ->
    io:format("Checking if ETS table 'request_times_table' exists...~n"),
    case ets:info(request_times_table) of
        undefined ->
            io:format("ETS table 'request_times_table' does not exist. Creating...~n"),
            ets:new(request_times_table, [named_table, public, set]);
        Info ->
            io:format("ETS table 'request_times_table' already exists: ~p~n", [Info]),
            ok
    end.

teardown(_) ->
	try
		meck:unload(zaya_rocksdb),

		persistent_term:erase(?DB_REF),
		ets:delete(request_times_table),

		case whereis(http_broker_queue_cleanup_service) of
			undefined -> ok; % Service not running
			Pid -> gen_server:stop(Pid)
		end,

		case application:which_applications() of
			[{http_broker, _}] -> application:stop(http_broker);
			_ -> ok % http_broker not running
		end,
        persistent_term:erase(?DB_REF),
        persistent_term:erase({http_broker,'@db_ref@'})
	catch
		error:Reason -> io:format("Error during teardown: ~p~n", [Reason])
	end,
	ok.

test_collect_data() ->
    SystemInfo = broker_analytics:collect_data(),
    io:format("SysInfo:~p~n",[SystemInfo]),
    ?assertMatch(#system_info{}, SystemInfo),
    ?assertEqual(2, SystemInfo#system_info.total_queue_count),
    ?assertEqual(2, SystemInfo#system_info.error_queues_counts),
    ?assert(is_map(SystemInfo#system_info.endpoints)),
    ?assert(maps:is_key(<<"endpoint1">>, SystemInfo#system_info.endpoints)).


test_convert_to_json() ->
    SystemInfo = #system_info{
        total_queue_count = 123,
        error_queues_counts = 3,
        endpoints = #{
            <<"/endpoint1">> => #endpoint_info{
                total_queue_count = 123,
                error_queue_counts = 12,
                targets = #{
                    <<"http://target1.com">> => #target_info{
                        queue_count = 123,
                        state = ok,
                        attempts = 0,
                        average_10request_duration = 500,
                        last_error = #error_info{
                            time = "2024-06-06T09:23:17:345",
                            text = "some error"
                        }
                    },
                    <<"http://target2.com">> => #target_info{
                        queue_count = 123,
                        state = error,
                        attempts = 12,
                        average_10request_duration = 500,
                        last_error = #error_info{
                            time = "2024-06-06T09:23:17:345",
                            text = "some error"
                        }
                    }
                }
            },
            <<"/endpoint2">> => #endpoint_info{
                total_queue_count = 0,
                error_queue_counts = 0,
                targets = #{
                    <<"http://target1.com">> => #target_info{
                        queue_count = 0,
                        state = ok,
                        attempts = 0,
                        average_10request_duration = 500,
                        last_error = #error_info{
                            time = "2024-06-06T09:23:17:345",
                            text = "some error"
                        }
                    },
                    <<"http://target2.com">> => #target_info{
                        queue_count = 0,
                        state = error,
                        attempts = 0,
                        average_10request_duration = 500,
                        last_error = #error_info{
                            time = "2024-06-06T09:23:17:345",
                            text = "some error"
                        }
                    }
                }
            }
        }
    },
    JsonBinary = broker_analytics:convert_to_json(SystemInfo),
    DecodedJson = jsx:decode(JsonBinary, [return_maps]),
    ExpectedJson = #{
        <<"total_queue_count">> => 123,
        <<"error_queues_counts">> => 3,
        <<"endpoints">> => #{
            <<"/endpoint1">> => #{
                <<"total_queue_count">> => 123,
                <<"error_queue_counts">> => 12,
                <<"targets">> => #{
                    <<"http://target1.com">> => #{
                        <<"queue_count">> => 123,
                        <<"state">> => <<"ok">>,
                        <<"attempts">> => 0,
                        <<"average_10request_duration">> => 500,
                        <<"last_error">> => #{
                            <<"time">> => <<"2024-06-06T09:23:17:345">>,
                            <<"text">> => <<"some error">>
                        }
                    },
                    <<"http://target2.com">> => #{
                        <<"queue_count">> => 123,
                        <<"state">> => <<"error">>,
                        <<"attempts">> => 12,
                        <<"average_10request_duration">> => 500,
                        <<"last_error">> => #{
                            <<"time">> => <<"2024-06-06T09:23:17:345">>,
                            <<"text">> => <<"some error">>
                        }
                    }
                }
            },
            <<"/endpoint2">> => #{
                <<"total_queue_count">> => 0,
                <<"error_queue_counts">> => 0,
                <<"targets">> => #{
                    <<"http://target1.com">> => #{
                        <<"queue_count">> => 0,
                        <<"state">> => <<"ok">>,
                        <<"attempts">> => 0,
                        <<"average_10request_duration">> => 500,
                        <<"last_error">> => #{
                            <<"time">> => <<"2024-06-06T09:23:17:345">>,
                            <<"text">> => <<"some error">>
                        }
                    },
                    <<"http://target2.com">> => #{
                        <<"queue_count">> => 0,
                        <<"state">> => <<"error">>,
                        <<"attempts">> => 0,
                        <<"average_10request_duration">> => 500,
                        <<"last_error">> => #{
                            <<"time">> => <<"2024-06-06T09:23:17:345">>,
                            <<"text">> => <<"some error">>
                        }
                    }
                }
            }
        }
    },
    ?assertEqual(ExpectedJson, DecodedJson).

test_log_data() ->
    JsonData = <<"{\"test\":\"data\"}">>,
    broker_analytics:log_data(JsonData),
    {ok, JsonFromFile} = file:read_file("queue_stats.json"),
    ?assertEqual(JsonData, JsonFromFile).

test_count_queues() ->
    QueueDB = persistent_term:get(?DB_REF),
    QueueCount = broker_analytics:count_queues(QueueDB),
    ?assertEqual(2, QueueCount).

test_get_attempts_count() ->
    QueueDB = persistent_term:get(?DB_REF),
    AttemptsCount = broker_analytics:get_attempts_count(QueueDB, some_key),
    ?assertEqual(1, AttemptsCount).

test_get_average_10request_duration() ->
    Duration = broker_analytics:get_average_10request_duration(<<"endpoint1">>, <<"service1">>),

    ?assertEqual(200.0, Duration).
-module(http_broker_acceptor_tests).
-define(TEST, true).
-include_lib("eunit/include/eunit.hrl").
-include("../include/http_broker.hrl").
-include("../include/analytics.hrl").

-record(request, {method, headers, body}).
-record(response, {code, headers, body}).

% Mock the necessary modules and functions
setup() ->
    meck:new(http_broker_acceptor, [passthrough]),
    meck:new(httpc, [non_strict]),
    meck:new(analytics, [non_strict]),
    
    meck:expect(httpc, request, 
                fun(_, _, _, _) -> 
                    {ok, {{protocol, 200, reason}, [{"Content-Type", "text/plain"}], <<"Success">>}}
                end),
    
    meck:expect(analytics, update_last_error, fun(_, _, _, _) -> ok end),

    meck:new(http_broker_queue, [non_strict]),

	meck:expect(http_broker_acceptor, call_one, 
        fun(_, Targets, _) ->
            [{_, [{Target, _} | _]} | _] = Targets,
            {Target, #response{code = 200, headers = [], body = <<"Success">>}}
        end),

    meck:expect(http_broker_acceptor, send_to_target, fun(_, _, _) -> 
        {ok, #response{code = 200, headers = [], body = <<"Success">>}}
    end),

	meck:expect(http_broker_acceptor, extract_endpoint, 
    fun(Input) ->
        case Input of
            {URL, _} when is_binary(URL) ->
                [_, Endpoint | _] = binary:split(URL, <<"/">>, [global]),
                Endpoint;
            URL when is_binary(URL) ->
                [_, Endpoint | _] = binary:split(URL, <<"/">>, [global]),
                Endpoint;
            _ -> undefined
        end
    end),

	meck:expect(http_broker_acceptor, pick_target,
        fun([{_, [Target | _]} | _]) -> {Target, []} end),

    meck:expect(http_broker_queue, enqueue, fun(_, _, _) -> ok end).


teardown(_) ->
	meck:unload(http_broker_acceptor),
	meck:unload(http_broker_queue),
	meck:unload(httpc),
	meck:unload(analytics).

	http_broker_acceptor_test_() ->
		{setup,
		 fun setup/0,
		 fun teardown/1,
		 [
		  fun test_call_one_strategy/0,
		  fun test_call_all_strategy/0,
		  fun test_cast_one_strategy/0,
		  fun test_cast_all_strategy/0
		 ]
		}.

create_request() ->
    #request{method = get, headers = #{}, body = <<>>}.

create_targets() ->
    [{1, [{<<"http://example.com/api1">>, #{}}, {<<"http://example.com/api2">>, #{}}]}].

create_config(call_all, Targets) ->
	#{strategy => call_all, targets => Targets, endpoint => <<"test_endpoint">>};
create_config(Strategy, Targets) ->
	#{strategy => Strategy, targets => Targets}.

assert_success_response(Result) ->
    ?assertMatch(#response{code = 200, body = <<"Success">>}, Result).	

test_call_one_strategy() ->
    Request = create_request(),
    Targets = create_targets(),
    Config = create_config(call_one, Targets),
    
    Result = http_broker_acceptor:try_send(Request, Config),
    assert_success_response(Result).

test_call_all_strategy() ->
    Request = create_request(),
    Targets = create_targets(),
    Config = create_config(call_all, Targets),
    
    Result = http_broker_acceptor:try_send(Request, Config),
    assert_success_response(Result),
    ?assert(meck:called(http_broker_queue, enqueue, '_')).

test_cast_one_strategy() ->
    Request = create_request(),
    Targets = create_targets(),
    Config = create_config(cast_one, Targets),
    
    Result = http_broker_acceptor:try_send(Request, Config),
    ?assertMatch(#response{code = 200, body = <<>>}, Result).

test_cast_all_strategy() ->
    Request = create_request(),
    Targets = create_targets(),
    Config = create_config(cast_all, Targets),
    
    Result = http_broker_acceptor:try_send(Request, Config),
    ?assertMatch(#response{code = 200, body = <<>>}, Result).
-module(analytics_acceptor).
-behaviour(cowboy_handler).
-export([start/0, init/2, terminate/3, handle_request/2]).

start() ->
    % Start the Cowboy listener for the API
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/queue_stats", ?MODULE, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(api_listener, [{port, 8081}], #{env => #{dispatch => Dispatch}}).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

terminate(_Reason, _Req, _Opts) ->
    ok.

handle_request(Req, State) ->
	handle_queue_stats(Req, State),
    {ok, Req, State}.

handle_queue_stats(CowboyRequest, State) ->
  SystemInfo = broker_analytics:collect_data(),
  JsonData = broker_analytics:convert_to_json(SystemInfo),
  broker_analytics:log_data(JsonData),
  Headers = #{<<"content-type">> => <<"application/json">>},
  CowboyResponse = cowboy_req:reply(200, Headers, JsonData, CowboyRequest),
  {ok, CowboyResponse, State}.

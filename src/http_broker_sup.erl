-module(http_broker_sup).

-behaviour(supervisor).

-include("http_broker.hrl").

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => ?DEFAULT_MAX_RESTARTS,
                 period => ?DEFAULT_MAX_PERIOD},

    ChildSpecs = [listener(http, 7000)],

    io:format("~nLISTENER: ~p",[ChildSpecs]),
    read_endpoints(),

    {ok, {SupFlags, ChildSpecs}}.

read_endpoints() ->
  Endpoints = ?ENV(http_broker, [{endpoints, []}]),
  io:format("~nEndpoints: ~p~n", [Endpoints]).

dispatch_rules() ->
  Routes = [
    {'_', [
      {"/service1/some_path", http_redirect, []}
    ]}
  ],
  cowboy_router:compile(Routes).

listener(http, Port) when is_integer(Port) ->
  #{
    id => http_broker,
    start => { cowboy, start_clear, [
      http_broker,
      [{port, Port}],
      #{env => #{dispatch => dispatch_rules()} }
    ]},
    restart => permanent,
    shutdown => ?ENV(stop_timeout, ?DEFAULT_STOP_TIMEOUT),
    type => worker,
    modules => [cowboy]
  };

listener(_, _) ->
  io:format("~nListener is not correct", []),
  undefined.

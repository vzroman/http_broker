%% +--------------------------------------------------------------+
%% | Copyright (c) 2023, Faceplate LTD. All Rights Reserved.      |
%% | Author: Karimov Adilbek, @berikka10@gmail.com                |
%% +--------------------------------------------------------------+

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

    ChildSpecs = [listener(http, ?ENV(port, #{}))],

    io:format("~nListener: ~p",[ChildSpecs]),

    io:format("~nValues: ~p~n",[maps:values(http_broker_lib:get_endpoints())]),

    {ok, {SupFlags, ChildSpecs}}.

dispatch_rules() ->
  Endpoints = http_broker_lib:get_endpoints(),
  DispatchRules = [{"/" ++ Key, http_broker_acceptor, [Value]}
    || {Key, Value} <- maps:to_list(Endpoints)],

  cowboy_router:compile([{'_', DispatchRules}]).


listener(http, Port) when is_integer(Port) ->
  #{
    id => http_broker,
    start => { cowboy, start_clear, [
      http_broker,
      [{port, Port}],
      #{env => #{dispatch=> dispatch_rules()} }
    ]},
    restart => permanent,
    shutdown => ?ENV(stop_timeout, ?DEFAULT_STOP_TIMEOUT),
    type => worker,
    modules => [cowboy]
  };
listener(_, _) ->
  io:format("~nListener is not correct", []),
  undefined.


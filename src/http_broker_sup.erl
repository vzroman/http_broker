%% +--------------------------------------------------------------+
%% | Copyright (c) 2023, Faceplate LTD. All Rights Reserved.      |
%% | Author: Karimov Adilbek, @berikka10@gmail.com                |
%% +--------------------------------------------------------------+

-module(http_broker_sup).

-behaviour(supervisor).

-include("http_broker.hrl").

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->

 httpc:set_options( ?ENV( http_client, ?DEFAULT_HTTP_CLIENT_OPTIONS ) ),

 http_broker_queue:on_init(),

  SubscriptionsServer = #{
    id=>esubscribe,
    start=>{esubscribe,start_link,[ ?SUBSCRIPTIONS_SCOPE ]},
    restart=>permanent,
    shutdown=> ?DEFAULT_STOP_TIMEOUT,
    type=>worker,
    modules=>[esubscribe]
  },

  %------------------Listener-----------------------------
  Port = ?ENV(port, ?DEFAULT_LISTEN_PORT),
  SSL = ?ENV(ssl, []),
  ListenerType =
    if
      SSL =:= [] -> start_clear;
      true -> start_tls
    end,

  Endpoints = ?ENV(endpoints, #{}),

  Listener =
    #{
      id=>http_listener,
      start=>{ cowboy ,ListenerType ,[
        http_listener,
        [ {port, Port} | SSL],
        #{env => #{dispatch=> dispatch_rules( Endpoints ) } }
      ]},
      restart=>permanent,
      shutdown=>?ENV(stop_timeout, ?DEFAULT_STOP_TIMEOUT),
      type=>worker,
      modules=>[cowboy]
    },

  %-----------------Queue services----------------------------
  StrategyCallAllEndpoints = [{
    Endpoint,
    maps:get( attempts, Params, infinity ),
    maps:to_list( Targets )
  } || { Endpoint, #{ strategy := call_all, targets := Targets } = Params } <- maps:to_list( Endpoints ) ],

  QueueServices =
    [
      #{
        id => list_to_atom( Endpoint ++"->" ++ Service ),
        start => {http_broker_queue_service, start_link, [ Endpoint, Target, Attempts ]},
        restart => permanent,
        shutdown => ?DEFAULT_STOP_TIMEOUT,
        type => worker,
        modules => [http_broker_queue_service]
      } ||
        { Endpoint, Attempts, Targets } <- StrategyCallAllEndpoints,
        { Service, _Config } = Target <-  Targets
    ],

  MaxAgeService = #{
    id => http_broker_max_age_service,
    start => {http_broker_max_age_service, start_link, []},
    restart => permanent,
    shutdown => ?DEFAULT_STOP_TIMEOUT,
    type => worker,
    modules => [http_broker_max_age_service]
  },
  CleanupService = #{
    id => http_broker_queue_cleanup_service,
    start => {http_broker_queue_cleanup_service, start_link, []},
    restart => permanent,
    shutdown => ?DEFAULT_STOP_TIMEOUT,
    type => worker,
    modules => [http_broker_queue_cleanup_service]
  },

  SupFlags = #{
    strategy => one_for_one,
    intensity => ?DEFAULT_MAX_RESTARTS,
    period => ?DEFAULT_MAX_PERIOD
  },

  {ok, {SupFlags, [
    SubscriptionsServer,
    Listener,
    MaxAgeService,
    CleanupService
    | QueueServices
  ]}}.

dispatch_rules( Endpoints ) ->
  DispatchRules =
    [{Endpoint, http_broker_acceptor, Config#{ targets => targets_by_order( Targets ), endpoint => Endpoint } }
    || {Endpoint, #{ targets := Targets } = Config} <- maps:to_list(Endpoints)],
  cowboy_router:compile([{'_', DispatchRules}]).

targets_by_order( Targets )->
  GroupsByOrder =
    maps:groups_from_list(
      fun({ _Target, Config })->
        maps:get( order, Config, 0 )
      end , maps:to_list(Targets)
    ),
  lists:sort( maps:to_list( GroupsByOrder ) ).
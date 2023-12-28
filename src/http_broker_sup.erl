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
%%  QueueServices =
%%    lists:append([
%%      [ #{
%%        id => list_to_atom( Endpoint ++"->" ++ Target ),
%%        start => {http_broker_queue_service, start_link, [ Endpoint, Target ]},
%%        restart => permanent,
%%        shutdown => ?DEFAULT_STOP_TIMEOUT,
%%        type => worker,
%%        modules => [http_broker_queue_service]
%%      } || Target <- maps:keys( Targets )
%%      ]
%%      || {Endpoint, #{targets:=Targets}} <- maps:to_list( Endpoints )
%%    ]),


%%  MaxAgeService = #{
%%    id => http_broker_max_age_service,
%%    start => {http_broker_max_age_service, start_link, []},
%%    restart => permanent,
%%    shutdown => ?DEFAULT_STOP_TIMEOUT,
%%    type => worker,
%%    modules => [http_broker_max_age_service]
%%  },
%%  CleanupService = #{
%%    id => http_broker_queue_cleanup_service,
%%    start => {http_broker_queue_cleanup_service, start_link, []},
%%    restart => permanent,
%%    shutdown => ?DEFAULT_STOP_TIMEOUT,
%%    type => worker,
%%    modules => [http_broker_queue_cleanup_service]
%%  },

  SupFlags = #{
    strategy => one_for_one,
    intensity => ?DEFAULT_MAX_RESTARTS,
    period => ?DEFAULT_MAX_PERIOD
  },

  {ok, {SupFlags, [
    Listener
%%    MaxAgeService,
%%    CleanupService
%%    | QueueServices
%%    SubscriptionsServer
  ]}}.

dispatch_rules( Endpoints ) ->

  DispatchRules =
    [{Endpoint, http_broker_acceptor, Config#{ targets => targets_by_order( Targets ) } }
    || {Endpoint, #{ targets := Targets } = Config} <- maps:to_list(Endpoints)],

  cowboy_router:compile([{'_', DispatchRules}]).

targets_by_order( Targets )->

  GroupsByOrder =
    maps:groups_from_list(fun({ _Target, Config })->maps:get( order, Config, 0 )  end , maps:to_list(Targets) ),

  lists:sort( maps:to_list( GroupsByOrder ) ).
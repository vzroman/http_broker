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

 http_broker_queue:on_init(),

  %------------------Listener-----------------------------
  Port = ?ENV(port, ?DEFAULT_LISTEN_PORT),
  CorrectedPort = http_broker_queue:check_settings(Port, ?MIN_LISTEN_PORT, ?MAX_LISTEN_PORT),
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
        [ {port, CorrectedPort} | SSL],
        #{env => #{dispatch=> dispatch_rules( Endpoints ) } }
      ]},
      restart=>permanent,
      shutdown=>?ENV(stop_timeout, ?DEFAULT_STOP_TIMEOUT),
      type=>worker,
      modules=>[cowboy]
    },

  %-----------------Queue services----------------------------
  QueueServices =
    [
      #{
        id => list_to_atom( Endpoint ++"->" ++ Service ),
        start => {http_broker_queue_service, start_link, [ Endpoint, Target ]},
        restart => permanent,
        shutdown => ?DEFAULT_STOP_TIMEOUT,
        type => worker,
        modules => [http_broker_queue_service]
      }
      || {Endpoint, #{targets:=Targets}} <- maps:to_list( Endpoints ), {Service, _Config} = Target <- maps:to_list( Targets )
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
  SubscriptionsServer = #{
    id=>esubscribe,
    start=>{esubscribe,start_link,[ ?SUBSCRIPTIONS_SCOPE ]},
    restart=>permanent,
    shutdown=> ?DEFAULT_STOP_TIMEOUT,
    type=>worker,
    modules=>[esubscribe]
  },

  SupFlags = #{
    strategy => one_for_one,
    intensity => ?DEFAULT_MAX_RESTARTS,
    period => ?DEFAULT_MAX_PERIOD
  },

  {ok, {SupFlags, [
    Listener,
    MaxAgeService,
    CleanupService,
    SubscriptionsServer
    | QueueServices
  ]}}.

dispatch_rules( Endpoints ) ->
  DispatchRules =
    [{Endpoint, http_broker_acceptor, Config#{ targets => targets_by_order( Targets ), endpoint => Endpoint } }
    || {Endpoint, #{ targets := Targets } = Config} <- maps:to_list(Endpoints), is_valid_endpoint_name(Endpoint)],
  cowboy_router:compile([{'_', DispatchRules}]).

targets_by_order( Targets )->
  GroupsByOrder =
    maps:groups_from_list(
      fun({ Target, Config })->
        is_valid_target_name(Target),
        http_broker_queue:check_settings(maps:get( order, Config, ?DEFAULT_ORDER ), ?MIN_ORDER)
      end , maps:to_list(Targets)
    ),
  lists:sort( maps:to_list( GroupsByOrder ) ).

is_valid_target_name(Target) ->
  case re:run(Target, "^(https?://[a-zA-Z0-9.-]+(:[0-9]+)?(/.*)?)|(?:[0-9]{1,3}\\.){3}[0-9]{1,3}$") of
    {match, _} -> true;
    nomatch ->
      ?LOGINFO("Invalid target name: ~p", [Target]),
      exit(invalid_url)
  end.

is_valid_endpoint_name(Endpoint) ->
  case re:run(Endpoint, "^/[a-zA-Z0-9_]+$") of
    {match, _} -> true;
    nomatch ->
      ?LOGINFO("Invalid endpoint name: ~p", [Endpoint]),
      exit(invalid_url)
  end.
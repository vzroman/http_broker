%%%-------------------------------------------------------------------
%% @doc http_broker public API
%% @end
%%%-------------------------------------------------------------------

-module(http_broker).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    http_broker_sup:start_link().

stop(_State) ->
    ok.
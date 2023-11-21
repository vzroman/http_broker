%% +--------------------------------------------------------------+
%% | Copyright (c) 2023, Faceplate LTD. All Rights Reserved.      |
%% | Author: Karimov Adilbek, @berikka10@gmail.com                |
%% +--------------------------------------------------------------+

-module(http_broker_lib).

-include("http_broker.hrl").

%% API
-export([get_targets/0, get_strategy/1]).

get_targets() ->
  Maps = maps:values(?ENV(endpoints, #{})),
  lists:flatmap(fun(#{ targets := TargetMap })
    -> maps:keys(TargetMap);
    (_) -> []
                end, Maps).


get_strategy(Endpoint) ->
  case maps:find(Endpoint, ?ENV(endpoints, #{})) of
    {ok, #{strategy := Strategy}} ->
      Strategy;
    _ ->
      undefined
  end.
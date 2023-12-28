%% +--------------------------------------------------------------+
%% | Copyright (c) 2023, Faceplate LTD. All Rights Reserved.      |
%% | Author: Karimov Adilbek, @berikka10@gmail.com                |
%% +--------------------------------------------------------------+

-module(http_broker).


-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    http_broker_sup:start_link().

stop(_State) ->
    ok.
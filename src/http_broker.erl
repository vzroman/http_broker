%% +--------------------------------------------------------------+
%% | Copyright (c) 2023, Faceplate LTD. All Rights Reserved.      |
%% | Author: Karimov Adilbek, @berikka10@gmail.com                |
%% +--------------------------------------------------------------+

-module(http_broker).

-define(QUEUE(Endpoint,Ref), { queue, Endpoint, Ref}).
-define(TARGET(Endpoint, Service, Ref),{ target, Endpoint, Service, Ref}).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->

    Ref = { Timestamp, make_ref() },

    zaya_rocksdb:write(DBRef, [
        { ?QUEUE(Endpoint, Ref), Message },

        { ?TARGET(Endpoint, Target, Ref), 0 = _Attempts }
    ] ),

    http_broker_sup:start_link().

stop(_State) ->
    ok.
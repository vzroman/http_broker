%% +--------------------------------------------------------------+
%% | Copyright (c) 2023, Faceplate LTD. All Rights Reserved.      |
%% | Author: Karimov Adilbek, @berikka10@gmail.com                |
%% +--------------------------------------------------------------+

-ifndef(http_broker).
-define(http_broker, 1).

-define(DB_REF, {http_broker, '@db_ref@'}).

-define(LOGERROR(Text), logger:error(Text)).
-define(LOGERROR(Text,Params), logger:error( Text, Params)).
-define(LOGWARNING(Text), logger:warning(Text)).
-define(LOGWARNING(Text,Params), logger:warning(Text, Params)).
-define(LOGINFO(Text), logger:info(Text)).
-define(LOGINFO(Text,Params), logger:info(Text, Params)).
-define(LOGDEBUG(Text), logger:debug(Text)).
-define(LOGDEBUG(Text,Params), logger:debug(Text,Params)).

-define(DEFAULT_LISTEN_PORT, 7000).
-define(MIN_LISTEN_PORT, 0).
-define(MAX_LISTEN_PORT, 65535).

-define(DEFAULT_ORDER, 0).
-define(MIN_ORDER, 0).

%----------------Default supervisor settings------------------------------
-define(DEFAULT_MAX_RESTARTS,10).
-define(DEFAULT_MAX_PERIOD,1000).
-define(DEFAULT_STOP_TIMEOUT,600000). % 10 min.

%----------------Services defaults-----------------------------------------
-define(DEFAULT_SCAN_CYCLE,1000).


-define(SUBSCRIPTIONS_SCOPE, '$http_broker_subscriptions$').

-define(STARTING_ATTEMPT_VALUE, 0).

-define(QUEUE(Endpoint,Ref), { queue, Endpoint, Ref}).
-define(TARGET(Endpoint, Service, Ref),{ target, Endpoint, Service, Ref}).

-define(ENV(Key,Default),application:get_env(http_broker,Key,Default)).
-define(ENV(OS,Config,Default),
  (fun()->
    case os:getenv(OS) of
      false->?ENV(Config,Default);
      Value->Value
    end
   end)()
).

-endif.
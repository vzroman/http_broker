-ifndef(http_broker).
-define(http_broker, 1).

%%-define(LOGERROR(Text), logger:error(Text)).
%%-define(LOGERROR(Text,Params), logger:error( Text, Params)).
%%-define(LOGWARNING(Text), logger:warning(Text)).
%%-define(LOGWARNING(Text,Params), logger:warning(Text, Params)).
%%-define(LOGINFO(Text), logger:info(Text)).
%%-define(LOGINFO(Text,Params), logger:info(Text, Params)).
%%-define(LOGDEBUG(Text), logger:debug(Text)).
%%-define(LOGDEBUG(Text,Params), logger:debug(Text,Params)).

-define(DEFAULT_MAX_RESTARTS,10).
-define(DEFAULT_MAX_PERIOD,1000).
-define(DEFAULT_SCAN_CYCLE,1000).
-define(DEFAULT_STOP_TIMEOUT,600000). % 10 min.

-define(SET_ENV(Key,Value),application:set_env(fp,Key,Value)).
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
-ifndef(http_broker).
-define(http_broker, 1).

-define(DEFAULT_MAX_RESTARTS,10).
-define(DEFAULT_MAX_PERIOD,1000).
-define(DEFAULT_SCAN_CYCLE,1000).
-define(DEFAULT_STOP_TIMEOUT,600000). % 10 min.

-define(ENV(Key,Default),application:get_env(http_broker,Key,Default)).
-define(ENV(OS,Config,Default),
  (fun()->
    case os:getenv(OS) of
      false->?ENV(Config,Default);
      Value->Value
    end
   end)()
).
-define(SET_ENV(Key,Value),application:set_env(fp,Key,Value)).
-define(LOGSTART, (?MODULE_STRING ++ ":" ++ erlang:integer_to_list(?LINE) ++ " ")).

-define(LOGINFO(Text), lager:info(?LOGSTART ++ Text)).
-define(LOGINFO(Text,Params), lager:info(?LOGSTART ++ Text, Params)).

-endif.
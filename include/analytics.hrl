-ifndef(ANALYTICS_RECORDS_HRL).
-define(ANALYTICS_RECORDS_HRL, true).

-record(endpoint_info, {
    total_queue_count :: integer(),
    error_queue_counts :: integer(),
    targets :: map()
}).

-record(error_info, {
    time :: string(),
    text :: string()
}).

-record(target_info, {
    queue_count :: integer(),
    state :: atom(),
    attempts :: integer(),
    average_10request_duration :: integer(),
    last_error :: #error_info{}
}).


-record(system_info, {
    total_queue_count :: integer(),
    error_queues_counts :: integer(),
    endpoints :: map()
}).

-endif.
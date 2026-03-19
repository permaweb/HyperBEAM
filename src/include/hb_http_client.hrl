-define(DEFAULT_RETRIES, 0).
-define(DEFAULT_RETRY_TIME, 1000).
-define(DEFAULT_KEEPALIVE_TIMEOUT, 60_000).
-define(DEFAULT_CONNECT_TIMEOUT, 60_000).

%% Connection Pool
-define(DEFAULT_CONN_POOL_READ_SIZE, 3).
-define(DEFAULT_CONN_POOL_WRITE_SIZE, 3).
%% Keep track of available connections
-define(CONNECTIONS_ETS, hb_http_client_connections).
%% Used to keep status of the connection
-define(CONN_STATUS_ETS, hb_http_client_conn_status).
%% Used for Round-robin connection
-define(CONN_COUNTER_ETS, hb_http_client_conn_counter).
%% Used to load connection pool configuration
-define(CONN_TERM, connection_pool_size).

%% Hackney pool
-define(HACKNEY_POOL, hb_hackney_pool).
-define(DEFAULT_HACKNEY_POOL_SIZE, 1500).

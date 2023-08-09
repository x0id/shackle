%% records
-record(cast, {
    client         :: client(),
    pid            :: undefined | pid(),
    request_id     :: request_id(),
    timeout        :: timeout(),
    timestamp      :: erlang:timestamp()
}).

-record(pool_options, {
    backlog_size  :: backlog_size(),
    client        :: client(),
    max_retries   :: max_retries(),
    pool_size     :: pool_size(),
    pool_strategy :: pool_strategy(),
    test_leak     :: boolean()
}).

-record(reconnect_state, {
    current :: undefined | time(),
    max     :: time() | infinity,
    min     :: time()
}).

%% types
-type backlog_size() :: pos_integer() | infinity.
-type cast() :: #cast {}.
-type client() :: module().
-type client_option() :: {address, inet_address()} |
                         {init_options, init_options()} |
                         {ip, inet_address()} |
                         {port, inet:port_number()} |
                         {protocol, protocol()} |
                         {reconnect, boolean()} |
                         {reconnect_time_max, time() | infinity} |
                         {reconnect_time_min, time()} |
                         {socket_options, socket_options()}.

-type client_options() :: [client_option()].
-type client_state() :: term().
-type external_request_id() :: term().
-type inet_address() :: inet:ip_address() | inet:hostname().
-type inet_port() :: inet:port_number().
-type init_options() :: term().
-type max_retries() :: non_neg_integer().
-type metric_type() :: counter | timing.
-type metric_key() :: iodata().
-type metric_value() :: integer().
-type pool_name() :: atom().
-type pool_option() :: {backlog_size, backlog_size()} |
                       {max_retries, max_retries()} |
                       {pool_size, pool_size()} |
                       {pool_strategy, pool_strategy()}.

-type pool_options() :: [pool_option()].
-type pool_options_rec() :: #pool_options {}.
-type pool_size() :: pos_integer().
-type pool_strategy() :: random | round_robin.
-type protocol() :: shackle_ssl | shackle_tcp | shackle_udp.
-type reconnect_state() :: #reconnect_state {}.
-type request_id() :: {server_name(), reference()}.
-type response() :: {external_request_id(), term()}.
-type server_index() :: pos_integer().
-type server_id() :: {pool_name(), server_index()}.
-type server_name() :: atom().
-type server_opts() :: {pool_name(), server_index(), client(), client_options()}.
-type socket() :: inet:socket() | ssl:sslsocket().
-type socket_option() :: gen_tcp:connect_option() | gen_udp:option() | ssl:connect_option().
-type socket_options() :: [socket_option()].
-type table() :: atom().
-type time() :: pos_integer().

-export_type([
    client_options/0,
    init_options/0,
    pool_options/0,
    request_id/0
]).

-module(shackle_pool).
-include("shackle_internal.hrl").

-ignore_xref([
    {shackle_pool_foil, lookup, 1}
]).

-ifdef(DLZR_NO_UNKNOWN).
-dialyzer([{nowarn_function, [options/1, server/3]}, no_unknown]).
-endif.

%% public
-export([
    start/3,
    start/4,
    stop/1
]).

%% internal
-export([
    init/0,
    server/1,
    terminate/0
]).

%% public
-spec start(pool_name(), client(), client_options()) ->
    ok | {error, shackle_not_started | pool_already_started}.

start(Name, Client, ClientOptions) ->
    start(Name, Client, ClientOptions, []).

-spec start(pool_name(), client(), client_options(), pool_options()) ->
    ok | {error, shackle_not_started | pool_already_started}.

start(Name, Client, ClientOptions, Options) ->
    case options(Name) of
        {ok, _OptionsRec} ->
            {error, pool_already_started};
        {error, shackle_not_started} ->
            {error, shackle_not_started};
        {error, pool_not_started} ->
            OptionsRec = options_rec(Client, Options),
            setup(Name, OptionsRec),
            start_children(Name, Client, ClientOptions, OptionsRec),
            ok
    end.

-spec stop(pool_name()) ->
    ok | {error, shackle_not_started | pool_not_started}.

stop(Name) ->
    case options(Name) of
        {ok, #pool_options {
                pool_size = PoolSize
            } = OptionsRec} ->

            stop_children(Name, lists:seq(1, PoolSize)),
            cleanup(Name, OptionsRec),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% internal
-spec init() ->
    ok.

init() ->
    ets:new(?ETS_TABLE_POOL_INDEX, [
        named_table,
        public,
        {write_concurrency, true}
    ]),
    foil:new(?MODULE),
    foil:load(?MODULE).

-spec server(pool_name()) ->
    {ok, client(), atom(), release_fun()} |
    {error, pool_not_started | no_server | shackle_not_started}.

server(Name) ->
    case options(Name) of
        {ok, #pool_options {max_retries = MaxRetries} = Options} ->
            server(Name, Options, MaxRetries + 1);
        {error, Reson} ->
            {error, Reson}
    end.

-spec terminate() ->
    ok.

terminate() ->
    foil:delete(?MODULE).

%% private
cleanup(Name, OptionsRec) ->
    shackle_sema:delete(Name),
    shackle_queue:delete(Name),
    shackle_status:delete(Name),
    cleanup_ets(Name, OptionsRec),
    cleanup_foil(Name, OptionsRec).

cleanup_ets(Name, #pool_options {pool_strategy = round_robin}) ->
    ets:delete(?ETS_TABLE_POOL_INDEX, {Name, round_robin});
cleanup_ets(_Name, _OptionsRec) ->
    ok.

cleanup_foil(Name, #pool_options {pool_size = PoolSize}) ->
    foil:delete(?MODULE, Name),
    [foil:delete(?MODULE, {Name, N}) || N <- lists:seq(1, PoolSize)],
    foil:load(?MODULE).

options(Name) ->
    try shackle_pool_foil:lookup(Name) of
        {ok, Options} ->
            {ok, Options};
        {error, key_not_found} ->
            {error, pool_not_started}
    catch
        error:undef ->
            {error, shackle_not_started}
    end.

options_rec(Client, Options) ->
    BacklogSize = ?LOOKUP(backlog_size, Options, ?DEFAULT_BACKLOG_SIZE),
    MaxRetries = ?LOOKUP(max_retries, Options, ?DEFAULT_MAX_RETRIES),
    PoolSize = ?LOOKUP(pool_size, Options, ?DEFAULT_POOL_SIZE),
    PoolStrategy = ?LOOKUP(pool_strategy, Options, ?DEFAULT_POOL_STRATEGY),

    #pool_options {
        backlog_size = BacklogSize,
        client = Client,
        max_retries = MaxRetries,
        pool_size = PoolSize,
        pool_strategy = PoolStrategy,
        test_leak = ?LOOKUP(test_leak, Options, false)
    }.

server(_Name, #pool_options {
        client = Client
    }, 0) ->

    ?METRICS(Client, counter, <<"no_server">>),
    {error, no_server};
server(Name, #pool_options {
        backlog_size = BacklogSize,
        client = Client,
        pool_size = PoolSize,
        pool_strategy = PoolStrategy
    } = Options, N) ->

    ServerId = server_id(Name, PoolSize, PoolStrategy),
    case shackle_status:active(ServerId) of
        true when BacklogSize =:= infinity ->
            {ok, ServerName} = shackle_pool_foil:lookup(ServerId),
            {ok, Client, ServerName, undefined};
        true ->
            case shackle_sema:acquire(Name, ServerId) of
                {ok, ReleaseFun} ->
                    maybe_nap(Options),
                    {ok, ServerName} = shackle_pool_foil:lookup(ServerId),
                    {ok, Client, ServerName, ReleaseFun};
                error ->
                    ?METRICS(Client, counter, <<"backlog_full">>),
                    server(Name, Options, N - 1)
            end;
        false ->
            ?METRICS(Client, counter, <<"disabled">>),
            server(Name, Options, N - 1)
    end.

maybe_nap(#pool_options{test_leak = true}) ->
    timer:sleep(1);
maybe_nap(_) ->
    ok.

server_id(Name, PoolSize, random) ->
    {Name, shackle_utils:random(PoolSize)};
server_id(Name, PoolSize, round_robin) ->
    UpdateOps = [{2, 1, PoolSize, 1}],
    Key = {Name, round_robin},
    [ServerId] = ets:update_counter(?ETS_TABLE_POOL_INDEX, Key, UpdateOps),
    {Name, ServerId}.

setup(Name, #pool_options {
        backlog_size = BacklogSize,
        pool_size = PoolSize
    } = OptionsRec) ->

    shackle_sema:new(Name, PoolSize, BacklogSize),
    shackle_queue:new(Name),
    shackle_status:new(Name, PoolSize),
    setup_ets(Name, OptionsRec),
    setup_foil(Name, OptionsRec).

setup_ets(Name, #pool_options {pool_strategy = round_robin}) ->
    ets:insert_new(?ETS_TABLE_POOL_INDEX, {{Name, round_robin}, 1});
setup_ets(_Name, _OptionsRec) ->
    ok.

setup_foil(Name, #pool_options {pool_size = PoolSize} = OptionsRec) ->
    foil:insert(?MODULE, Name, OptionsRec),
    [foil:insert(?MODULE, {Name, N}, server_name(Name, N)) ||
        N <- lists:seq(1, PoolSize)],
    foil:load(?MODULE).

server_name(Name, Index) ->
    list_to_atom(atom_to_list(Name) ++ "_" ++ integer_to_list(Index)).

server_spec(Name, Index, Client, ClientOptions) ->
    ServerName = server_name(Name, Index),
    ServerOpts = {Name, Index, Client, ClientOptions},
    StartFunc = {?SERVER, start_link, [ServerName, ServerOpts]},
    {ServerName, StartFunc, permanent, 5000, worker, [?SERVER]}.

start_children(Name, Client, ClientOptions, #pool_options {
        pool_size = PoolSize
    }) ->

    [supervisor:start_child(?SUPERVISOR,
        server_spec(Name, Index, Client, ClientOptions)) ||
        Index <- lists:seq(1, PoolSize)].

stop_children(_Name, []) ->
    ok;
stop_children(Name, [Index | T]) ->
    ServerName = server_name(Name, Index),
    supervisor:terminate_child(?SUPERVISOR, ServerName),
    supervisor:delete_child(?SUPERVISOR, ServerName),
    stop_children(Name, T).

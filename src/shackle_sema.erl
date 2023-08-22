-module(shackle_sema).
-include("shackle.hrl").

-export([new/3, delete/1, acquire/2]).

-spec new(pool_name(), pos_integer(), pos_integer()) ->
    ok.

new(_PoolName, _PoolSize, infinity) ->
    ok;
new(PoolName, PoolSize, BacklogSize) ->
    SemaList = [sema_nif:create(BacklogSize) || _ <- lists:seq(1, PoolSize)],
    persistent_term:put({sema, PoolName}, list_to_tuple(SemaList)).

-spec acquire(pool_name(), server_id()) ->
    {ok, release_fun()} | error.

acquire(PoolName, {_, ServerId}) ->
    Sema = element(ServerId, persistent_term:get({sema, PoolName})),
    case sema_nif:occupy(Sema) of
        {ok, _} ->
            Pid = self(),
            {ok, fun () -> sema_nif:vacate(Sema, Pid) end};
        {error, backlog_full} ->
            error
    end.

-spec delete(pool_name()) ->
    ok.

delete(PoolName) ->
    persistent_term:erase(PoolName),
    ok.

-module(backlog_leak_test).

-include_lib("eunit/include/eunit.hrl").

-define(SEMA_BACKLOG, 1).

simple_test_() ->
    {setup, fun setup/0, fun cleanup/1, [{timeout, 300, fun test_leak/0}]}.

-ifdef(SEMA_BACKLOG).

test_leak() ->
    Sema = element(1, persistent_term:get({sema, arithmetic})),
    race_loop(
        4_000,
        200,
        test_backlog_leak_fun(fun () ->
            maps:get(cnt, sema_nif:info(Sema))
        end)
    ).

-else.

test_leak() ->
    ServerId = {arithmetic, 1},
    {ok, Backlog} = shackle_pool_foil:lookup({arithmetic, backlog}),
    ?assertThrow(
        {error, backlog_leak},
        race_loop(
            4_000,
            1_000_000,
            test_backlog_leak_fun(fun () ->
                element(2, hd(ets:lookup(Backlog, ServerId))) end
            end)
        )
    ).

-endif.

test_backlog_leak_fun(GetBacklogLengthF) ->
    fun(N) ->
        Self = self(),
        Len = GetBacklogLengthF(),
        Pid = spawn(fun() ->
            Self ! start,
            arithmetic_tcp_client:noop()
        end),
        spin(N),
        exit(Pid, kill),
        timer:sleep(100),
        case GetBacklogLengthF() of
            Len ->
                ok;
            _ ->
                throw({error, backlog_leak})
        end,
        receive
            start ->
                true
        after 0 ->
            false
        end
    end.

setup() ->
    setup([{backlog_size, 8}, {pool_size, 1}, {test_leak, true}]).

setup(Options) ->
    shackle_app:start(),
    arithmetic_tcp_server:start(),
    timer:sleep(100),
    arithmetic_tcp_client:start(Options),
    timer:sleep(500).

cleanup(_) ->
    error_logger:tty(false),
    arithmetic_tcp_client:stop(),
    arithmetic_tcp_server:stop(),
    shackle_app:stop().

race_loop(SpinCount, IterationsLeft, Fun) ->
    race_loop(SpinCount, Fun(SpinCount), IterationsLeft, Fun).

race_loop(SpinCount, Result, IterationsLeft, Fun) when IterationsLeft > 0 ->
    % io:format(user, "~p ~p ~p~n", [IterationsLeft, SpinCount, Result]),
    N_ = adjust_spin(Result, SpinCount),
    race_loop(N_, Fun(N_), IterationsLeft - 1, Fun);
race_loop(_, _, _, _) ->
    ok.

adjust_spin(true, SpinCount) -> SpinCount - 1;
adjust_spin(false, SpinCount) -> SpinCount + 1.

spin(0) ->
    ok;
spin(SpinCount) ->
    spin(SpinCount - 1).

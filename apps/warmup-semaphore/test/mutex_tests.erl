-module(mutex_tests).
-include_lib("eunit/include/eunit.hrl").

start_stop_test() ->
    mutex:start(),
    ?assert(lists:member(mutex, registered())),

    mutex:stop(),
    timer:sleep(100),
    ?assertNot(lists:member(mutex, registered())).


main_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun test_mutex/1
     ]}.

%% tests with started mutex

test_mutex(_) ->
    Timer = spawn(fun timer/0),

    PidA = spawn(fun () ->
            ok = mutex:wait(),
            Timer ! {timing, {mutex_start_a, erlang:now()}},
            timer:sleep(100),
            Timer ! {timing, {mutex_end_a, erlang:now()}},
            ok = mutex:signal()
        end),

    PidB = spawn(fun () ->
            timer:sleep(50),
            Timer ! {timing, {mutex_wait_b, erlang:now()}},
            ok = mutex:wait(),
            Timer ! {timing, {mutex_start_b, erlang:now()}},
            ok = mutex:signal(),
            timer:sleep(100) % let PidB run longer than PidA so we can wait in sequence
        end),

    ExitReasonPidA = wait_for_exit(PidA),
    ExitReasonPidB = wait_for_exit(PidB),

    Timer ! {self(), timings},
    {ok, Timings} =
        receive
            {timings, T} -> {ok, T};
            Other        -> Other
        end,

    MutexStartA = proplists:get_value(mutex_start_a, Timings),
    MutexEndA = proplists:get_value(mutex_end_a, Timings),
    MutexWaitB = proplists:get_value(mutex_wait_b, Timings),
    MutexStartB = proplists:get_value(mutex_start_b, Timings),

    [?_assertEqual(normal, ExitReasonPidA),
     ?_assertEqual(normal, ExitReasonPidB),
     %% expected temporal sequence: MutexStartA < MutexWaitB < MutexEndA < MutexStartB
     ?_assert(timer:now_diff(MutexWaitB, MutexStartA) > 0),
     ?_assert(timer:now_diff(MutexEndA, MutexWaitB) > 0),
     ?_assert(timer:now_diff(MutexStartB, MutexEndA) > 0)].

%% helper

setup() ->
    mutex:start().

cleanup(Pid) ->
    mutex:stop(),
    wait_for_exit(Pid),
    timer:sleep(100).

wait_for_exit(Pid) ->
    MRef = erlang:monitor(process, Pid),
    receive {'DOWN', MRef, _, _, Info} -> Info end.


timer() ->
    timer_loop([]).

timer_loop(Timestamps) ->
    receive
        {timing, Timestamp} -> timer_loop(Timestamps ++ [Timestamp]);
        {Pid, timings}      -> Pid ! {timings, Timestamps};
        _                   -> timer_loop(Timestamps)
    end.

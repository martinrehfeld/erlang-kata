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
     ]}.

%% tests with started mutex


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

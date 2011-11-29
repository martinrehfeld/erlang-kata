-module(my_db_tests).
-include_lib("eunit/include/eunit.hrl").

start_stop_test() ->
    my_db:start(),
    ?assert(lists:member(my_db, registered())),

    my_db:stop(),
    timer:sleep(100),
    ?assertNot(lists:member(my_db, registered())).


main_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun test_write/1
     ]}.

%% tests with started my_db

test_write(_) ->
    ?_assertEqual(ok, my_db:write(foo, bar)).

%% helper

setup() ->
    my_db:start().

cleanup(Pid) ->
    my_db:stop(),
    wait_for_exit(Pid).

wait_for_exit(Pid) ->
    MRef = erlang:monitor(process, Pid),
    receive {'DOWN', MRef, _, _, _} -> ok end.

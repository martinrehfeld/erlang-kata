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
      fun test_read_miss/1,
      fun test_write_and_read/1
     ]}.

%% tests with started my_db

test_read_miss(_) ->
    Result = my_db:read(baz),
    ?_assertEqual({error, instance}, Result).

test_write_and_read(_) ->
    WriteResult = my_db:write(foo, bar),
    ReadResult  = my_db:read(foo),

    [?_assertEqual(ok,        WriteResult),
     ?_assertEqual({ok, bar}, ReadResult)].

%% helper

setup() ->
    my_db:start().

cleanup(Pid) ->
    my_db:stop(),
    wait_for_exit(Pid),
    timer:sleep(100).

wait_for_exit(Pid) ->
    MRef = erlang:monitor(process, Pid),
    receive {'DOWN', MRef, _, _, _} -> ok end.

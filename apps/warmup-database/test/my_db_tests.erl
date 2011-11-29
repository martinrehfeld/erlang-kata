-module(my_db_tests).
-include_lib("eunit/include/eunit.hrl").

start_stop_test() ->
    my_db:start(),
    ?assert(lists:member(my_db, registered())),

    my_db:stop(),
    timer:sleep(100),
    ?assertNot(lists:member(my_db, registered())).

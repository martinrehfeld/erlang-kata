-module(db_tests).
-include_lib("eunit/include/eunit.hrl").

create_new_db_test() ->
    ?assertEqual([], db:new()).

destroy_db_test() ->
    Db = db:new(),
    ?assertEqual(ok, db:destroy(Db)).

-module(db_tests).
-include_lib("eunit/include/eunit.hrl").

create_new_db_test() ->
    ?assertEqual([], db:new()).

destroy_db_test() ->
    Db = db:new(),
    ?assertEqual(ok, db:destroy(Db)).

write_keys_test() ->
    Db = db:new(),

    Db1 = db:write(francesco, london, Db),
    ?assertEqual([{francesco, london}], Db1),

    Db2 = db:write(lelle, stockholm, Db1),
    ?assertEqual([{lelle, stockholm}, {francesco, london}], Db2),

    Db3 = db:write(joern, stockholm, Db2),
    ?assertEqual([{joern,stockholm},{lelle,stockholm},{francesco,london}], Db3).

read_miss_test() ->
    Db = db:new(),
    ?assertEqual({error, instance}, db:read(ola, Db)).

read_hit_test() ->
    Db = db:new(),
    Db1 = db:write(ola, barcelona, Db),
    Db2 = db:write(oliver, berlin, Db1),
    ?assertEqual({ok, barcelona}, db:read(ola, Db2)).

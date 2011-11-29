-module(db_tests).
-include_lib("eunit/include/eunit.hrl").
-include("record.hrl").

create_new_db_test() ->
    ?assertEqual([], db:new()).

destroy_db_test() ->
    Db = db:new(),
    ?assertEqual(ok, db:destroy(Db)).

write_keys_test() ->
    Db = db:new(),

    Db1 = db:write(francesco, london, Db),
    ?assertEqual([#data{key=francesco, data=london}], Db1),

    Db2 = db:write(lelle, stockholm, Db1),
    ?assertEqual([#data{key=lelle, data=stockholm},
                  #data{key=francesco, data=london}], Db2),

    Db3 = db:write(joern, stockholm, Db2),
    ?assertEqual([#data{key=joern, data=stockholm},
                  #data{key=lelle, data=stockholm},
                  #data{key=francesco, data=london}], Db3).

read_miss_test() ->
    Db = db:new(),
    ?assertEqual({error, instance}, db:read(ola, Db)).

read_hit_test() ->
    Db = db:new(),
    Db1 = db:write(ola, barcelona, Db),
    Db2 = db:write(oliver, berlin, Db1),
    ?assertEqual({ok, barcelona}, db:read(ola, Db2)).

match_test() ->
    Db = db:new(),
    Db1 = db:write(francesco, london, Db),
    Db2 = db:write(lelle, stockholm, Db1),
    Db3 = db:write(joern, stockholm, Db2),

    ?assertEqual([joern,lelle], db:match(stockholm, Db3)).

delete_test() ->
    Db = db:new(),
    Db1 = db:write(francesco, london, Db),
    Db2 = db:write(lelle, stockholm, Db1),
    Db3 = db:write(joern, stockholm, Db2),

    Db4 = db:delete(lelle, Db3),

    ?assertEqual([joern], db:match(stockholm, Db4)).

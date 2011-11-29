-module(db_tests).
-include_lib("eunit/include/eunit.hrl").
-include("record.hrl").

create_new_db_test() ->
    Db = db:new(),
    ?assertEqual([], ets:tab2list(Db)).

destroy_db_test() ->
    Db = db:new(),
    ?assertEqual(ok, db:destroy(Db)).

write_keys_test() ->
    Db = db:new(),

    Db1 = db:write(francesco, london, Db),
    ?assertEqual({ok, london}, db:read(francesco, Db1)),
    ?assertMatch([_], ets:tab2list(Db1)),

    Db2 = db:write(lelle, stockholm, Db1),
    ?assertEqual({ok, stockholm}, db:read(lelle, Db2)),
    ?assertEqual({ok, london}, db:read(francesco, Db2)),
    ?assertMatch([_, _], ets:tab2list(Db2)),

    Db3 = db:write(joern, stockholm, Db2),
    ?assertEqual({ok, stockholm}, db:read(joern, Db3)),
    ?assertEqual({ok, stockholm}, db:read(lelle, Db3)),
    ?assertEqual({ok, london}, db:read(francesco, Db3)),
    ?assertMatch([_, _, _], ets:tab2list(Db3)).

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

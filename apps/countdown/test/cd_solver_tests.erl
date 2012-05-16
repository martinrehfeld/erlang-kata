-module(cd_solver_tests).
-include_lib("eunit/include/eunit.hrl").

%%
%% TESTS
%%

add_test() ->
    ?assertEqual(skip, cd_solver:add(1, 0)),
    ?assertEqual(2,    cd_solver:add(1, 1)).

subtract_test() ->
    ?assertEqual(skip, cd_solver:subtract(1, 0)),
    ?assertEqual(skip, cd_solver:subtract(1, 2)),
    ?assertEqual(0,    cd_solver:subtract(1, 1)).

multiply_test() ->
    ?assertEqual(skip, cd_solver:multiply(1, 1)),
    ?assertEqual(2,    cd_solver:multiply(1, 2)).

divide_test() ->
    ?assertEqual(skip, cd_solver:divide(1, 1)),
    ?assertEqual(skip, cd_solver:divide(1, 2)),
    ?assertEqual(1,    cd_solver:divide(2, 2)).

%% Solutions: [{delta, #solution}, ...]
solver_input1_test() ->
    Solutions = cd_solver:solutions(203, [ 50, 100, 4, 2, 2, 4 ]),
    ?assertMatch([{0, _} | _], Solutions).

solver_input2_test() ->
    Solutions = cd_solver:solutions(465, [ 25, 4, 9, 2, 3, 10 ]),
    ?assertMatch([{0, _} | _], Solutions).

solver_input3_test() ->
    Solutions = cd_solver:solutions(241, [ 9, 8, 10, 5, 9, 7 ]),
    ?assertMatch([{0, _} | _], Solutions).

solver_input4_test() ->
    Solutions = cd_solver:solutions(824, [ 3, 7, 6, 2, 1, 7 ]),
    ?assertMatch([{2, _} | _], Solutions). % best solution is 826 with a delta of 2

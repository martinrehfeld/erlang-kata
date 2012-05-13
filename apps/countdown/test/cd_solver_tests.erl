-module(cd_solver_tests).
-include_lib("eunit/include/eunit.hrl").

%%
%% Macros for getting stacktraces on demand
%%

-define(TEST_START, try ok).
-define(TEST_END,
                ok
                catch
                    Type:X ->
                        io:format("~p~n", [{Type, X, erlang:get_stacktrace()}]),
                        ?assert(unhandled_exception)
                end).

%%
%% TESTS
%%

%% available_indexes_test() ->
%%     ?assertEqual([], cd_solver:available_indexes(2,1)),
%%     ?assertEqual([1], cd_solver:available_indexes(0,1)),
%%     ?assertEqual([2], cd_solver:available_indexes(2,2)),
%%     ?assertEqual([3], cd_solver:available_indexes(6,3)).

add_test() ->
    ?assertEqual(no_op,       cd_solver:add(1, 0)),
    ?assertEqual(2,           cd_solver:add(1, 1)).

subtract_test() ->
    ?assertEqual(no_op,       cd_solver:subtract(1, 0)),
    ?assertEqual(not_allowed, cd_solver:subtract(1, 2)),
    ?assertEqual(0,           cd_solver:subtract(1, 1)).

multiply_test() ->
    ?assertEqual(no_op,       cd_solver:multiply(1, 1)),
    ?assertEqual(2,           cd_solver:multiply(1, 2)).

divide_test() ->
    ?assertEqual(no_op,       cd_solver:divide(1, 1)),
    ?assertEqual(not_allowed, cd_solver:divide(1, 2)),
    ?assertEqual(1,           cd_solver:divide(2, 2)).

solver_test() ->
    %% Solutions: [{delta, #solution}, ...]
    Solutions1 = cd_solver:solutions(178, [100, 75, 3]),
    Solutions2 = cd_solver:solutions(203, [50, 100, 4, 2, 2, 4]),
    ?assertMatch([{0, _}], Solutions1),
    ?assertMatch([{0, _} | _], Solutions2).

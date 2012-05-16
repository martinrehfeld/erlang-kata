-module(cd_solver).

%% API
-export([solutions/2]).

%% only exported for eunit
-export([add/2, subtract/2, multiply/2, divide/2]).

-define(i2b(I), list_to_binary(integer_to_list(I))).
-define(OPERATORS, [{<<"+">>, fun add/2},
                    {<<"-">>, fun subtract/2},
                    {<<"*">>, fun multiply/2},
                    {<<"/">>, fun divide/2}]).

-record(solution, {result, expression}).


%% @doc: solve Countdown game for given Target and Numbers
solutions(Target, Numbers) ->
    A = initialize_solutions(Target, Numbers),
    A1 = combine(A, Target, ?OPERATORS, Numbers),
    BestSolutions = best_solutions(A1),
    error_logger:info_msg("Best solution(s):~n~s~n",
                          [string:join(format_solutions(BestSolutions), "\n")]),
    BestSolutions.


%% @doc: Seed an array with the base expressions aka the given numbers.
%% The solutions array uses a bitmap of used numbers for a given expression as
%% index and has dict elements with Key:Delta to target, Value: #solution
initialize_solutions(Target, Numbers) ->
    MapSize = 1 bsl length(Numbers),
    A = array:new(MapSize, {default, dict:new()}),

    F = fun(Index, Array) ->
            UsedBitmap = bitmap_for_index(Index),
            Number = lists:nth(Index, Numbers),
            Delta = abs(Target - Number),
            Expression = ?i2b(Number),
            S = #solution{result=Number, expression=Expression},
            store_solution(Array, UsedBitmap, Delta, S)
        end,
    lists:foldl(F, A, lists:seq(1, length(Numbers))).


%% @doc: entry point for recursively combining all number permutations with
%% all operators
combine(A, Target, Operators, Numbers) ->
    combine(A, Target, Operators, Numbers, solution_count(A)).

%% @doc: combination on the array level: combine every two sets of solutions
%% (each set using the same input numbers)
combine(A, Target, Operators, Numbers, NumberOfSolutions) ->
    FJ =
        fun(J, Solutions2, {I, Solutions1, Array}) ->
            case I band J of
                0 -> % Bitmaps do not overlap -> process
                    NewArray = combine_solutions(I, J, Solutions1, Solutions2, Array, Target),
                    {I, Solutions1, NewArray};

                _ -> % Bitmaps *do* overlap -> skip
                    {I, Solutions1, Array}
            end
        end,

    FI =
        fun(I, Solutions, Array) ->
            {I, Solutions, NewArray} = array:sparse_foldl(FJ, {I, Solutions, Array}, Array),
            NewArray
        end,

    A1 = array:sparse_foldl(FI, A, A),

    case solution_count(A1) of
        NumberOfSolutions -> % no new combinations found -> we are done
            A1;

        IncreasedNumberOfSolutions ->
            combine(A1, Target, Operators, Numbers, IncreasedNumberOfSolutions)
    end.


%% @doc: combination on the solutions level: combine every two expressions with
%% all the operators
combine_solutions(I, J, Solutions1, Solutions2, A, Target) ->
    FT =
        fun(_T, Solution2, {Solution1, Array}) ->
            NewArray = combine_operators(I, J, Solution1, Solution2, Array, Target),
            {Solution1, NewArray}
        end,

    FS =
        fun(_S, Solution, Array) ->
            {Solution, NewArray} = dict:fold(FT, {Solution, Array}, Solutions2),
            NewArray
        end,

    dict:fold(FS, A, Solutions1).


%% @doc: combination on the operator level: combine two specific solutions with
%% all the operators
combine_operators(I, J, S1, S2, A, Target) ->
    F = fun({OpSymbol, OpFn}, Array) ->
            case OpFn(S1#solution.result, S2#solution.result) of
                not_allowed -> Array;
                no_op       -> Array;

                Result ->
                    Expression =
                        iolist_to_binary([<<$(>>, S1#solution.expression,
                                          OpSymbol, S2#solution.expression, <<$)>>]),
                    Solution = #solution{result=Result, expression=Expression},
                    Delta = abs(Target - Result),
                    Index = I bor J,
                    store_solution(Array, Index, Delta, Solution)
            end
        end,
    lists:foldl(F, A, ?OPERATORS).


%% @doc: count all solutions in the array
solution_count(A) ->
    F = fun(_Index, Solutions, Count) -> Count + dict:size(Solutions) end,
    array:sparse_foldl(F, 0, A).


store_solution(Array, Index, Delta, #solution{} = Solution) ->
    Entries = array:get(Index, Array),
    NewEntries = dict:store(Delta, Solution, Entries),
    array:set(Index, NewEntries, Array).


%% @doc: calculate the initial bitmap for a given index into the Numbers list
bitmap_for_index(Index) -> 1 bsl (Index - 1).


%%
%% Selection and formatting of best solutions
%%

best_solutions(A) ->
    BestPerUsedNumbers = best_per_used_numbers(A),
    best_of_best(BestPerUsedNumbers).


best_per_used_numbers(A) ->
    F = fun(_UsedBitmap, Solutions, Acc) ->
            OrderedSolutions = lists:keysort(1, dict:to_list(Solutions)),
            BestSolution = lists:nth(1, OrderedSolutions),
            [BestSolution | Acc]
        end,
    array:sparse_foldl(F, [], A).


best_of_best(Solutions) ->
    OrderedSolutions = lists:keysort(1, Solutions),
    {LowestDelta, _} = lists:nth(1, OrderedSolutions),
    lists:takewhile(fun ({D, _}) -> D =:= LowestDelta end, OrderedSolutions).


format_solutions(Entries) ->
    F = fun(Entry) ->
            {Delta, #solution{result=Result, expression=Expression}} = Entry,
            io_lib:format("~s = ~p # delta: ~p", [Expression, Result, Delta])
        end,
    lists:map(F, Entries).


%%
%% Operators
%%

add(N1, N2) when N2 =/= 0 -> N1 + N2;
add(_N1, _N2) -> no_op.

subtract(N1, N2)  when N1 < N2 -> not_allowed;
subtract(_N1, N2) when N2 =:= 0 -> no_op;
subtract(N1, N2)  when N1 >= N2 -> N1 - N2.

multiply(N1, N2) when N2 =/= 1 -> N1 * N2;
multiply(_N1, _N2) -> no_op.

divide(_N1, N2) when N2 =:= 0 -> not_allowed;
divide(N1, N2) when N1 rem N2 =/= 0 -> not_allowed;
divide(_N1, N2) when N2 =:= 1 -> no_op;
divide(N1, N2) when N1 rem N2 =:= 0 -> N1 div N2.

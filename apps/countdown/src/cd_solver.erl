-module(cd_solver).

%% API
-export([solutions/2]).

%% only exported for eunit
-export([add/2, subtract/2, multiply/2, divide/2]).

-define(OPERATORS, [{'+', fun add/2},
                    {'-', fun subtract/2},
                    {'*', fun multiply/2},
                    {'/', fun divide/2}]).

-record(solution, {result, expression}).


%% @doc: solve Countdown game for given Target and Numbers
solutions(Target, Numbers) ->
    A = initialize_solutions(Target, Numbers),
    A1 = combine(A, Target, ?OPERATORS, Numbers),
    BestSolutions = best_solutions(A1),
    error_logger:info_msg("Best solution(s) out of ~w:~n~s~n",
                          [solution_count(A1),
                           string:join(format_solutions(BestSolutions), "\n")]),
    BestSolutions.


%% @doc: Seed an array with the base expressions aka the given numbers.
%% The solutions array uses a bitmap of used numbers for a given expression as
%% index and has dict elements with Key:Delta to target, Value: #solution
initialize_solutions(Target, Numbers) ->
    MapSize = 1 bsl length(Numbers),
    A = array:new(MapSize, {default, dict:new()}),

    F = fun (Index, Array) ->
            UsedBitmap = bitmap_for_index(Index),
            Number = lists:nth(Index, Numbers),
            Delta = abs(Target - Number),
            S = #solution{result = Number, expression = Number},
            add_solution(Array, UsedBitmap, Delta, S)
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
        fun (J, Solutions2, {I, Ref, Solutions1, Count}) ->
            case I band J of
                0 -> % Bitmaps do not overlap -> process
                    Index = I bor J,
                    Pid = self(),
                    spawn(fun () ->
                                combine_solutions(Pid, Ref, Index, Solutions1,
                                    Solutions2, Target)
                          end),
                    {I, Ref, Solutions1, Count + 1};

                _ -> % Bitmaps *do* overlap -> skip
                    {I, Ref, Solutions1, Count}
            end
        end,

    F = fun (Index, Solutions, Array) ->
            array:set(Index, Solutions, Array)
        end,

    FK =
        fun(Ref, Array) ->
            receive
                {new_solutions, Ref, AddtionalArray} ->
                    array:sparse_foldl(F, Array, AddtionalArray)
            end
        end,

    FI =
        fun (I, Solutions, Array) ->
            Ref = make_ref(),
            {I, Ref, Solutions, Count} = array:sparse_foldl(FJ, {I, Ref, Solutions, 0}, Array),
            lists:foldl(FK, Array, lists:duplicate(Count, Ref))
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
combine_solutions(Pid, Ref, Index, Solutions1, Solutions2, Target) ->
    FT =
        fun (_T, Solution2, {Solution1, Array}) ->
            NewArray = combine_operators(Index, Solution1, Solution2, Array, Target),
            {Solution1, NewArray}
        end,

    FS =
        fun (_S, Solution, Array) ->
            {Solution, NewArray} = dict:fold(FT, {Solution, Array}, Solutions2),
            NewArray
        end,

    Pid ! {new_solutions, Ref,
           dict:fold(FS, array:new({default, dict:new()}), Solutions1)}.


%% @doc: combination on the operator level: combine two specific solutions with
%% all the operators
combine_operators(Index, S1, S2, A, Target) ->
    F = fun ({OpSymbol, OpFn}, Array) ->
            case OpFn(S1#solution.result, S2#solution.result) of
                skip -> Array;

                Result ->
                    Expression = { S1#solution.expression, OpSymbol,
                                   S2#solution.expression },
                    Solution = #solution{result = Result, expression = Expression},
                    Delta = abs(Target - Result),
                    add_solution(Array, Index, Delta, Solution)
            end
        end,
    lists:foldl(F, A, ?OPERATORS).


%% @doc: count all solutions in the array
solution_count(A) ->
    F = fun (_Index, Solutions, Count) -> Count + dict:size(Solutions) end,
    array:sparse_foldl(F, 0, A).


%% @doc: add a new solution to the array/dict
add_solution(Array, Index, Delta, #solution{} = Solution) ->
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
    F = fun (_UsedBitmap, Solutions, Acc) ->
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
    [ format_solution(Delta, Solution) || {Delta, Solution} <- Entries ].

format_solution(Delta, #solution{result = Result, expression = Expression}) ->
    io_lib:format("~s = ~w # ~s",
                  [format_solution(Expression), Result, remark(Delta)]).

format_solution(Expression) when is_tuple(Expression) ->
    { Operand1, Operator, Operand2 } = Expression,
    io_lib:format("(~s ~s ~s)", [format_solution(Operand1),
                                 atom_to_list(Operator),
                                 format_solution(Operand2)]);
format_solution(Expression) ->
    integer_to_list(Expression).


remark(Delta) ->
    case Delta of
        0 -> "exact";
        _ -> "off by " ++ integer_to_list(Delta)
    end.


%%
%% Operators
%%

add(N1, N2) when N2 =/= 0 -> N1 + N2;
add(_N1, _N2) -> skip.

subtract(N1, N2) when N1 < N2 -> skip;
subtract(_N1, N2) when N2 =:= 0 -> skip;
subtract(N1, N2) -> N1 - N2.

multiply(N1, N2) when N2 =/= 1 -> N1 * N2;
multiply(_N1, _N2) -> skip.

divide(N1, N2) when N1 rem N2 =/= 0 -> skip;
divide(_N1, N2) when N2 =:= 0 -> skip;
divide(_N1, N2) when N2 =:= 1 -> skip;
divide(N1, N2) -> N1 div N2.

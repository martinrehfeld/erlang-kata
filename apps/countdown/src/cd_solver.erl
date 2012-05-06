-module(cd_solver).

%% API
-export([solutions/2]).

%% only exported for eunit
-export([add/2, subtract/2, multiply/2, divide/2, available_indexes/2]).

-define(OPERATORS, [{'+', fun add/2},
                    {'-', fun subtract/2},
                    {'*', fun multiply/2},
                    {'/', fun divide/2}]).

-record(vertex, {result, used}).
-record(edge,   {operator, operand}).

solutions(Target, Numbers) ->
    {G, Root} = build_graph(Numbers),
    best_solutions(G, Root, Target).

build_graph(Numbers) ->
    G = digraph:new([acyclic]),
    Used = 0,
    Root = digraph:add_vertex(G, #vertex{result = 0, used = Used}),
    {add_next_level(G, Root, available_indexes(Used, length(Numbers)), Numbers),
     Root}.

add_next_level(G, VStart, AvailableIndexes, Numbers) ->
    lists:foldl(fun(Index, G1) ->
            add_operations(G1, VStart, Index, Numbers, ?OPERATORS)
        end, G, AvailableIndexes).

add_operations(G, VStart, Index, Numbers, Operators) ->
    lists:foldl(fun(Operator, G1) ->
            add_compute_step(G1, VStart, Index, Numbers, Operator)
        end, G, Operators).

add_compute_step(G, #vertex{result=N1,used=Bitmap0}=VStart, Index, Numbers, Operator) ->
    N2 = lists:nth(Index, Numbers),
    {OperatorName, OperatorFun} = Operator,

    case OperatorFun(N1, N2) of
        no_op       -> G;
        not_allowed -> G;

        Result ->
            Bitmap1 = Bitmap0 bor (1 bsl Index),
            Vertex= #vertex{result = Result, used = Bitmap1},
            VNew = digraph:add_vertex(G, Vertex),
            digraph:add_edge(G, VStart, VNew, #edge{operator = OperatorName, operand = N2}),
            %% error_logger:info_msg("New Op ~p~n",
            %%     [digraph:edge(G, lists:nth(1, digraph:out_edges(G, VStart)))]),
            add_next_level(G, VNew, available_indexes(Bitmap1, length(Numbers)),
                Numbers)
    end.

available_indexes(Bitmap, Size) ->
    lists:filter(fun(Index) ->
            Mask = bnot (1 bsl Index),
            Bitmap bor Mask =:= Mask
        end, lists:seq(1, Size)).

best_solutions(G, Root, Target) ->
    Vertices = digraph:vertices(G),
    Deltas =
        lists:map(fun(V) ->
                {abs(V#vertex.result - Target), V}
            end, Vertices),
    OrderedDeltas =
        lists:sort(fun(A, B) ->
                {Delta1, _Vertex1} = A,
                {Delta2, _Vertex2} = B,
                Delta1 =< Delta2
            end, Deltas),
    {_BestDelta, BestVertex} = hd(OrderedDeltas),
    Solutions = [{BestVertex#vertex.result, calculations(G, Root, BestVertex)}],
    error_logger:info_msg("Found best solution (out of ~p unique results):~n~p~n",
        [length(Vertices), Solutions]),
    Solutions.

calculations(G, Root, BestVertex) ->
    calculations(G, Root, BestVertex, []).

calculations(G, Root, EndVertex, Operations) ->
    InEdges = digraph:in_edges(G, EndVertex),
    E = hd(InEdges),
    {_, SourceVertex, _, Operation} = digraph:edge(G, E),
    case SourceVertex of
        Root -> [Operation|Operations];
        _    -> calculations(G, Root, SourceVertex, [Operation|Operations])
    end.

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

divide(N1, N2) when N1 rem N2 =/= 0 -> not_allowed;
divide(_N1, N2) when N2 =:= 1 -> no_op;
divide(N1, N2) when N1 rem N2 =:= 0 -> N1 div N2.

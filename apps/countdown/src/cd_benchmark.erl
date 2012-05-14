-module(cd_benchmark).

-export([run/2, run/3]).

run(Fn, Args) ->
    Start = now(),
    apply(Fn, Args),
    End = now(),

    Runtime = timer:now_diff(End, Start) / 1000,
    error_logger:info_msg("Run with args ~p took ~p ms~n", [Args, Runtime]),
    Runtime.

run(Count, Fn, Args) ->
    TotalRuntime = lists:foldl(fun(_, Total) -> Total + run(Fn, Args) end,
                               0, lists:seq(1, Count)),
    AverageRuntime = TotalRuntime / Count,
    error_logger:info_msg("~p Iterations with args ~p took ~p ms (Avg: ~p ms)~n",
                          [Count, Args, TotalRuntime, AverageRuntime]),
    TotalRuntime.

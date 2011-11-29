%% @doc `mutex' module as a warmup exercise for the erlang course held at wooga
%% @version Exercise II-1
%%
%% @author Martin Rehfeld <martin.rehfeld@glnetworks.de>

-module(mutex).

-export([start/0, stop/0, wait/0, signal/0]).

-record(state, {semaphore}).

%% @doc mutex:start() ⇒ ok.
start() ->
    register(?MODULE, spawn(fun init/0)),
    ok.

%% @doc mutex:stop() ⇒ ok.
stop() ->
    ?MODULE ! shutdown,
    ok.

%% @doc mutex:wait() ⇒ ok.
wait() ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, wait},
    receive
        {Ref, Result} -> Result
    end.

%% @doc mutex:signal() ⇒ ok.
signal() ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, signal},
    receive
        {Ref, Result} -> Result
    after 5000 ->
        {error, timeout}
    end.

%% @private
init() ->
    loop(#state{semaphore=free}).

%% @private
loop(#state{semaphore=Semaphore}=S) ->
    receive
        {Pid, MsgRef, wait} when Semaphore =:= free ->
            Pid ! {MsgRef, ok},
            loop(#state{semaphore=busy});
        {Pid, MsgRef, signal} when Semaphore =:= busy ->
            Pid ! {MsgRef, ok},
            loop(#state{semaphore=free});

        shutdown ->
            exit(shutdown);
        code_change ->
            ?MODULE:loop(S)
    end.

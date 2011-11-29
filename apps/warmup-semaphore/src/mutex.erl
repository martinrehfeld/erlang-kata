%% @doc `mutex' module as a warmup exercise for the erlang course held at wooga
%% @version Exercise II-1
%%
%% @author Martin Rehfeld <martin.rehfeld@glnetworks.de>

-module(mutex).

-export([start/0, stop/0, wait/0, signal/0]).

-record(state, {semaphore, holder=undefined}).

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
    process_flag(trap_exit, true),
    loop(#state{semaphore=free}).

%% @private
loop(#state{semaphore=Semaphore, holder=Holder}=S) ->
    receive
        {Pid, MsgRef, wait} when Semaphore =:= free ->
            catch link(Pid),
            Pid ! {MsgRef, ok},
            loop(S#state{semaphore=busy, holder=Pid});
        {Pid, MsgRef, signal} when Semaphore =:= busy ->
            Pid ! {MsgRef, ok},
            unlink(Pid),
            %% clean any pending exit messages for Pid from mailbox
            receive
                {'EXIT', Pid, _} -> true
            after 0 ->
                true
            end,
            loop(S#state{semaphore=free, holder=undefined});

        {'EXIT', FromPid, _Reason} ->
            %% clean any pending messages from Pid from mailbox
            receive
                {FromPid, _, wait}   -> true;
                {FromPid, _, signal} -> true
            after 0 ->
                true
            end,
            %% free the semaphore when exit came from the current holder
            case FromPid of
                Holder -> loop(S#state{semaphore=free});
                _      -> loop(S)
            end;

        shutdown ->
            exit(shutdown);
        code_change ->
            ?MODULE:loop(S)
    end.

%% @doc `my_db' module as a warmup exercise for the erlang course held at wooga
%% @version Exercise 2
%%
%% @author Martin Rehfeld <martin.rehfeld@glnetworks.de>

-module(my_db).

-export([start/0, stop/0, write/2, delete/1, read/1, match/1]).

%% @doc ￼my_db:start() ⇒ ok.
start() ->
    register(?MODULE, spawn(fun init/0)),
    ok.

%% @doc my_db:stop() ⇒ ok.
stop() ->
    ?MODULE ! shutdown,
    ok.

%% @doc my_db:write(Key, Element) ⇒ ok.
write(Key, Element) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {write, Key, Element}},
    receive
        {Ref, Result} -> Result
    after 5000 ->
        {error, timeout}
    end.

%% @doc my_db:delete(Key) ⇒ ok.
delete(Key) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {delete, Key}},
    receive
        {Ref, Result} -> Result
    after 5000 ->
        {error, timeout}
    end.

%% @doc my_db:read(Key) ⇒ {ok, Element} | {error, instance}.
read(Key) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {read, Key}},
    receive
        {Ref, Result} -> Result
    after 5000 ->
        {error, timeout}
    end.

%% @doc my_db:match(Element) ⇒ [Key1, ..., KeyN].
match(Element) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {match, Element}},
    receive
        {Ref, Result} -> Result
    after 5000 ->
        {error, timeout}
    end.

%% @private
init() ->
    Db = db:new(),
    loop(Db).

%% @private
loop(Db) ->
    receive
        {Pid, MsgRef, {write, Key, Element}} ->
            Db1 = db:write(Key, Element, Db),
            Pid ! {MsgRef, ok},
            loop(Db1);
        {Pid, MsgRef, {delete, Key}} ->
            Db1 = db:delete(Key, Db),
            Pid ! {MsgRef, ok},
            loop(Db1);
        {Pid, MsgRef, {read, Key}} ->
            Pid ! {MsgRef, db:read(Key, Db)},
            loop(Db);
        {Pid, MsgRef, {match, Element}} ->
            Pid ! {MsgRef, db:match(Element, Db)},
            loop(Db);

        shutdown ->
            exit(shutdown);
        code_change ->
            ?MODULE:loop(Db);
        Unknown ->
            error_logger:warning_msg("my_db received unknown message: ~p~n",[Unknown]),
            loop(Db)
    end.

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
    ?MODULE ! {write, Key, Element},
    ok.

%% @doc my_db:delete(Key) ⇒ ok.
delete(Key) ->
    ok.

%% @doc my_db:read(Key) ⇒ {ok, Element} | {error, instance}.
read(Key) ->
    {error, instance}.

%% @doc my_db:match(Element) ⇒ [Key1, ..., KeyN].
match(Element) ->
    [].

%% @private
init() ->
    Db = db:new(),
    loop(Db).

%% @private
loop(Db) ->
    receive
        {write, Key, Element} ->
            db:write(Key, Element, Db),
            loop(Db);

        shutdown ->
            exit(shutdown);
        code_change ->
            ?MODULE:loop(Db);
        Unknown ->
            error_logger:warning_msg("my_db received unknown message: ~p~n",[Unknown]),
            loop(Db)
    end.

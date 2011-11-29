%% @doc `db' module as a warmup exercise for the erlang course held at wooga
%% @version Exercise 1
%%
%% @author Martin Rehfeld <martin.rehfeld@glnetworks.de>

-module(db).

-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).

%% @doc ￼db:new() ⇒ Db.
new() ->
    [].

%% @doc db:destroy(Db) ⇒ ok.
destroy(Db) ->
    ok.

%% @doc db:write(Key, Element, Db) ⇒ NewDb.
write(Key, Element, Db) ->
    Db.

%% @doc db:delete(Key, Db) ⇒ NewDb.
delete(Key, Db) ->
    Db.

%% @doc db:read(Key, Db) ⇒{ok, Element} | {error, instance}.
read(Key, Db) ->
    {error, instance}.

%% @doc db:match(Element, Db) ⇒ [Key1, ..., KeyN].
match(Element, Db) ->
    [].
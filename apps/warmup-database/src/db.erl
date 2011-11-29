%% @doc `db' module as a warmup exercise for the erlang course held at wooga
%% @version Exercise 4
%%
%% @author Martin Rehfeld <martin.rehfeld@glnetworks.de>

-module(db).
-include("record.hrl").

-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).

%% @doc ￼db:new() ⇒ Db.
new() ->
    ets:new(db, [private, {keypos,2}]).

%% @doc db:destroy(Db) ⇒ ok.
destroy(Db) ->
    ets:delete(Db),
    ok.

%% @doc db:write(Key, Element, Db) ⇒ NewDb.
write(Key, Element, Db) ->
    ets:insert(Db, #data{key=Key, data=Element}),
    Db.

%% @doc db:delete(Key, Db) ⇒ NewDb.
delete(Key, Db) ->
    ets:delete(Db, Key),
    Db.

%% @doc db:read(Key, Db) ⇒{ok, Element} | {error, instance}.
read(Key, Db) ->
    case ets:lookup(Db, Key) of
        []       -> {error, instance};
        [Record] -> {ok, Record#data.data}
    end.

%% @doc db:match(Element, Db) ⇒ [Key1, ..., KeyN].
match(Element, Db) ->
    Matches = ets:match(Db, #data{key='$1', data=Element}),
    [hd(X) || X <- Matches].

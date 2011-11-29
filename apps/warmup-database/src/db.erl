%% @doc `db' module as a warmup exercise for the erlang course held at wooga
%% @version Exercise 3
%%
%% @author Martin Rehfeld <martin.rehfeld@glnetworks.de>

-module(db).
-include("record.hrl").

-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).

%% @doc ￼db:new() ⇒ Db.
new() ->
    [].

%% @doc db:destroy(Db) ⇒ ok.
destroy(_Db) ->
    ok.

%% @doc db:write(Key, Element, Db) ⇒ NewDb.
write(Key, Element, Db) ->
    [#data{key=Key, data=Element}|Db].

%% @doc db:delete(Key, Db) ⇒ NewDb.
delete(Key, Db) ->
    reject(Key, Db, []).

%% @doc db:read(Key, Db) ⇒{ok, Element} | {error, instance}.
read(Key, Db) ->
    get(Key, Db).

%% @doc db:match(Element, Db) ⇒ [Key1, ..., KeyN].
match(Element, Db) ->
    find(Element, Db).


%% @private
get(Key, [#data{key=Key, data=Element}|_T]) ->
    {ok, Element};
get(Key, [_Record|T]) ->
    get(Key, T);
get(_Key, []) ->
    {error, instance}.

%% @private
find(Element, Db) ->
    Matches = [],
    find(Element, Db, Matches).

find(Element, [#data{key=Key, data=Element}|T], Matches) ->
    find(Element, T, Matches ++ [Key]);
find(Element, [_Record|T], Matches) ->
    find(Element, T, Matches);

find(_Element, [], Matches) ->
    Matches.

%% @private
reject(Key, [#data{key=Key}|T], Matches) ->
    reject(Key, T, Matches);
reject(Key, [Record|T], Matches) ->
    reject(Key, T, Matches ++ [Record]);

reject(_Key, [], Matches) ->
    Matches.

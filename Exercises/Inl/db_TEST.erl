%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File: db_TEST.erl
%%% @author trainers@erlang-solutions.com
%%% @copyright 1999-2013 Erlang Solutions Ltd.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Test the 'db' module. Has the following interface:
%%%
%%% db_TEST:all() -
%%%     runs all tests.
%%%
%%% db_TEST:insert_read_all() -
%%%     insert 3 elements and read them. Also test reading a
%%%     non-existent key.
%%%
%%% db_TEST:insert_delete_one() -
%%%     insert 3 elements and delete each of them. Also test deleting
%%%     a non-existent key.
%%%
%%% db_TEST:insert_delete_all() -
%%%     insert 3 elements and delete all of them but in different
%%%     order. All should return empty database.
%%%
%%% db_TEST:insert_overwrite_one() -
%%%     insert 3 elements and overwrite each of them one at a time.
%%%
%%% db_TEST:insert_overwrite_all() -
%%%     insert 3 elements and overwrite all of them.

-module(db_TEST).
-export([all/0,insert_read_all/0,insert_delete_one/0,insert_delete_all/0,
	 insert_overwrite_one/0,insert_overwrite_all/0]).

%% all() -> ok.
%%  Run all the tests.

all() ->
    insert_read_all(),
    insert_delete_one(),
    insert_delete_all(),
    insert_overwrite_one(),
    insert_overwrite_all().

%% insert_read_all() -> ok.
%%  Insert standard 3 elements and reads them.

insert_read_all() ->
    io:format("Running insert_read_all", []),
    Db0 = init_db([{curt,1},{bert,2},{sune,3}]),

    io:format(" 1", []),
    {ok,1} = db:read(curt, Db0),

    io:format(" 2", []),
    {ok,2} = db:read(bert, Db0),

    io:format(" 3", []),
    {ok,3} = db:read(sune, Db0),

    io:format(" 4", []),			%Read non-existent key
    {error,instance} = db:read(sten, Db0),

    io:format(" - ok\n").

%% insert_delete_one() -> ok.
%%  Insert standard 3 elements and try deleting one of them.

insert_delete_one() ->
    io:format("Running insert_delete_one", []),
    Db0 = init_db([{curt,1},{bert,2},{sune,3}]),

    io:format(" 1", []),
    {ok,1} = db:read(curt, Db0),
    {error,instance} = db:read(curt, db:delete(curt, Db0)),

    io:format(" 2", []),
    {ok,2} = db:read(bert, Db0),
    {error,instance} = db:read(bert, db:delete(bert, Db0)),

    io:format(" 3", []),
    {ok,3} = db:read(sune, Db0),
    {error,instance} = db:read(sune, db:delete(sune, Db0)),

    io:format(" 4", []),			%Delete non-existent key
    {error,instance} = db:read(sten, Db0),
    {error,instance} = db:read(sten, db:delete(sten, Db0)),

    io:format(" - ok\n").

%% insert_delete_all() -> ok.
%%  Insert standard 3 elements and try deleting all of them in
%%  different orders. Should return the empty database.

insert_delete_all() ->
    io:format("Running insert_delete_all", []),
    Empty = db:new(),
    Db0 = init_db([{curt,1},{bert,2},{sune,3}]),

    io:format(" 1", []),
    Empty = lists:foldl(fun (K, D) -> db:delete(K, D) end,
			Db0, [curt,bert,sune]),
    io:format(" 2", []),
    Empty = lists:foldl(fun (K, D) -> db:delete(K, D) end,
			Db0, [bert,sune,curt]),
    io:format(" 3", []),
    Empty = lists:foldl(fun (K, D) -> db:delete(K, D) end,
			Db0, [sune,bert,curt]),
    io:format(" - ok\n").

%% insert_overwrite_one() -> ok.
%%  Insert standard 3 elements and try overwriting one of them.

insert_overwrite_one() ->
    io:format("Running insert_overwrite_one", []),
    Db0 = init_db([{curt,1},{bert,2},{sune,3}]),

    io:format(" 1", []),
    {ok,1} = db:read(curt, Db0),
    {ok,10} = db:read(curt, db:write(curt, 10, Db0)),

    io:format(" 2", []),
    {ok,2} = db:read(bert, Db0),
    {ok,20} = db:read(bert, db:write(bert, 20, Db0)),

    io:format(" 3", []),
    {ok,3} = db:read(sune, Db0),
    {ok,30} = db:read(sune, db:write(sune, 30, Db0)),

    io:format(" - ok\n").

%% insert_overwrite_all() -> ok.
%%  Insert standard 3 elements and try overwriting all of them.

insert_overwrite_all() ->
    io:format("Running insert_overwrite_all", []),
    Db0 = init_db([{curt,1},{bert,2},{sune,3}]),
    Db1 = write_db([{curt,10},{bert,20},{sune,30}], Db0),

    io:format(" 1", []),
    {ok,10} = db:read(curt, Db1),

    io:format(" 2", []),
    {ok,20} = db:read(bert, Db1),

    io:format(" 3", []),
    {ok,30} = db:read(sune, Db1),

    io:format(" 4", []),			%Read non-existent key
    {error,instance} = db:read(sten, Db1),
    io:format(" - ok\n").

%% init_db([{Key,Val}]) -> Database.
%%  Create and initialise a database with keys curt, bert and sune.

init_db(Vals) -> write_db(Vals, db:new()).

%% write_db([{Key,Val}], Database) -> Database.
%%  Writes in all key-val into existing database.

write_db(Vals, Db) ->
    lists:foldl(fun ({K,V}, D) -> db:write(K, V, D) end, Db, Vals).

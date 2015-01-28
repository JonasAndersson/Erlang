%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File: my_db_TEST.erl
%%% @author trainers@erlang-solutions.com
%%% @copyright 1999-2013 Erlang Solutions Ltd.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Test a database server. These tests should run on all the servers
%%% so we give the name of module which implements the server as an
%%% argument. For example we can call my_db_TEST:all(my_db).
%%%
%%% It has the following interface:
%%%
%%% my_db_TEST:all(DbModuleName) -
%%%     runs all the tests.
%%%
%%% my_db_TEST:start_stop(DbModuleName) -
%%%     test starting and stopping the database twice.
%%%
%%% my_db_TEST:insert_read_all(DbModuleName) -
%%%     insert 3 elements and read them all from a single client.
%%%
%%% my_db_TEST:insert_par_read_all(DbModuleName) -
%%%     insert 3 elements and read them all from multiple clients in
%%%     parallel. Prints in which order the clients complete.
%%%
%%% my_db_TEST:insert_par_overwrite_all(DbModuleName) -
%%%     insert 3 elements and overwrite them all from multiple clients
%%%     in parallel. Prints in which order the clients complete.


-module(my_db_TEST).
-export([all/1,start_stop/1,insert_read_all/1,insert_par_read_all/1,
	 insert_par_overwrite_all/1]).
-export([pmap/2]).

%% all(DbModule) -> ok.
%%  Run all the tests.

all(Mod) ->
    start_stop(Mod),
    insert_read_all(Mod),
    insert_par_read_all(Mod),
    insert_par_overwrite_all(Mod).

%% start_stop(DbModule) -> ok.
%%  Test starting and stopping a database.

start_stop(Mod) ->
    io:format("Running start_stop", []),
    ok = Mod:start(),
    ok = Mod:stop(),
    timer:sleep(100),				%Give it time to stop
    ok = Mod:start(),
    ok = Mod:stop(),
    io:format(" - ok\n").

%% insert_read_all(DbModule) -> ok.
%%  Insert standard 3 elements and reads them.

insert_read_all(Mod) ->
    io:format("Running insert_read_all", []),
    Do = fun () ->
		 ok = write_db(Mod, [{curt,1},{bert,2},{sune,3}]),

		 io:format(" 1", []),
		 {ok,1} = Mod:read(curt),

		 io:format(" 2", []),
		 {ok,2} = Mod:read(bert),

		 io:format(" 3", []),
		 {ok,3} = Mod:read(sune),

		 io:format(" 4", []),		%Read non-existent key
		 {error,instance} = Mod:read(sten),

		 io:format(" - ok\n")
	 end,
    ok = with_db(Mod, Do).

%% insert_par_read_all(DbModule) -> ok.
%%  Insert standard 3 elements and reads them from multiple processes
%%  in parallel.

insert_par_read_all(Mod) ->
    io:format("Running insert_par_read_all", []),
    Child = fun (I) ->
		    {ok,1} = Mod:read(curt),
		    {ok,2} = Mod:read(bert),
		    {ok,3} = Mod:read(sune),
		    {error,instance} = Mod:read(sten),
		    io:format(" ~p", [I])
	    end,
    Do = fun () ->
		 ok = write_db(Mod, [{curt,1},{bert,2},{sune,3}]),
		 pmap(Child, lists:seq(1, 100)),
		 io:format(" - ok\n")
	 end,
    with_db(Mod, Do).

%% insert_par_overwrite_all(DbModule) -> ok.
%%  Insert standard 3 elements and overwrite them from multiple processes
%%  in parallel and read them all afterwards.

insert_par_overwrite_all(Mod) ->
    io:format("Running insert_par_overwrite_all", []),
    Child = fun (I) ->
		    ok = write_db(Mod, [{curt,10},{bert,20},{sune,30}]),
		    io:format(" ~p", [I])
	    end,
    Do = fun () ->
		 ok = write_db(Mod, [{curt,1},{bert,2},{sune,3}]),
		 pmap(Child, lists:seq(1, 100)),

		 {ok,10} = Mod:read(curt),
		 {ok,20} = Mod:read(bert),
		 {ok,30} = Mod:read(sune),
		 {error,instance} = Mod:read(sten),
		 io:format(" - ok\n")
	 end,
    with_db(Mod, Do).

%% with_db(DbModule, Do) -> Return.
%%  Start a database, execute Do() then stop the database.

with_db(Mod, Do) ->
    ok = Mod:start(),
    Ret = (catch Do()),				%Don't crash us
    ok = Mod:stop(),
    timer:sleep(100),				%Give it time to stop
    Ret.

%% write_db(DbModule, [{Key,Val}]) -> ok.
%%  Writes in all key-val into existing database.

write_db(Mod, Vals) ->
    lists:foldl(fun ({K,V}, ok) -> Mod:write(K, V) end, ok, Vals).

%% pmap(Do, List) -> [Res].
%%  Do a Parallel Map spawning Count separate processes for each
%%  element in List and returning a list of the return values. We
%%  spawn a separate master process to make sure we create as little
%%  baggage as possible in the calling process.

pmap(Do, List) ->
    Self = self(),
    Pid = spawn(fun () -> pmap_master(Self, Do, List) end),
    receive
	{return,Pid,Ret} -> Ret
    end.

pmap_master(Starter, Do, List) ->
    process_flag(trap_exit, true),		%Trap exits
    Self = self(),
    %% Spawn N processes all doing Do().
    Prs = [ spawn_link(fun() -> pmap_child(Self, E, Do) end) || E <- List ],
    %% Wait for all processes to die.
    Rs = [ pmap_result(Pid) || Pid <- Prs ],
    Starter ! {return,self(),Rs}.		%Return result

pmap_child(Master, E, Do) ->
    Ret = Do(E),
    Master ! {reply,self(),Ret}.

pmap_result(Pid) ->
    receive
	{reply,Pid,Ret} -> Ret;
	{'EXIT',Pid,normal} -> ok;		%Just return ok here
	{'EXIT',Pid,_} -> error
    end.

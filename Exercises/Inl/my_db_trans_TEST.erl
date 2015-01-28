%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File: my_db_trans_TEST.erl
%%% @author trainers@erlang-solutions.com
%%% @copyright 1999-2013 Erlang Solutions Ltd.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Test a database server. These tests should run on all the servers
%%% which have lock/unlock so we give the name of module which
%%% implements the server as an argument. For example we can call
%%% my_db_trans_TEST:all(my_db_trans).
%%%
%%% It has the following interface:
%%%
%%% my_db_trans_TEST:all(DbModuleName) -
%%%     runs all the tests.
%%%
%%% my_db_trans_TEST:simple_locking(DbModuleName) -
%%%     write a value then read it in parallel from multiple clients
%%%     with transactions.
%%%
%%% my_db_trans_TEST:unlocked_update(DbModuleName) -
%%%     insert a counter and then increment in parallel from multiple
%%%     clients without transactions. Return the final value of the
%%%     counter.
%%%
%%% my_db_trans_TEST:locked_update(DbModuleName) -
%%%     insert a counter and then increment in parallel from multiple
%%%     clients with transactions. Return the final value of the
%%%     counter.

-module(my_db_trans_TEST).
-export([all/1,simple_locking/1,unlocked_update/1,locked_update/1]).

%% all(DbModule) -> ok.
%%  Run all the tests.

all(Mod) ->
    simple_locking(Mod),
    unlocked_update(Mod),
    locked_update(Mod).

%% simple_locking(DbModule) -> ok.
%%  Run multiple clients in parallel writing adn reading a value.

simple_locking(Mod) ->
    io:format("Running simple_locking", []),
    Child = fun (I) ->
		    %% Sleep between writing and reading
		    ok = Mod:lock(),
		    ok = Mod:write(curt, I),
		    timer:sleep(100),
		    {ok,I} = Mod:read(curt),
		    ok = Mod:unlock()
	    end,
    Do = fun () ->
		 ok = write_db(Mod, [{curt,1}]),
		 pmap(Child, lists:seq(1, 100)),
		 ok
	 end,
    ok = with_db(Mod, Do),
    io:format(" - ok\n").

%% unlocked_update(DbModule) -> ok.
%%  Insert a counter and increment in parallel from multiple clients
%%  without locking.

unlocked_update(Mod) ->
    io:format("Running unlocked_update", []),
    Child = fun (I) ->				%Child action
		    %% Sleep before action and between reading and writing.
		    timer:sleep(I rem 100),
		    {ok,C} = Mod:read(counter),
		    timer:sleep(I rem 100),
		    ok = Mod:write(counter, C+1)
	    end,
    Do = fun () ->				%Master action
		 ok = write_db(Mod, [{counter,0}]),
		 pmap(Child, lists:seq(1,100)),
		 Mod:read(counter)		%Read final value of counter
	 end,
    {ok,Count} = with_db(Mod, Do),
    io:format(" - ~w\n", [Count]).

%% locked_update(DbModule) -> ok.
%%  Insert a counter and increment in parallel from multiple clients
%%  inside lock/unlock transactions.

locked_update(Mod) ->
    io:format("Running locked_update", []),
    Child = fun (I) ->				%Child action
		    %% Sleep before action and between reading and writing.
		    timer:sleep(I rem 100),
		    ok = Mod:lock(),		%Lock
		    {ok,C} = Mod:read(counter),
		    timer:sleep(I rem 100),
		    ok = Mod:write(counter, C+1),
		    ok = Mod:unlock()		%Unlock
	    end,
    Do = fun () ->				%Master action
		 %% Add a counter to db and read/write processes.
		 ok = write_db(Mod, [{counter,0}]),
		 pmap(Child, lists:seq(1,100)),
		 Mod:read(counter)		%Read final value of counter
	 end,					
    {ok,Count} = with_db(Mod, Do),
    io:format(" - ~w\n", [Count]).

%% with_db(DbModule, Do) -> Return.
%%  Start a database, execute Do() then stop the database.

with_db(Mod, Do) ->
    ok = Mod:start(),
    Ret = (catch Do()),				%Don't crash us
    ok = Mod:stop(),
    timer:sleep(100),				%Give it time to stop
    Ret.

%% write_my_db_trans([{Key,Val}]) -> ok.
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

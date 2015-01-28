%Assignment 3.* in textbook or 2.7 in handout.
%Jonas Andersson, jonand4
%db.erl

% The db should be able to:
% new -> create a new list.
% destroy - > remove the db.
% write -> create new element in db.
% delete -> remove item from db.
% read -> read item from db.
% match -> search for full match in db.

-module(db). 
-export([new/0,destroy/1,write/3]).

%Creates new reference to an empty list.
new() ->
    {empty}.

% This drops the reference to the db.
% as mentioned in the literature, if a file or db exists, this needs to be deleted aswell.
destroy(Db) ->
    ok.

%This should write a new tuple to db. If tuple exists this should be an update.
%The tree structure should be:
%{Key, Value, LeftBranch, RightBranch} for nodes and
% 'empty' atoms for leaves.
write(Key, Element, Db) when Db =/= {empty}->
    {TempKey,TempValue,Less,More} = Db,
    Bool = Key<TempKey,
    case Bool of
	true -> case Less of
		    empty -> {TempKey,TempValue,{Key,Element,Empty,Empty},More};
		 _    -> write(Key, Element, Less)
		end;
	false -> case More of
		     empty ->{TempKey,TempValue,Less,{Key,Element,Empty,Empty}};
		   -> write(Key,Element,More)
		 end
    end.
write(Key, Element, Db) -> {Key,Element,Db}.

    

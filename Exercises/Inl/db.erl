%Jonas Andersson, jonand4
%Exercise 2.7 from handouts.

-module(db).
-export([new/0,write/3,read/2,delete/2, match/2, destroy/1]).

%Returns empty tuple. (Reference to new Db)
new() ->
    {node, empty}.

%Deletes the entire db, if a file or similar exists this must be removed.
destroy(Db) ->
    ok.

%Matching empty node and writes to it.
write(Key, Value, {node, empty}) ->
    {node, {Key, Value, {node, empty}, {node,empty}}};

%Matching node and checks if it Key is less then the current Key
write(Key, Value,{node,{ OldKey, OldValue, Less, More}}) when Key < OldKey ->
    {node,{OldKey, OldValue, write(Key, Value, Less), More}};

%Matching node and checks if it Key is less then the current Key
write(Key, Value, {node, {OldKey,OldValue, Less,More}}) when Key > OldKey ->
    {node, {OldKey, OldValue, Less, write(Key,Value,More)}};

%Matching key and replaces it with the new value . (Update)
write(Key, Value, {node, {Key,_, Less,More}}) ->
    {node, {Key,Value,Less,More}}.

%Tries to find Key in the tree and returns the value.
%First clause catches if db is empty or on a leaf. (Ie. Key not found).
read(_, {node, empty})->
    undefined;

%Stops at this clause if key is found and then returns its value.
read(Key, {node, {Key,Value,_,_}}) ->
    {Key, Value};

%Goes to Less node through the tree until empty node is hit.
read(Key, {node,{CurrentKey,_,Less,_}}) when Key < CurrentKey ->
    read(Key, Less);

%Goes to More node if it exists.
read(Key,{node,{_,_,_,More}}) ->
    read(Key, More).

%Finds and deletes the Key. Returns a new "updated" reference to Db.
delete(Key, {node,empty})->
    {node, empty};
%If key is found, remove it and attach leaves correctly. (Matches Key and Key).
delete(Key,{node,{Key, Value, Less, More}}) ->  
   % {node,empty},
  %  write(),
  %  write();
    delete_inner();
%Runs left in the tree.
delete(Key, {node,{CurrentKey,_,Less,_}}) when Key<CurrentKey ->
    delete(Key,Less);
%Runs right in the tree.
delete(Key,{node,{_,_,_,More}}) ->
    delete(Key,More).

delete_inner() ->
    ok.

%Returns a list of Keys that matches the Value.
match(Value, {node,empty}) ->
    stub.



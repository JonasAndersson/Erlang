%Exercise 2.4 Erlang
%Jonas Andersson, jonand4
%db.erl

-module(db).
-export([new/0]).
-export([write/2]).

new()->
    [].
write(Tuple, Db)->
    [Tuple|Db].
read(Key,Db)->
    %TODO -------------------

%Exercise 3.4 Erlang
%Jonas Andersson, jonand4
%db.erl

-module(db).
-export([new/0]).
-export([write/2]).
-export[(read/2)].
-export[(delete/2)].

new()->
    [].
write(Tuple, Db)->
    [Tuple|Db].

read(Key,Db) when Db /= []->
    [H|L] = Db,
    {N,_,_} = H,
    case Key of
	N -> H;
	_  -> read(Key,L)
     end.

delete(Key,Db) when Db /=[]->
    [H|L] = Db,
    {N,_,_} = H,
    case Key of
	N -> ;
	_  -> delete(Key,L)
      end.

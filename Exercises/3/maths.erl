%Exercises for chapter 3 Erlang.
%Jonas Andersson, jonand4
%math.erl

-module(maths).
-export([sum/1]).
-export([sum_interval/2]). 
-export([create/1]).
-export([create_reverse/1]).
-export([print/1]).
-export([even_print/1]).

sum(0) -> 0;
sum(N) when N > 0 -> N + sum(N-1).

sum_interval(N,M) when  N==M -> M;
sum_interval(N,M) -> N + sum_interval(N+1,M).

create(0)->
    [];
create(N) -> create(N,[]).
create(N,List) when N == 0 ->
    List;
create(N,List) when N > 0 ->  
    create(N-1,[N|List]).
    
create_reverse(0)->
    [];
create_reverse(N) -> create_reverse(N,[],1).
create_reverse(N,List,M) when N  == M ->
    [M|List];
create_reverse(N,List,M) when M < N ->  
    create_reverse(N,[M|List],M+1).

even_print(0) ->
    ok;
even_print(N)->
    even_print(N,1).
even_print(N,M) when (M-1)==N ->
    even_print(0);
even_print(N,M) when M rem 2 == 0 -> io:format("Number:~p~n",[M]), even_print(N,M+1);
even_print(N,M) -> even_print(N,M+1). 

print(0) ->
    ok;
print(N)->
    print(N,1).
print(N,M) when (M-1)==N ->
    print(0);
print(N,M) -> io:format("Number:~p~n",[M]), print(N,M+1).

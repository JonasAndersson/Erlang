%Exercises for chapter 2 Erlang.
%Jonas Andersson, jonand4
%math.erl

-module(math).
-export([sum/1]). %Will return the sum of all values between 1 and N.
%-export([sum_interval/2]).

sum(N)->
    case N of
	N /=  0 ->
	    N + sum(N-1);
	N -> 0
end.

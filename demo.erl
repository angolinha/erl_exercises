-module(demo).
-export([double/1]).
-import(math, [sqrt/1]).

double(Value) ->
	times(Value, 2).
times(X, Y) ->
	X*Y.


-module(temp).
-export([slave/0]).

slave() ->
	master ! "It's alive!",
	exit(0)
.

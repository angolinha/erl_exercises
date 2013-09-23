-module(temp).
-export([start/1, slave/0]).

start(N) ->
    List = create_slaves(N),        %% create N slave processes and obtain list od then in form {Pid, order_num}
	io:format("~w ~n", [List])
.

create_slaves(N) ->
    case ( N == 0 ) of
        true ->
            List = {};                                    %% at the end of recursion List is empty and waits for additions
        false ->
            Pid = spawn(temp, slave, []),         %% save Pid of created slave process
            List = [ create_slaves(N-1) | { Pid, N } ]      %% form list of previously created slave processes and currently created process
    end,
    List                                                    %% return list of all created slave processes
.

slave() ->
	null
.

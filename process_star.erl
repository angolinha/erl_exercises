-module(process_star).
-export([start/3,leaf/1]).

start(M, N, Message) ->
	flush(),
	initiate(N, M, Message),
	operate(N*M),
	'END'
.

initiate(N, M, Msg) ->
	case ( N == 0 ) of
		true ->
			"";
		false ->
			spawn(process_star, leaf, [M]) ! {self(), Msg},		%% recursively spawn N processes with M in function call and send them Root's PID & Msg
			initiate(N-1, M, Msg)
	end
.

operate(Reps) ->
	case ( Reps == 0 ) of
		true ->
			"";
		false ->				%% receive M messages from N processes. Print message and senders PID every time
			receive
				{ Pid, Msg } ->
					io:format("Received msg: ~s from: ~p ~n~n", [Msg, Pid]),
					Pid ! { self(), Msg }
			end,
			operate(Reps-1)
	end
.

leaf(M) ->
	case ( M == 0 ) of
		true ->
			exit(0);
		false ->			%% receive message from root process M times, then terminate gracefully
			receive
				{ Root, Msg } ->
					Root ! { self(), Msg }
			end,
			leaf(M-1)
	end	
.

flush() ->				%% function to flush root process's mailbox
	receive
		_ -> 
			flush()
        	after
                0 -> ok
	end
.

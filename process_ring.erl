-module(process_ring).
-export([start/3,child_proc/3]).

start(M, N, Message) ->
	flush(),
	Pid = spawn(process_ring, child_proc, [M, N-2, self()]),	%% store PID of first process in ring which starts executing function child_proc
	Pid ! Message,				%% start message sending
	root_send(M, N-1, Pid),		%% special send function for root process which also prints logs (every process signalizes its PID after message sending)
	'END'
.

child_proc(M, N, Root) ->		%% N is two less than original N because we are already running the root process and we spawned this process
	case ( N == 0 ) of
		true -> 
			send(M, Root, Root);	%% last spawned process just sends message and doesn't spawn another process + its target process is root (to form a ring)
		false -> 
			send(M, spawn(process_ring, child_proc, [M, N-1, Root]), Root)	%% every new process starts sending and spawns its target
	end,
	exit(0)
.

send(M, Target, Root) ->
	case ( M == 0 ) of
		true -> 		%% if message went around the ring M-times, we do nothing
			"";
		false -> 
			receive					%% wait while message has been sent to the current process's PID
				Msg -> 
					Root ! self(), 			%% notify root process with current process's PID, that this particular process has sent the message
					Target ! Msg,				%% pass the message to the next process
					send(M-1, Target, Root) 			%% continue with recursive sending
			end
	end
.

root_send(M, N, Target) ->
	case ( M == 0 ) of
		true -> 
			"";			%% if message has been sent M times, do nothing
		false ->
			print_logs(N),			%% print PIDs of N processes which forwarded the message
			receive
				Msg -> 
					erlang:display(Msg),		%% print message itself
					Target ! Msg,					%% else forward the message to the target
					root_send(M-1, N, Target)		%% continue in recursion (printing logs and forwarding message)
			end
	end
.

print_logs(N) ->
	case ( N == 0 ) of
		true ->
			"";
		false ->
			receive
				Log -> erlang:display(Log),
					print_logs(N-1)
			end
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

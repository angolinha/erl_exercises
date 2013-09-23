-module(master_slaves).
-export([start/1, master/1, slave_listen/0, to_slave/2]).

start(N) ->
	flush(),
	register('output', self()),
	spawn(master_slaves, master, [N])
.

master(N) ->
	flush(), 						%% empty master's mailbox
	register('master', self()),		%% register first process as master
	List = create_slaves(N),		%% create N slave processes and obtain list od then in form {Pid, order_num}
	master_listen(N, List)				%% await messages and commands
.

create_slaves(N) ->
	case ( N == 0 ) of
		true ->
			List = {};									%% at the end of recursion List is empty and waits for additions
		false ->
			Pid = spawn(master_slaves, slave_listen, []),			%% save Pid of created slave process
			List = [ create_slaves(N-1) | { Pid, N } ]		%% form list of previously created slave processes and currently created process
	end,
	List													%% return list of all created slave processes
.

to_slave(N, Msg) ->
	master ! { "Msg", N, Msg },		%% send tuple to master which says: "This is message which should be sent to N-th slave."
	receive
		Msg ->
			io:format("~w ~n",[Msg])
	end
.

master_listen(M, List) ->
	receive
		{ Type, N, Msg } ->												%% await tuple of certain form
			case Type of
				"Msg" ->												%% if message should be forwarded
					case ( Msg == 'HALT') of
						true ->											%% if master received "HALT" message it sends "DIE" message to every slave and stops listening
							shutdown_slaves(List),
							output ! io_lib:format("Master process going down.~n");
						false ->										%% or else master forwards message to target slave process and continues listening
							Pid = find_pid(N, List), 
							case ( Pid == null ) of
								true ->
									output ! io_lib:format("~nDidn't find pair Pid to N ~w !~n~n", [N]);
								false ->
									Pid ! Msg
							end,
							master_listen(M, List)
					end;
				"Log" ->												%% if message should be printed
					output ! io_lib:format("Slave ~p got message ~w.~n", [N, Msg]),
					master_listen(M, List)
			end
	end
.

find_pid(N, List) ->
	case List of
		[] ->						%% if examined list is empty, we do nothing
			null;
		{ PID, M } ->				%% last examined list will be tuple, so we compare Pids and do proper action 
			case ( M == N ) of
				true ->
					PID;
				false ->
					null
			end;
		[ Head | Tail ] ->
			case Tail of						%% examine if head of the list is in {Pid, N} form
				{ Pid, I } ->
					case ( I == N ) of
						true -> 
							Pid;						%% if head is in correct form and order number equals to N, return process's Pid
						false ->
							find_pid(N, Head)			%% else continue with searching Tail
					end;
				_ ->
					find_pid(N, Head)			%% first list element is empty tuple, so if we find it, we skip it
			end
	end
.

shutdown_slaves(List) ->				%% recursively shutdown all slaves
	case List of
		{} ->
			null;
		[] ->
			null;
		{ PID, _ } ->
			PID ! "DIE";
		[ Head | Tail ] ->
			case Tail of
				{ Pid, _ } ->
					Pid ! "DIE";
				_ ->
					null
			end,
			shutdown_slaves(Head)
	end
.

slave_listen() ->
	receive
		Msg ->
			case ( Msg == "DIE" ) of
				true -> 								%% if message is "DIE", process terminates gracefully with its PID as exit message
					exit("Obtained DIE message.");
				false ->								%% or else it sends received message to master to be printed and continues listening
					master ! { "Log", self(), Msg },
					slave_listen()
			end
	end
.

flush() ->
	receive
		_ ->
			flush()
			after
				0 -> ok
	end
.

-module(master_slaves).
-export([start/1]).

start(N) ->
	flush(), 						%% empty master's mailbox
	register('master', self()),		%% register first process as master
	%%List = create_slaves(N),		%% create N slave processes and obtain list od then in form {Pid, order_num}
	create_slaves(N),
	master_listen(N)				%% await messages and commands
.

create_slaves(N) ->
	case ( N == 0 ) of
		true ->
%%			List = null;									%% at the end of recursion List is empty and waits for additions
			null;
		false ->
			Pid = spawn(master_slaves, slave, [N]),			%% save Pid of created slave process
%%			List = [ create_slaves(N-1) | { Pid, N } ],		%% form list of previously created slave processes and currently created process
			register('slave_'++N, Pid)
	end
%%	List													%% return list of all created slave processes
.

slave(N) ->									%% sending dummy messages (code which is executed by every newly created slave process)
	case N of
		1 -> 
			to_slave(3, 'Sprava 1.');
		2 -> 
			to_slave(4, 'Sprava 2.');
		3 -> 
			to_slave(1, 'Sprava 3.');
		4 -> 
			to_slave(2, 'HALT')
	end
.

to_slave(N, Msg) ->
	master ! { "Msg", N, Msg }		%% send tuple to master which says: "This is message which should be sent to N-th slave."
.

master_listen(M) ->
	receive
		{ Type, N, Msg } ->												%% await tuple of certain form
			case Type of
				"Msg" ->												%% if message should be forwarded
					case ( Msg == "HALT") of
						true ->											%% if master received "HALT" message it sends "DIE" message to every slave and stops listening
							shutdown_slaves(M),
							io:format("Master process going down."),
							null;
						false ->										%% or else master forwards message to target slave process and continues listening
							N ! Msg,
							master_listen(M)
					end;
				"Log" ->												%% if message should be printed
					io:format("Message: ~w from: ~p ~n", [Msg, N])
			end
	end
.

shutdown_slaves(N) ->				%% recursively shutdown all slaves
	case ( N == 0 ) of
		true ->
			null;
		false ->
			shutdown_slaves(N-1),
			to_slave(N, "DIE")
	end
.

slave_listen() ->
	receive
		Msg ->
			case ( Msg == "DIE" ) of
				true -> 								%% if message is "DIE", process terminates gracefully with its PID as exit message
					exit(self());
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

%% @author Martin Šustek <xsustek.martin@gmail.com>
%% @doc Master_slaves
%%
%% The module contains an example implementation of the Master/slaves exercise from [www.erlang.org/exercises]
%% @version 1.0a ({@version})
%% @copyright 2013 Martin Šustek

-module(master_slaves).
-author("Martin Sustek").
-date({2013,10,01}).
-version("1.0a").

-export([start/1, master/1, slave_listen/0, to_slave/2]).

-spec(start(integer()) -> pid()).
%% @doc Register current process as output and spawn master process
%%
%%
start(N) ->
	flush(),
	case whereis(output) of
		undefined ->
			nil;
		_ ->
			unregister(output)
	end,
	register(output, self()),
	spawn_link(master_slaves, master, [N]).

-spec(master(integer()) -> integer() | nil).
%% @doc Register process as master, spawn N slave processes and listen
%%
%%
master(N) ->
	flush(), 						%% empty master's mailbox
	case whereis(master) of
		undefined ->
			nil;
		_ ->
			unregister(master)
	end,
	register(master, self()),		%% register first process as master
	process_flag(trap_exit, true),	%% master process will trap exit messages
	List = create_slaves(N),		%% create N slave processes and obtain list od then in form {Pid, order_num}
	master_listen(List).			%% await messages and commands

-spec(create_slaves(integer()) -> list() | nil).
%% @doc Create N slave processes and return List of their Pids acompained with their order number
%%
%%
create_slaves(0) ->
	nil;
create_slaves(N) ->
	Pid = spawn_link(master_slaves, slave_listen, []),	%% save Pid of created slave process
	List = [ create_slaves(N-1) | { Pid, N } ],		%% form list of previously created slave processes and currently created process
	List.											%% return list of all created slave processes

%% @doc Send message Msg to master process and tell him to forward it to N-th slave process
%%
%%
to_slave(N, Msg) ->
	master ! { "Msg", N, Msg },		%% send tuple to master which says: "This is message which should be sent to N-th slave."
	receive
		{Pid, Msg} ->
			io:format("Slave ~p got message: ~p ~n",[Pid, Msg]);
		{Pid, "LOST"} ->
			io:format("Didn't find pair Pid to N: ~p ~n", [Pid]);
		{N, _} ->
			io:format("Recreated failed process ~p.~n", [N]);
		Ack ->
			io:format("~p ~n",[Ack])
	end.

%% @doc Listen to messages, sort them and perform proper action based on message type
%%
%%
master_listen(List) ->
	receive
		{"Msg", _, "HALT"} ->
			shutdown_slaves(List),
			output ! "Master process going down.",
			exit(0);
		{ "Msg", N, Msg } ->														%% await tuple of certain form
			Pid = find_pid(N, List),
			case ( Pid == nil ) of
				true ->
					output ! {N, "LOST"};
				false ->
					Pid ! Msg
			end,
			master_listen(List);
		{ "Log", N, Msg } ->														%% if message should be printed
			output ! {N, Msg},
			master_listen(List);
		{'EXIT', Pid, _} ->
			output ! {find_order_num(Pid, List), true},
			NewList = refresh_list(Pid, spawn_link(master_slaves, slave_listen, []), List, List, length(List)),
			master_listen(NewList)
	end.

find_pid(N, List) ->
	case List of
		{ PID, M } ->				%% last examined list will be tuple, so we compare Pids and do proper action
			case ( M == N ) of
				true ->
					PID;
				false ->
					nil
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
			end;
		_ ->
			nil
	end.

find_order_num(Pid, List) ->
	case List of
		{ PID, M } ->				%% last examined list will be tuple, so we compare Pids and do proper action
			case ( PID == Pid ) of
				true ->
					M;
				false ->
					nil
			end;
		[ Head | Tail ] ->
			case Tail of						%% examine if head of the list is in {Pid, N} form
				{ Pd, I } ->
					case ( Pd == Pid ) of
						true ->
							I;						%% if head is in correct form and order number equals to N, return process's Pid
						false ->
							find_order_num(Pid, Head)			%% else continue with searching Tail
					end;
				_ ->
					find_order_num(Pid, Head)			%% first list element is empty tuple, so if we find it, we skip it
			end;
		_ ->
			nil
	end.

refresh_list(Old, New, List, Original, Index) ->
	case List of
		{ PID, M } ->				%% last examined list will be tuple, so we compare Pids and do proper action
			case ( PID == Old ) of
				true ->
					lists:sublist(Original, Index-1) ++	{New, M} ++ lists:nthtail(Index,Original);
				false ->
					nil
			end;
		[ Head | Tail ] ->
			case Tail of						%% examine if head of the list is in {Pid, N} form
				{ Pid, I } ->
					case ( Pid == Old ) of
						true ->	%% if head is in correct form and order number equals to N, return process's Pid
							lists:sublist(Original, Index-1) ++ {New, I} ++ lists:nthtail(Index,Original);
						false ->
							refresh_list(Old, New, Head, Original, Index-1)			%% else continue with searching Tail
					end;
				_ ->
					refresh_list(Old, New, Head, Original, Index-1)			%% first list element is empty tuple, so if we find it, we skip it
			end;
		_ ->
			nil
	end.

shutdown_slaves(List) ->				%% recursively shutdown all slaves
	case List of
		{ PID, _ } ->
			PID ! "DIE";
		[ Head | Tail ] ->
			case Tail of
				{ Pid, _ } ->
					Pid ! "DIE";
				_ ->
					nil
			end,
			shutdown_slaves(Head);
		_ ->
			nil
	end.

slave_listen() ->
	receive
		"DIE"->
			exit(normal);
		Msg ->
			master ! { "Log", self(), Msg },
			slave_listen()
	end.

flush() ->
	receive
		_ ->
			flush()
			after
				0 -> ok
	end.
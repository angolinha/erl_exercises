%% @author Martin Šustek <xsustek.martin@gmail.com>
%% @doc Master_slaves
%%
%% The module contains an example implementation of the Master/slaves exercise from [www.erlang.org/exercises]
%% @version 1.0b ({@version})
%% @copyright 2013 Martin Šustek

-module(master_slaves).
-author("Martin Sustek").
-date({2013,10,01}).
-version("1.0b").

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
	flush(),
	case whereis(master) of
		undefined ->
			nil;
		_ ->
			unregister(master)
	end,
	register(master, self()),
	process_flag(trap_exit, true),
	List = create_slaves(N),
	master_listen(List).

-spec(create_slaves(integer()) -> list()).
%% @doc Create N slave processes and return List of their Pids acompained with their order number
%%
%%
create_slaves(0) ->
	[];
create_slaves(N) ->
	Pid = spawn_link(master_slaves, slave_listen, []),
	List = lists:append(create_slaves(N-1), [{ Pid, N }]),
	List.

-spec(to_slave(integer(), list()) -> list()).
%% @doc Send message Msg to master process and tell him to forward it to N-th slave process. Then wait for proper reaction.
%%
%%
to_slave(N, Msg) ->
	master ! { "Msg", N, Msg },
	receive
		{Pid, Msg} ->
			io:format("Slave ~p got message: ~p ~n",[Pid, Msg]);
		{Pid, "LOST"} ->
			io:format("Didn't find pair Pid to N: ~p ~n", [Pid]);
		{_, N} ->
			io:format("Recreated failed process ~p.~n", [N]);
		Ack ->
			io:format("~p ~n",[Ack])
	end.

-spec(master_listen(list()) -> list()).
%% @doc Listen to messages, sort them and perform proper action based on message type
%%
%%
master_listen(List) ->
	receive
		{"Msg", _, "HALT"} ->
			shutdown_slaves(List),
			output ! "Master process going down.",
			exit(0);
		{ "Msg", N, Msg } ->
			case lists:keyfind(N, 2, List) of
				false ->
					output ! {N, "LOST"};
				{Pid, _} ->
					Pid ! Msg
			end,
			master_listen(List);
		{ "Log", N, Msg } ->
			output ! {N, Msg},
			master_listen(List);
		{'EXIT', Pid, _} ->
			N = lists:keyfind(Pid, 1, List),
			output ! N,
			master_listen(lists:keyreplace(Pid, 1, List, {spawn_link(master_slaves, slave_listen, []), N}))
	end.

-spec(shutdown_slaves(list()) -> nil).
%% @doc Recursively shutdown all slaves
%%
%%
shutdown_slaves(List) ->
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

-spec(slave_listen() -> nil).
%% @doc Listen to messages. If message is DIE, terminate gracefully. Otherwise send ack to master process.
%%
%%
slave_listen() ->
	receive
		"DIE"->
			exit(normal);
		Msg ->
			master ! { "Log", self(), Msg },
			slave_listen()
	end.

-spec(flush() -> nil).
%% @doc Empty calling process's mailbox recursively.
%%
%%
flush() ->
	receive
		_ ->
			flush()
			after
				0 -> ok
	end.
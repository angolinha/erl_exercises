-module(synchMess).
-export([send_mes/1,p1/2,p2/3]).

send_mes(M) ->
	Pid1 = spawn(synchMess,p1,[self(), M]),
	spawn(synchMess,p2,[Pid1, self(), M]),
	listen(M*2).

listen(Reps) ->
	case (Reps == 0) of
		true -> "";
		false -> listen(Reps-1),
			receive
				Msg  -> erlang:display(Msg)
			end
	end.

p1(Mother, Reps) ->
	receive
		Pid -> Brother = Pid
	end,
	p1_rec(Mother, Brother, Reps, "Sprava od 1. procesu"),
	exit(0).

p1_rec(Mother, Brother, Reps, Msg) ->
	case (Reps == 0) of
		true -> "";
		false -> p1_rec(Mother, Brother, Reps-1, Msg),
			receiver(Mother, Brother),
			sender(Brother, Msg)
	end.

p2(Brother, Mother, Reps) ->
	Brother ! self(),
	p2_rec(Mother, Brother, Reps, "Sprava od 2. procesu"),
	exit(0).

p2_rec(Mother, Brother, Reps, Msg) ->
	case (Reps == 0) of
		true -> "";
		false -> p2_rec(Mother, Brother, Reps-1, Msg),
			sender(Brother, Msg),
			receiver(Mother, Brother)
	end.

sender(Brother, Msg) ->
	Brother ! Msg,
	receive
		true -> ""
	end.

receiver(Mother, Brother) ->
	receive
		Msg -> Mother ! Msg
	end,
	Brother ! true.

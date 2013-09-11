-module(lists1).
-export([min_max/1]).

min_max(List) ->
	{min_(List), max_(List)}.

min_(List) ->
	case List of
		[] -> empty;
		[_ | Tail] -> case min_(Tail) of
			empty -> hd(List);
			_ -> case ( hd(List) < min_(Tail)) of
				true -> hd(List);
				false -> min_(Tail)
			end 
		end
	end
.

max_(List) ->
	case List of
		[] -> empty;
		[_ | Tail] -> case max_(Tail) of
			empty -> hd(List);
			_ -> case ( hd(List) > max_(Tail)) of
				true -> hd(List);
				false -> max_(Tail)
			end 
		end
	end
.

%% @author Aurel Paulovic <paulovic@fiit.stuba.sk> [fiit.stuba.sk/~paulovic]
%% @doc Set
%%
%% The module contains an example implementation of the 1st assignment for the course Distributed program systems
%% @version 1.0b ({@version})
%% @copyright 2013 Aurel Paulovic

-module(set).
-author("Aurel Paulovic").
-date({2013,03,10}).
-version("1.0b").

-export([newSet/0,toSet/1,insert/2,delete/2,prec/2,succ/2,show/1,intersect/2,union/2,
	 diff/2, equals/2,max/1,min/1,map/2,foldl/3,filter/2,card/1,isin/2,all/2,
	 any/2,product/3]).

%% specific data type
-record(set, {
	  items = [] :: [integer()]
}).
%% variable type defined for pattern matching in function parameter section
-type myset() :: #set{items :: [integer()]}.

-spec(newSet() -> myset()).
%% @doc Create an empty set
newSet() ->
    #set{}.

-spec(toSet([integer()]) -> myset()).
%% @doc Create a set contaning the elements from List
%%
%% The resulting set is valid - an ordered set of unique integers
toSet(List) when is_list(List) ->
    toSet(List,[]).

-spec(toSet([integer()],[integer()]) -> myset()).
%% @doc Traverse the items in list and put the valid items in the resulting accumulator
%% @private
toSet([],Items) ->
    #set{items = Items};
toSet([H|T],Items) ->
    toSet(T,p_ensure(Items,H)).

-spec(insert(myset(),integer()) -> myset()).
%% @doc Insert an integer E into set
%%
%% Silently discards non-unique E
insert(#set{items = Items},E) when is_integer(E), is_list(Items) ->
    #set{items = p_ensure(Items,E)}.

-spec(p_ensure([integer()],integer()) -> [integer()]).
%% @doc Ensure that the integer E is in the ordered List
%%
%% If E is not in the List, it will be inserted at the proper place
%% @private
p_ensure(List,E) when not is_integer(E) ->
    List;
p_ensure([],E) ->
    [E];
p_ensure([E|_] = List,E) ->
    List;
p_ensure([L|T],E) when L < E ->
    [L | p_ensure(T,E)];
p_ensure([_H|_] = Rest,E) -> % implicit E < _H
    [E | Rest].

-spec(delete(myset(),integer()) -> myset()).
%% @doc Delete the integer E from set
delete(#set{items = Items},E) when is_integer(E), is_list(Items) ->
    #set{items = p_remove(Items,E)}. % we could use '--' operator, but we have ordered list so this might be actually more efficient (depends on the speed of C vs elrang bytecode)

-spec(p_remove([integer()],integer()) -> [integer()]).
%% @doc Remove the integer E from set
%%
%% If E is not in the list, return the original list
%% @private
p_remove([],_E) ->
    [];
p_remove([E|T],E) ->
    T;
p_remove([L|T],E) when L < E ->
    [L | p_remove(T,E)];
p_remove([_H|_T] = Rest,_E) -> % implicit _E < _H
    Rest.

-spec(prec(myset(),integer()) -> integer() | nil).
%% @doc Return the element preceeding the E element
%%
%% If there is no such element (E is not present in the set or E is the first element) returns the atom nil.
prec(#set{items = Items},E) when is_integer(E), is_list(Items) ->
    prec(Items,E,nil).
%% @todo doc
prec([],_E,_Ret) ->
    nil;
prec([E|_T],E,Ret) ->
    Ret;
prec([H|T],E,_Ret) -> % TODO skoncit skor
    prec(T,E,H).

-spec(succ(myset(),integer()) -> integer() | nil).
%% @doc Return the successor of the element E
%%
%% If there is no such element (E is not present in the set or E is the last element) return the atom nil.
succ(#set{items = Items},E) when is_integer(E), is_list(Items) ->
    p_succ(Items,E).

-spec(p_succ([integer()],integer()) -> integer() | nil).
%% @doc Return the successor of the element E
%% @private
p_succ([],_E) ->
    nil;
p_succ([E|[]],E) ->
    nil;
p_succ([E,N|_T],E) ->
    N;
p_succ([L|T],E) when L < E ->
    p_succ(T,E);
p_succ([_H|_T],_E) -> % implicit H > E
    nil.

-spec(show(myset()) -> ok).
%% @doc Print out the elements of the list to the console
show(#set{items = Items}) when is_list(Items) ->
    io:format("Set contains: ~w~n",[Items]),
    ok.

-spec(union(Set1 :: myset(),Set2 :: myset()) -> myset()).
%% @doc Create a set that is an union of the two sets
union(#set{items = Items1},#set{items = Items2}) when is_list(Items1), is_list(Items2) ->
    #set{items = merge(Items1,Items2)}.

-spec(merge(List1 :: [integer()],List2 ::[integer()]) -> [integer()]).
%% @doc Union two lists of unique ordered integers
%%
%% The resulting list is ordered and contains only unique integers
%% @private
merge([],Items2) ->
    Items2;
merge(Items1,[]) ->
    Items1;
merge([H|T1],[H|T2]) ->
    [H | merge(T1,T2)];
merge([L|T1],[H|_T2] = Items2) when L < H ->
    [L | merge(T1,Items2)];
merge([_H|_T1] = Items1,[L|T2]) -> % implicit L < H
    [L | merge(Items1,T2)].

-spec(diff(Set1 :: myset(),Set2 :: myset()) -> myset()).
%% @doc Create set that is a difference of the two sets (Set1 - Set2)
diff(#set{items = Items1},#set{items = Items2}) when is_list(Items1), is_list(Items2) ->
    #set{items = p_diff(Items1,Items2)}.

-spec(p_diff([List1 :: integer()],[List2 :: integer()]) -> [integer()]).
%% @doc Create difference of the two lists
%%
%% Performs the equivalent of List1 -- List2 for ordered lists with unique elements
%% @private
p_diff([],_Items2) ->
    [];
p_diff(Items1,[]) ->
    Items1;
p_diff([H|T1],[H|T2]) ->
    p_diff(T1,T2);
p_diff([L|T1],[H|_T2] = Items2) when L < H ->
    [L | p_diff(T1,Items2)];
p_diff([_H|_T1] = Items1,[_L|T2]) ->
    p_diff(Items1,T2).

-spec(intersect(Set1 :: myset(),Set2 :: myset()) -> myset()).
%% @doc Create a set that is the intersect of the two sets
intersect(#set{items = Items1},#set{items = Items2}) when is_list(Items1), is_list(Items2) ->
    #set{items = p_intersect(Items1,Items2)}.

-spec(p_intersect(List1 :: [integer()],List2 :: [integer()]) -> [integer()]).
%% @doc Create ordered list of unique integers that appear both in List1 and List2
%% @private
p_intersect([],_Items2) ->
    [];
p_intersect(_Items1,[]) ->
    [];
p_intersect([H|T1],[H|T2]) ->
    [H | p_intersect(T1,T2)];
p_intersect([L|T1],[H|_T2] = Items2) when L < H ->
    p_intersect(T1,Items2);
p_intersect([_H|_T1] = Items1,[_L|T2]) ->
    p_intersect(Items1,T2).

-spec(equals(Set1 :: myset(),Set2 :: myset()) -> boolean()).
%% @doc Return true if Set1 and Set2 are equal, otherwise false
equals(#set{items = Items},#set{items = Items}) when is_list(Items) ->
    true;
equals(#set{items = Items1},#set{items = Items2}) when is_list(Items1), is_list(Items2) ->
    false.

-spec(min(myset()) -> integer() | nil).
%% @doc Return the minimum element of the set
%%
%% If there is no minimum (empty set), return nil
min(#set{items = Items}) when is_list(Items) ->
    case Items of
	[] ->
	    nil;
	[H|_T] ->
	    H
    end.

-spec(max(myset()) -> integer() | nil).
%% @doc Return the maximum element of the set
%%
%% If there is no maximum (empty set), return nil
max(#set{items = Items}) when is_list(Items) ->
    p_max(Items).

-spec(p_max([integer()]) -> integer() | nil).
%% @doc Return the last element of the list
%% @todo Could be merged into max
%% @private
p_max([H|[]]) ->
    H;
p_max([_H|T]) ->
    p_max(T);
p_max([]) ->
    nil.

-spec(map(myset(),function()) -> [any()]).
%% @doc Create list that contains all the elements of set after appplying the funnction Fun to each one
map(#set{items = Items},Fun) when is_list(Items), is_function(Fun,1) ->
    % toSet([Fun(X) || X <- Items]). % if we want to get proper set
    [Fun(X) || X <- Items]. % if we want to get list

-spec(foldl(myset(),function(),any()) -> any()).
%% @doc Fold left the elements of the set using function Fun with the starting value of Acc0
foldl(#set{items = Items},Fun,Acc0) when is_list(Items), is_function(Fun,2) ->
    p_foldl(Items,Fun,Acc0).

-spec(p_foldl(List :: [integer()],function(),any()) -> any()).
%% @doc Fold left the List using function Fun
%% @private
%% @todo could be merged with foldl
p_foldl([],_Fun,Acc0) ->
    Acc0;
p_foldl([H|T],Fun,Acc0) ->
    p_foldl(T,Fun,Fun(H,Acc0)).

-spec(filter(myset(),function()) -> myset()).
%% @doc Filters the elements of the Set using predicate Pred
filter(#set{items = Items},Pred) when is_list(Items), is_function(Pred,1) ->
    #set{items = [X || X <- Items, Pred(X)]}.

-spec(card(myset()) -> integer()).
%% @doc Return the number of elements in the set
card(#set{items = Items}) when is_list(Items) ->
    card(Items,0).

-spec(card([integer()],integer()) -> integer()).
%% @doc Return the number of elements in the list
%% @private
card([],Acc) ->
    Acc;
card([_H|T],Acc) ->
    card(T,Acc + 1).

-spec(isin(myset(),integer()) -> boolean()).
%% @doc Test whether the element E is present in the Set
isin(#set{items = Items},E) when is_list(Items), is_integer(E) ->
    p_isin(Items,E).

-spec(p_isin([integer()],integer()) -> boolean()).
%% @doc Test whether the element E is in the List
%% @private
p_isin([E|_T],E) ->
    true;
p_isin([L|T],E) when L < E ->
    p_isin(T,E);
p_isin(_List,_E) ->
    false.

-spec(all(myset(),function()) -> boolean()).
%% @doc Test whether all elements return true for the predicate Fun
all(#set{items = Items},Fun) when is_list(Items), is_function(Fun,1) ->
    p_all(Items,Fun).

-spec(p_all([integer()],function()) -> boolean()).
%% @doc Test whether all elements return true for the predicate Fun
%% @private
p_all([],_Fun) ->
    true;
p_all([H|T],Fun) ->
    case Fun(H) of
	true ->
	    p_all(T,Fun);
	_ ->
	    false
    end.

-spec(any(myset(),function()) -> boolean()).
%% @doc Test whether at least one of the elements in the Set returns true for the predicate Fun
any(#set{items = Items},Fun) when is_list(Items), is_function(Fun,1) ->
    p_any(Items,Fun).

-spec(p_any([integer()],function()) -> boolean()).
%% @doc Test whether at least one of the elements in the List returns true fro the predicate Fun
%% @private
p_any([],_Fun) ->
    false;
p_any([H|T],Fun) ->
    case Fun(H) of
	true ->
	    true;
	_  ->
	    p_any(T,Fun)
    end.

-spec(product(Set1 :: myset(), Set2 :: myset(), function()) -> myset()).
%% @doc Returns the resulting set which elements are the cross-product of the elements of Set1 and Set2 after applying the function Fun
product(#set{items = Items1},#set{items = Items2},Fun) when is_list(Items1), is_list(Items2), is_function(Fun,2) ->
    toSet([Fun(X,Y) || X <- Items1, Y <- Items2]).


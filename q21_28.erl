-module(q21_28).

-export([
		 insertAt/3,
		 range/2,
		 rnd_select/2,
		 diff_select/2,
		 rnd_permu/1,
		 combinations/2,
		 group/2,
		 lsort/1,
		 lfsort/1
		]).

%% 1 Problem 21
%% Insert an element at a given position into a list.
%% 
%% Example:
%% 
%% * (insert-at 'alfa '(a b c d) 2)
%% (A ALFA B C D)
%% Example in Haskell:
%% 
%% P21> insertAt 'X' "abcd" 2
%% "aXbcd"
insertAt(El, List, Pos) ->
	insertAt(El, List, Pos-1, []).

insertAt(El, [], _, Res) ->
	lists:reverse(Res) ++ El;
insertAt(El, Tail, 0, Res) ->
	lists:reverse(Res) ++ El ++ Tail;
insertAt(El, [H|Tail], Pos, Res) ->
	insertAt(El, Tail, Pos-1, [H|Res]).

%% 2 Problem 22
%% Create a list containing all integers within a given range.
%% 
%% Example:
%% 
%% * (range 4 9)
%% (4 5 6 7 8 9)
%% Example in Haskell:
%% 
%% Prelude> range 4 9
%% [4,5,6,7,8,9]
range(Start, End) ->
	range(Start, End, []).

range(Start, Start, Range) ->
	Range;
range(Start, Current, Range) ->
	range(Start, Current-1, [Current|Range]).

%% 3 Problem 23
%% Extract a given number of randomly selected elements from a list.
%% 
%% Example:
%% 
%% * (rnd-select '(a b c d e f g h) 3)
%% (E D A)
%% Example in Haskell:
%% 
%% Prelude System.Random>rnd_select "abcdefgh" 3 >>= putStrLn
%% eda
rnd_select(List, Amount) ->
	rnd_select(List, Amount, length(List), []).

rnd_select(_, 0, _, Result) ->
	Result;
rnd_select(List, Amount, Length, Result) ->
	Current = lists:nth(floor(rand:uniform() * Length) + 1, List),
	rnd_select(lists:filter(fun(X) -> X =/= Current end, List), Amount-1, Length-1, [Current|Result]). 

%% 4 Problem 24
%% Lotto: Draw N different random numbers from the set 1..M.
%% 
%% Example:
%% 
%% * (rnd-select 6 49)
%% (23 1 17 33 21 37)
%% Example in Haskell:
%% 
%% Prelude System.Random>diff_select 6 49
%% Prelude System.Random>[23,1,17,33,21,37]
diff_select(Amount, End) ->
	rnd_select(range(1, End), Amount).

%% 5 Problem 25
%% Generate a random permutation of the elements of a list.
%% 
%% Example:
%% 
%% * (rnd-permu '(a b c d e f))
%% (B A D C E F)
%% Example in Haskell:
%% 
%% Prelude System.Random>rnd_permu "abcdef"
%% Prelude System.Random>"badcef"
rnd_permu(List) ->
	rnd_select(List, length(List)).

%% 6 Problem 26
%% (**) Generate the combinations of K distinct objects chosen from the N elements of a list
%% 
%% In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For pure mathematicians, this result may be great. But we want to really generate all the possibilities in a list.
%% 
%% Example:
%% 
%% * (combinations 3 '(a b c d e f))
%% ((A B C) (A B D) (A B E) ... )
%% Example in Haskell:
%% 
%% > combinations 3 "abcdef"
%% ["abc","abd","abe",...]
combinations(N, List) ->
	combinations(N, List, 0, []).

combinations(N, _, N, Result) ->
	%% Reverse the list to prettify the result, no effect anyhow.
	[lists:reverse(Result)];
combinations(_, [], _, _) ->
	[];
combinations(N, [H|Rest], Acc, Result) ->
	combinations(N, Rest, Acc+1, [H|Result]) ++ combinations(N, Rest, Acc, Result).
	
%% 7 Problem 27
%% Group the elements of a set into disjoint subsets.
%% 
%% a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities and returns them in a list.
%% 
%% Example:
%% 
%% * (group3 '(aldo beat carla david evi flip gary hugo ida))
%% ( ( (ALDO BEAT) (CARLA DAVID EVI) (FLIP GARY HUGO IDA) )
%% ... )
%% b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.
%% 
%% Example:
%% 
%% * (group '(aldo beat carla david evi flip gary hugo ida) '(2 2 5))
%% ( ( (ALDO BEAT) (CARLA DAVID) (EVI FLIP GARY HUGO IDA) )
%% ... )
%% Note that we do not want permutations of the group members; i.e. ((ALDO BEAT) ...) is the same solution as ((BEAT ALDO) ...). However, we make a difference between ((ALDO BEAT) (CARLA DAVID) ...) and ((CARLA DAVID) (ALDO BEAT) ...).
%% 
%% You may find more about this combinatorial problem in a good book on discrete mathematics under the term "multinomial coefficients".
%% 
%% Example in Haskell:
%% 
%% P27> group [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
%% [[["aldo","beat"],["carla","david","evi"],["flip","gary","hugo","ida"]],...]
%% (altogether 1260 solutions)
%%  
%% 27> group [2,2,5] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
%% [[["aldo","beat"],["carla","david"],["evi","flip","gary","hugo","ida"]],...]
%% (altogether 756 solutions)
%%
%% Implementing only part b) as the problem is a superset of part a).
%% simply call group([2,3,4], Items) to solve part a).
group(Counts = [Count|Rest], Items) ->
	L = group(Counts, Items, []),
	unflatten(L, Count, Rest, Counts, [], [], []).

unflatten([], _, _, _, _, _, Result) ->
	Result;
unflatten([H|T], 0, [], Counts = [Count|Rest], L1, L2, L3) ->
	L2_ = [L1|L2],
	unflatten([H|T], Count, Rest, Counts, [], [], [lists:reverse(L2_)|L3]);
unflatten([H|T], 0, [Count|Rest], Counts, L1, L2, L3) ->
	unflatten([H|T], Count, Rest, Counts, [], [L1|L2], L3);
unflatten([H|T], Count, Rest, Counts, L1, L2, L3) ->
	unflatten(T, Count-1, Rest, Counts, [H|L1], L2, L3).

group([], [], Result) ->
	[Result];
group([Count], Items, Result) ->
	Combs = combinations(Count, Items),
	lists:map(fun(Comb) -> 
					  group([], lists:filter(fun(Item) -> 
														 not lists:member(Item, Comb)
												 end, Items), [Comb|Result])
			  end, Combs);
group([Count|Counts], Items, Result) ->
	Combs = combinations(Count, Items),
	lists:flatten(lists:map(fun(Comb) -> 
					  group(Counts, lists:filter(fun(Item) -> 
														 not lists:member(Item, Comb)
												 end, Items), [Comb|Result])
			  end, Combs)).

%% 8 Problem 28
%% Sorting a list of lists according to length of sublists
%% 
%% a) We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of this list according to their length. E.g. short lists first, longer lists later, or vice versa.
%% 
%% Example:
%% 
%% * (lsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
%% ((O) (D E) (D E) (M N) (A B C) (F G H) (I J K L))
%% Example in Haskell:
%% 
%% Prelude>lsort ["abc","de","fgh","de","ijkl","mn","o"]
%% Prelude>["o","de","de","mn","abc","fgh","ijkl"]
%% b) Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the elements of this list according to their length frequency; i.e., in the default, where sorting is done ascendingly, lists with rare lengths are placed first, others with a more frequent length come later.
%% 
%% Example:
%% 
%% * (lfsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
%% ((i j k l) (o) (a b c) (f g h) (d e) (d e) (m n))
%% Example in Haskell:
%% 
%% lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
%% ["ijkl","o","abc","fgh","de","de","mn"]
lsort([]) ->
	[];
lsort([{L, _}]) ->
	[L];
lsort([{L, Length}|Rest]) ->
	lsort([Item || Item = {_, Length1} <- Rest,
				   Length1 < Length]) 
		++ 
		[L]
		++
		lsort([Item || Item = {_, Length2} <- Rest,
					   Length2 >= Length]);				   
lsort(L) ->
	lsort([{Item, length(Item)} || Item <- L]).

lfsort(L) ->
	lfsort(L, L, #{}).

lfsort([], L, Frequencies) ->
	lsort(lists:map(fun(Item) ->
							Length = length(Item),
							#{Length := Frequency} = Frequencies,
							{Item, Frequency}
					end, L));
lfsort([H|T], L, Frequencies) ->
	Length = length(H),
	case maps:is_key(Length, Frequencies) of
		true ->
			#{Length := Frequency} = Frequencies,
			lfsort(T, L, Frequencies#{Length => Frequency + 1});
		false ->
			lfsort(T, L, Frequencies#{Length => 1})
	end.


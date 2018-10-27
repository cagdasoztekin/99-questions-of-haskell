-module(q11_20).

-export([encodeModified/1,
		 decodeModified/1,
		 decodeModified_/1,
		 encodeDirect/1,
		 dupli/1,
		 repli/2,
		 drop/2,
		 split/2,
		 slice/3,
		 rotate/2,
		 removeAt/2]).


%% 1 Problem 11
%% (*) Modified run-length encoding.
%% 
%% Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.
%% 
%% Example:
%% 
%% * (encode-modified '(a a a a b c c a a d e e e e))
%% ((4 A) B (2 C) (2 A) D (4 E))
%% Example in Haskell:
%% 
%% P11> encodeModified "aaaabccaadeeee"
%% [Multiple 4 'a',Single 'b',Multiple 2 'c',
%%  Multiple 2 'a',Single 'd',Multiple 4 'e']
encodeModified([H]) ->
	[ta(H)];
encodeModified([H1, H2 | L]) ->
	lists:reverse(encodeModified(H2, 1, H1, L, [])).

encodeModified(H, Count, H, [], Res) ->
	[{Count+1, ta(H)} | Res];
encodeModified(I2, 1, I1, [], Res) ->
	[ta(I2), ta(I1) | Res];
encodeModified(I2, Count, I1, [], Res) ->
	[ta(I2), {Count, ta(I1)} | Res]; 
encodeModified(H, Count, H, [HNew|T], Res) ->
	encodeModified(HNew, Count+1, H, T, Res);												 
encodeModified(H1, 1, H2, [HNew|T], Res) ->
	encodeModified(HNew, 1, H1, T, [ta(H2) | Res]);
encodeModified(H1, Count, H2, [HNew|T], Res) ->
	encodeModified(HNew, 1, H1, T, [{Count, ta(H2)} | Res]).


%% 2 Problem 12
%% (**) Decode a run-length encoded list.
%% 
%% Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.
%% 
%% Example in Haskell:
%% 
%% P12> decodeModified 
%%        [Multiple 4 'a',Single 'b',Multiple 2 'c',
%%         Multiple 2 'a',Single 'd',Multiple 4 'e']
%% "aaaabccaadeeee"

%% More convenient version first, where the input is a list of tuples
%% of the form {multiple, Num, Char} or {single, Char}.
decodeModified(Qs) ->
	decodeModified(Qs, "").

decodeModified([], Res) ->
	lists:flatten(lists:reverse(Res));
decodeModified([{multiple, Num, Char} | Qs], Res) ->
	decodeModified(Qs, [Char || _ <- lists:seq(1, Num)] ++ Res);
decodeModified([{single, Char} | Qs], Res) ->
	decodeModified(Qs, [Char | Res]).


%% Less convenient version
decodeModified_(Qs) ->
	lists:flatten(decodeModified_(Qs, [], "")).

decodeModified_([], [Char, Num, multiple], Res) ->
	lists:reverse([Char || _ <- lists:seq(1, Num)] ++ Res);
decodeModified_([], [Char, single], Res) ->
	lists:reverse([Char | Res]);
decodeModified_([H|T], [Char, Num, multiple], Res) ->
	decodeModified_(T, [H], [Char || _ <- lists:seq(1, Num)] ++ Res);
decodeModified_([H|T], [Char, single], Res) ->
	decodeModified_(T, [H], [Char | Res]);
decodeModified_([H|T], Q, Res) ->
	decodeModified_(T, [H|Q], Res).

%% 3 Problem 13
%% (**) Run-length encoding of a list (direct solution).
%% 
%% Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.
%% 
%% Example:
%% 
%% * (encode-direct '(a a a a b c c a a d e e e e))
%% ((4 A) B (2 C) (2 A) D (4 E))
%% Example in Haskell:
%% 
%% P13> encodeDirect "aaaabccaadeeee"
%% [Multiple 4 'a',Single 'b',Multiple 2 'c',
%%  Multiple 2 'a',Single 'd',Multiple 4 'e']

encodeDirect([Cur]) ->
	[single, ta(Cur)];
encodeDirect([Prev, Cur | T]) ->
	lists:flatten(lists:reverse(encodeDirect(Cur, Prev, 1, T, []))).

encodeDirect(Cur, Cur, Count, T, Res) ->
	case T of
		[] ->
			[[multiple, Count+1, ta(Cur)] | Res];
		[Next|Rest] ->
			encodeDirect(Cur, Next, Count+1, Rest, Res)
	end;
encodeDirect(Prev, Cur, Count, T, Res) ->
	case T of
		[] ->
			case Count of
				1 ->
					[[single, ta(Cur)], [single, ta(Prev)] | Res];
				Count ->
					[[single, ta(Cur)], [multiple, Count, ta(Prev)] | Res]
			end;
		[Next|Rest] ->
			case Count of
				1 ->
					encodeDirect(Cur, Next, 1, Rest, [[single, ta(Prev)] | Res]);
				Count ->
					encodeDirect(Cur, Next, 1, Rest, [[multiple, Count, ta(Prev)] | Res])
			end
	end.

%% 4 Problem 14
%% (*) Duplicate the elements of a list.
%% 
%% Example:
%% 
%% * (dupli '(a b c c d))
%% (A A B B C C C C D D)
%% Example in Haskell:
%% 
%% > dupli [1, 2, 3]
%% [1,1,2,2,3,3]

dupli(L) ->
	dupli(lists:reverse(L), []).

dupli([], Res) ->
	Res;
dupli([H|T], Res) ->
	dupli(T, [H, H | Res]).

%% 5 Problem 15
%% (**) Replicate the elements of a list a given number of times.
%% 
%% Example:
%% 
%% * (repli '(a b c) 3)
%% (A A A B B B C C C)
%% Example in Haskell:
%% 
%% > repli "abc" 3
%% "aaabbbccc"

repli(L, K) ->
	repli(lists:reverse(L), K, []).

repli([], _, Res) ->
	Res;
repli([H|T], K, Res) ->
	repli(T, K, repli({append, H}, K, Res));
repli({append, _}, 0, Res) ->
	Res;
repli({append, H}, K, Res) ->
	repli({append, H}, K-1, [H|Res]).

%% 6 Problem 16
%% (**) Drop every N'th element from a list.
%% 
%% Example:
%% 
%% * (drop '(a b c d e f g h i k) 3)
%% (A B D E G H K)
%% Example in Haskell:
%% 
%% *Main> dropEvery "abcdefghik" 3
%% "abdeghk"

drop(L, N) ->
	lists:reverse(drop(L, N, N-1, [])).

drop([], _, _, Res) ->
	Res;
drop([_|T], N, 0, Res) ->
	drop(T, N, N-1, Res);	 
drop([H|T], N, Acc, Res) ->
	drop(T, N, Acc-1, [H|Res]).

%% 7 Problem 17
%% (*) Split a list into two parts; the length of the first part is given.
%% 
%% Do not use any predefined predicates.
%% 
%% Example:
%% 
%% * (split '(a b c d e f g h i k) 3)
%% ( (A B C) (D E F G H I K))
%% Example in Haskell:
%% 
%% *Main> split "abcdefghik" 3
%% ("abc", "defghik")

split(L, N) ->
	split(L, N, []).

split(L1, 0, L2) ->
	[lists:reverse(L2), L1];
split([H|L1], N, L2) ->
	split(L1, N-1, [H|L2]).

%% 8 Problem 18
%% (**) Extract a slice from a list.
%% 
%% Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 1.
%% 
%% Example:
%% 
%% * (slice '(a b c d e f g h i k) 3 7)
%% (C D E F G)
%% Example in Haskell:
%% 
%% *Main> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
%% "cdefg"

slice(L, I1, I2) ->
	slice(L, I1-1, I2-I1+1, []).

slice(_, 0, 0, Res) ->
	lists:reverse(Res);
slice([H|T], 0, I2, Res) ->
	slice(T, 0, I2-1, [H|Res]);
slice([_|T], I1, I2, Res) ->
	slice(T, I1-1, I2, Res).

%% 9 Problem 19
%% (**) Rotate a list N places to the left.
%% 
%% Hint: Use the predefined functions length and (++).
%% 
%% Examples:
%% 
%% * (rotate '(a b c d e f g h) 3)
%% (D E F G H A B C)
%% 
%% * (rotate '(a b c d e f g h) -2)
%% (G H A B C D E F)
%% Examples in Haskell:
%% 
%% *Main> rotate ['a','b','c','d','e','f','g','h'] 3
%% "defghabc"
%%  
%% *Main> rotate ['a','b','c','d','e','f','g','h'] (-2)
%% "ghabcdef"

rotate(L, Acc) ->
	case Acc > 0 of
		true ->
			rotate(p, L, Acc);
		false ->
			rotate(n, lists:reverse(L), -Acc, [])
	end.

rotate(p, Res, 0) ->
	Res;
rotate(p, [H|T], Acc) ->
	rotate(p, T++[H], Acc-1).

rotate(n, L, 0, R) ->
	R ++ lists:reverse(L);
rotate(n, [H|T], Acc, R) ->
	rotate(n, T, Acc-1, [H|R]).

%% 10 Problem 20
%% (*) Remove the K'th element from a list.
%% 
%% Example in Prolog:
%% 
%% ?- remove_at(X,[a,b,c,d],2,R).
%% X = b
%% R = [a,c,d]
%% Example in Lisp:
%% 
%% * (remove-at '(a b c d) 2)
%% (A C D)
%% (Note that this only returns the residue list, while the Prolog version also returns the deleted element.)
%% 
%% Example in Haskell:
%% 
%% *Main> removeAt 2 "abcd"
%% ('b',"acd")

removeAt(L, Pos) ->
	removeAt(Pos-1, L, "", []).

removeAt(-1, [], C, Res) ->
	{[C], lists:reverse(Res)};
removeAt(0, [H|T], _, Res) ->
	removeAt(-1, T, H, Res);
removeAt(-1, [H|T], C, Res) ->
	removeAt(-1, T, C, [H|Res]);
removeAt(Acc, [H|T], C, Res) ->
	removeAt(Acc-1, T, C, [H|Res]).

ta(A) ->
	list_to_atom([A]).


-module(q1_10).

-export([myLast/1,
		 myButLast/1,
		 elementAt/2,
		 myLength/1,
		 myReverse/1,
		 isPalindrome/1,
		 flatten/1,
		 compress/1,
		 pack/1,
		 encode/1
		]).

% 1 Problem 1
% (*) Find the last element of a list.
%
% Example in Haskell:
%
% Prelude> myLast [1,2,3,4]
% 4
% Prelude> myLast ['x','y','z']
% 'z'

myLast([H]) ->
	H;
myLast([_|T]) ->
	myLast(T).

% 2 Problem 2
% (*) Find the last but one element of a list.
%
% (Note that the Lisp transcription of this problem is incorrect.)
%
% Example in Haskell:
%
% Prelude> myButLast [1,2,3,4]
% 3
% Prelude> myButLast ['a'..'z']
% 'y'

myButLast([H1, _]) ->
	H1;
myButLast([_|T]) ->
	myButLast(T).

% 3 Problem 3
% (*) Find the K'th element of a list. The first element in the list is number 1.
% 
% Example:
% 
% * (element-at '(a b c d e) 3)
% c
% Example in Haskell:
% 
% Prelude> elementAt [1,2,3] 2
% 2
% Prelude> elementAt "haskell" 5
% 'e'

elementAt([H|_], 1) ->
	H;
elementAt([_|T], Acc) ->
	elementAt(T, Acc - 1).

% 4 Problem 4
% (*) Find the number of elements of a list.
% 
% Example in Haskell:
% 
% Prelude> myLength [123, 456, 789]
% 3
% Prelude> myLength "Hello, world!"
% 13

myLength(L) ->
	myLength(L, 0).

myLength([], Acc) ->
	Acc;
myLength([_|T], Acc) ->
	myLength(T, Acc + 1).

% 5 Problem 5
% (*) Reverse a list.
% 
% Example in Haskell:
% 
% Prelude> myReverse "A man, a plan, a canal, panama!"
% "!amanap ,lanac a ,nalp a ,nam A"
% Prelude> myReverse [1,2,3,4]
% [4,3,2,1]

myReverse(L) ->
	myReverse(L, []).

myReverse([], L) ->
	L;
myReverse([H|T], L) ->
	myReverse(T, [H|L]).

% 6 Problem 6
% (*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
% 
% Example in Haskell:
% 
% *Main> isPalindrome [1,2,3]
% False
% *Main> isPalindrome "madamimadam"
% True
% *Main> isPalindrome [1,2,4,8,16,8,4,2,1]
% True

isPalindrome(L) ->
	isPalindrome(L, myReverse(L)).

isPalindrome([], []) ->
	true;
isPalindrome([H], [H]) ->
	true;
isPalindrome([H|T], [H|T]) ->
	isPalindrome(T, T);
isPalindrome(_, _) ->
	false.

% 7 Problem 7
% (**) Flatten a nested list structure.
% 
% Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).
% 
% Example:
% 
% * (my-flatten '(a (b (c d) e)))
% (A B C D E)
% Example in Haskell:
% 
% We have to define a new data type, because lists in Haskell are homogeneous.
% 
%  data NestedList a = Elem a | List [NestedList a]
% *Main> flatten (Elem 5)
% [5]
% *Main> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
% [1,2,3,4,5]
% *Main> flatten (List [])
% []

flatten(L) ->
	flatten(L, []).

flatten([], Res) ->
	myReverse(Res);
flatten([H|T], Res) when is_list(H) ->
	flatten(T, myReverse(flatten(H, Res)));
flatten([H|T], Res) ->
	flatten(T, [H|Res]).

% 8 Problem 8
% (**) Eliminate consecutive duplicates of list elements.
% 
% If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
% 
% Example:
% 
% * (compress '(a a a a b c c a a d e e e e))
% (A B C A D E)
% Example in Haskell:
% 
% > compress "aaaabccaadeeee"
% "abcade"

compress(L) ->
	compress(L, []).

compress([], Res) ->
	myReverse(Res);
compress([H|T], []) ->
	compress(T, [H]);
compress([H|T], [H|_] = Res) ->
	compress(T, Res);
compress([H1|T], [_|_] = Res) ->
	compress(T, [H1|Res]).

% 9 Problem 9
% (**) Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
% 
% Example:
% 
% * (pack '(a a a a b c c a a d e e e e))
% ((A A A A) (B) (C C) (A A) (D) (E E E E))
% Example in Haskell:
% 
% *Main> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 
%              'a', 'd', 'e', 'e', 'e', 'e']
% ["aaaa","b","cc","aa","d","eeee"]
% Solutions

pack([]) ->
	[];
pack([H]) ->
	[H];
pack([H|T]) ->
	pack(T, H, 1, []).

h_acc_pack(H, Acc) ->
	h_acc_pack(H, Acc, []).

h_acc_pack(_, 0, Res) ->
	Res;
h_acc_pack(H, Acc, Res) ->
	h_acc_pack(H, Acc-1, [H|Res]).

pack([], H, Acc, Res) ->
	myReverse([h_acc_pack(H, Acc) | Res]);
pack([H|T], H, Acc,  Res) ->
	pack(T, H, Acc+1, Res);
pack([H1|T], H2, Acc, Res) ->
	pack(T, H1, 1, [h_acc_pack(H2, Acc) | Res]). 

% 10 Problem 10
% (*) Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
% 
% Example:
% 
% * (encode '(a a a a b c c a a d e e e e))
% ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
% Example in Haskell:
% 
% encode "aaaabccaadeeee"
% [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

encode([]) ->
	[];
encode([H|T]) ->
	encode(T, H, 1, []).

encode([], H, Acc, Res) ->
	myReverse([{Acc, list_to_atom([H])} | Res]);
encode([H|T], H, Acc, Res) ->
	encode(T, H, Acc+1, Res);
encode([H1|T], H2, Acc, Res) ->
	encode(T, H1, 1, [{Acc, list_to_atom([H2])} | Res]). 

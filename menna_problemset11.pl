% problemset 11

% 1.a
% Check if first is a subsequence of second.
subseq([],_).
subseq([X|Xs],[X|Ys]) :- subseq(Xs,Ys).
subseq([X|Xs],[_Y|Ys]) :- subseq([X|Xs],Ys).

% 1.b
% Utility predicate to check if first is a sublist of second starting from position 0, and remove it.
sublist([],R,R).
sublist([X|Xs],[X|Ys],R) :- sublist(Xs,Ys,R).

% Search for subslist Needle in Haystack. Needle needs to occur consecutively in Haystack. 
search(Needle, Haystack, 0) :- sublist(Needle, Haystack, _).
search(Needle, [_Y|Haystack], Position) :- search(Needle, Haystack, NewPosition), Position is (NewPosition + 1).

% 1.c
% Can NewWhole can be produced from OldWhole by replacing zero or more occurrences
% of Old with New?
replace(_,_, [] , []).
replace(Old, New, [X|OldWhole], [X|NewWhole]) :- replace(Old, New, OldWhole, NewWhole).
replace(Old, New, OldWhole, NewWhole) :- sublist(Old, OldWhole, R1), sublist(New, NewWhole, R2), replace(Old, New, R1, R2).

% 1.d
% Is first list a suffix of the second?
suffix(X,X).
suffix(X,[_Y|Ys]) :- suffix(X,Ys).

% 1.e
% Does the second list consist of the first one only, repeated?
repeat(_X,[]).
repeat(X, [X|Xs]) :- repeat(X,Xs).

% 2.a
% Checks that a given number is less than or equal (=<) to all elements in a given list.
allLEQ(_X,[]).
allLEQ(X,[Y|Ys]):- (X =< Y), allLEQ(X,Ys).

% Checks that a given number is greater than or equal (>=) to all elements in a given list.
allGEQ(_X,[]).
allGEQ(X,[Y|Ys]):- (X >= Y), allGEQ(X,Ys).

% 2.b
% Checks that a given number is the minimum of a list.
minimum(X,Y) :- subseq([X],Y), allLEQ(X,Y).

% 2.c
% The list given is assumed to be sorted.
% Utility predicate that counts the number of elements less than Pivot in a list.
countLess(_,[],0).
countLess(Pivot, [X|List], R) :- countLess(Pivot, List, NewR), X < Pivot, R is (NewR + 1).
countLess(Pivot, [X|List], R) :- countLess(Pivot, List, NewR), X >= Pivot, R is NewR.

% Utility predicate that counts the number of elements greater than Pivot in a list.
countGreater(_,[],0).
countGreater(Pivot, [X|List], R) :- countGreater(Pivot, List, NewR), X > Pivot, R is (NewR + 1).
countGreater(Pivot, [X|List], R) :- countGreater(Pivot, List, NewR), X =< Pivot, R is NewR.

% Checks that Less (Greater) contains all elements of List that are less than 
% (resp. greater than) Pivot.
partition(Pivot, List, Less, Greater) :- 
    countLess(Pivot, List, R), length(Less,R),
    countGreater(Pivot, List, R2), length(Greater,R2),
    append(Less, [Pivot|Greater], FullList), subseq(FullList, List),
    allLEQ(Pivot, Greater), allGEQ(Pivot, Less).

% 2.d
% Checks that a number is the median of a list.
% The list given is assumed to be sorted.
medianHelper(X,[X|Y],Z) :- length(Y,R), length(Z,R).
medianHelper(X,[Y|YS],Z) :- medianHelper(X,YS,[Y|Z]). 

median(X,Y) :- medianHelper(X,Y,[]).

% 3.a
% Utility function that checks that list is all ones.
isAllOnes([]).
isAllOnes([1|X]) :- isAllOnes(X).

% Utility function that gets that the size of two lists is equal, and puts size in N. 
getSize([],[],0).
getSize([_X|XS],[_Y|YS], N) :- getSize(XS,YS, NewN), N is (NewN + 1).

% Checks on two binary lists that the second is the first +1.
increment(X,[1|Y]) :-  getSize(X,Y,M), length(X,M), length(Y,M), isAllZeroes(Y), isAllOnes(X).
increment([0|X],[1|Y]) :- getSize(X,Y,M),  length(X,M), length(Y,M), isAllZeroes(Y), isAllOnes(X).
increment([X|Xs], [X|Ys]) :- getSize(Xs,Ys,M), length(Xs,M), length(Ys,M), increment(Xs,Ys).

% 3.b
% Utiltiy Predicate that checks that list is all zeroes.
isAllZeroes([]).
isAllZeroes([0|X]) :- isAllZeroes(X).

% Utility predicate that checks that list is NOT all zeroes.
notAllZeroes(X) :- isAllZeroes(X), ! , fail.
notAllZeroes(_X).

% A predicate that helps count trailing zeroes.
countTrailingZeros(X, N) :- isAllZeroes(X), length(X,M), N is M.
countTrailingZeros([X|Xs],N) :- notAllZeroes([X|Xs]), countTrailingZeros(Xs, N).

% 4
% fib2 verifies that 2 numbers are consecutive Fibonacci numbers
fib2(0,1).
fib2(X,Y) :- fib2(Z, X), Y is (Z + X).

% fib is used to generate the Fibonacci sequence
fib(0).
fib(X) :- fib2(_,X).



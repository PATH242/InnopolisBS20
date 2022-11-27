% Problemset 12
% 1.a

minimumHelper([],R,R).
minimumHelper([A|As], X, R) :- (A < X), minimumHelper(As, A, R).
minimumHelper([A|As], X, R) :- (A >= X), minimumHelper(As, X, R).

minimum([A|As], X):- minimumHelper(As, A, X).

% 1.b
% TODO: fix

leastSpecificHelper(_,_,[]).
leastSpecificHelper(T,Ttemp,[A|As]):- A = Ttemp , leastSpecificHelper(T,T,As).

runHelper([A|_As],Rs,A) :- leastSpecificHelper(A,A,Rs), !.
runHelper([_A|As],Rs,T) :- runHelper(As, Rs, T).

leastSpecific(T,Rs):- runHelper(Rs,Rs,T). 

% 2.a

remove(_,[],[]).
remove(X,[X|As], Rs):-  !, remove(X, As, Rs).
remove(X,[A|As],[A|Rs]) :- remove(X,As,Rs).

% 2.b
removeU(X,[],[]).
removeU(X, [A|As], Rs) :- X = A, !, removeU(X,As,Rs).
removeU(X, [A|As], [A|Rs]) :- removeU(X, As, Rs).

% 3
nat(0).
nat(N) :- nat(K), N is K+1.

nat(1, 1) :- !.
nat(1, Max) :- Max > 1.
nat(N, Max) :- M is Max-1, nat(K, M), N is K+1.
    
% 3.a

isPrime(N) :-  nat(X,N), nat(Y,N), (X < N), (Y < N) , (N is (X * Y)), !, fail.
isPrime(N).

prime(N) :- isPrime(N).
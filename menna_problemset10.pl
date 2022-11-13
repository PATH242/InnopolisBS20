% student(Name, Group)
student(alisa, 2).
student(bob, 1).
student(chloe, 2).
student(denise, 1).
student(edward, 2).

% friend(Name, Name)
friend(alisa, bob).
friend(alisa, denise).
friend(bob, chloe).
friend(bob, edward).
friend(chloe, denise).
friend(denise, edward).

% parent(Parent, Child)
parent(marjorie, bart).
parent(marjorie, lisa).
parent(marjorie, maggie).
parent(homer, bart).
parent(homer, lisa).
parent(homer, maggie).
parent(abraham, homer).
parent(mona, homer).
parent(jacqueline, marjorie).
parent(jacqueline, patty).
parent(jacqueline, selma).
parent(clancy, marjorie).
parent(clancy, patty).
parent(clancy, selma).
parent(selma, ling).

% unary(Number)
unary(z).
unary(s(X)) :- unary(X).

% 1.a https://photos.app.goo.gl/A3PDhWkSYPTQXbAy7
% 1.b https://photos.app.goo.gl/wrPaYKJmC3wwq4vV9
% 1.c https://photos.app.goo.gl/DKuRwKS7a2JNtkgE6

% 2.
groupmates(X,Y) :- student(X,Z), student(Y,Z).

% 3.
ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(X,Z), ancestor(Z,Y).  
    
relative(X,Y) :- ancestor(Z,X), ancestor(Z,Y).

% 4.a
double(z,z).
double(s(X),s(s(Y))) :- double(X,Y).

% 4.b
leq(z,s(_X)).
leq(s(X),s(Y)) :- leq(X,Y).

% 4.c
add(z,Y,Y).
add(s(X),Y,R) :- add(X,s(Y),R).

mult(z,_Y,z).
mult(s(X),Y,R) :- add(Y,Z,R), mult(X,Y,Z).

% 4.d

divideBy2(z,z).
divideBy2(s(s(X)),s(R)) :- divideBy2(X,R).

powerOf2(z,s(z)).
powerOf2(s(X),M) :- leq(N,M), divideBy2(M,R), powerOf2(X,R).


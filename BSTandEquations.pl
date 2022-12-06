% Homework 3
% 1.1
% • node(Value, Left, Right) 
% Is tree?
tree(empty).
tree( node(_Value, Left, Right) ) :- tree(Left) , tree(Right).

% 1.2
% Is first tree a subtree of the second and has same root?
subtree(empty,_).
subtree(node(Value, Left1, Right1), node(Value, Left2, Right2) ) :-
    subtree(Left1, Left2), subtree(Right1, Right2).

% 1.3
% Get list starting from start [start, start+1, start +2 ...etc].
from(_Start, []).
from(Start, [Start|List]) :- NewStart is (Start + 1), from(NewStart, List).

% 1.4
% List of tree elements from left to right.
preorder(empty, []).
preorder(node(Root,Left,Right), Xs) :- append([Root|A], B, Xs), 
    preorder(Left, A), preorder(Right, B).
% 1.5
% Extending built in predicates to work with infinities.
leq(-infinity, _X) :- !.
leq(_X, +infinity) :- !.
leq(X,Y) :- (X =< Y).

less(-infinity, _X) :- !.
less(_X, +infinity) :- !.
less(X,Y) :- (X < Y).

% 1.6
% Utility predicates to help verify binary search trees.
% Check that Root is smaller than all elements in tree.
smallerThanGrandchildren(_, empty).
smallerThanGrandchildren(Root, node(X2,Left,Right)) :- Root < X2, 
    smallerThanGrandchildren(Root,Left),
    smallerThanGrandchildren(Root,Right).

% Check that Root is bigger than all elements in tree.
biggerThanGrandchildren(_,empty).
biggerThanGrandchildren(Root, node(X2,Left, Right) ):-
    Root > X2,
    biggerThanGrandchildren(Root, Left),
    biggerThanGrandchildren(Root, Right).

% Check that tree is binary search tree.
% Tree with no children is a BST.
bst(empty).
bst(node(_Root, empty, empty)).

% Tree with one child: root must be smaller than right child.
bst(node(Root, empty, node(Y, L,R))) :- Root < Y,
    smallerThanGrandchildren(Root, L), smallerThanGrandchildren(Root, R),
    bst(node(Y,L,R)).
% Tree with one chlid: root must be bigger than left child.
bst(node(Root,node(Y,L,R), empty) ) :- Root >= Y,
    biggerThanGrandchildren(Root, L), biggerThanGrandchildren(Root, R),
    bst(node(Y,L,R)).

% Tree with two children: root must be bigger than left subtree,
% and smaller than right subtree.
bst(node(Root, node(X1,L1,R1), node(X2,L2,R2) ) ) :-
    Root >= X1, Root < X2,
    biggerThanGrandchildren(Root, L1), biggerThanGrandchildren(Root, R1),
    smallerThanGrandchildren(Root, L2), smallerThanGrandchildren(Root, R2),
    bst(node(X1,L1,R1)), bst(node(X2,L2,R2)).

% 1.7
% Node is inserted as child of leaf.
bstInsert(Value, empty, node(Value,empty,empty) ).

bstInsert(Value, node(X,Left1,Right), node(X, Left2, Right) ) :-
          Value < X, !, bstInsert(Value, Left1, Left2).
bstInsert(Value, node(X, Left, Right1), node(X, Left, Right2) ) :-
    Value > X, bstInsert(Value, Right1, Right2).

% 1.8
% Get Min and Max from binary search tree.

% Traverse left subtrees to reach the leaf that has the min.
bstMin(node(X,empty,_), X).
bstMin(node(_,Left,_), Min) :-
    bstMin(Left, Min). 

% Traverse right subtrees to reach the leaf that has the max.
bstMax(node(X,_,empty), X).
bstMax(node(_,_,Right), Max) :-
    bstMax(Right, Max).

% 1.9
% utility function to help find new root for BST -> biggest number.
newBstRoot(node(Root, Left, empty), Root, Left) :- !.
newBstRoot(node(Value, Left, Right), Root, node(Value, Left, Right2) ) :-
    newBstRoot(Right, Root, Right2).

% Didnt find the node and reached an empty tree.
bstDelete(_Value, empty, empty) :- !, fail.

% Found the node, only Left subtree exists: the tree after deletion 
% will contain left subtree without the node.
bstDelete(Value, node(Value,Left,empty), Left) :- !.
% Found the node, only Right subtree exists: the tree after deletion 
% will contain Right subtree without the node.
bstDelete(Value, node(Value, empty, Right), Right):- !.

% Found the node, Right and Left children exist: the tree after 
% deletion will contain the largest value child as the new root,
% while left and right subtrees remain the same. This works since 
% the largest value child is a leaf found in the rightmost subtree.
bstDelete(Value, node(Value, Left, Right), node(X2, Left2, Right)) :-
          newBstRoot(Left, X2, Left2).

% Traverse the tree until you reach the desired node to delete.
bstDelete(Value, node(X, Left1, Right), node(X, Left2, Right) ):-
    Value < X, !,
    bstDelete(Value, Left1, Left2).
bstDelete(Value, node(X, Left, Right1), node(X, Left, Right2) ):-
    Value > X,
    bstDelete(Value, Right1, Right2).

% 2
% 2.1
% Utility predicate to help determine if input is an expression or not.
not_expr(_X+_Y) :- !, fail.
not_expr(_X*_Y) :- !, fail.
not_expr(_).

% Predicate to determine if input is a valid expression.
% Where variables are not allowed, and only +, * are allowed.
expr(X):- var(X), !, fail.
expr(X):- not_expr(X), !, number(X).

expr(X+Y) :- expr(X), expr(Y).
expr(X*Y) :- expr(X), expr(Y).

% 2.2
% Predicate to determine if input is not an empty list.
notEmptyList([]) :- !, fail.
notEmptyList(_).

% Predicate to determine expression given is a valid expression made out 
% of numbers in list given in same order.
expr(X,[X]):- !.
expr(X+Y, Xs) :-  append(A,B,Xs), notEmptyList(A), notEmptyList(B),expr(X,A), expr(Y,B).
expr(X*Y, Xs) :- append(A,B,Xs), notEmptyList(A), notEmptyList(B), expr(X,A), expr(Y,B).

% 2.3
% Get equation made out of numbers in list in same order.
equation(List, Equation) :- expr(Expr, List), Ans is Expr, Equation = (Ans = Expr).

% 2.4
% Get all possible equations made out of numbers in list in same order.
equations(List, Eqs, Equations) :-
    equation(List, NewEq),
    \+ member(NewEq, Eqs), !,
    equations(List, [NewEq|Eqs],Equations).
equations(_, Equations, Equations).

equations(List, Equations) :-
    equations(List,[],Equations).

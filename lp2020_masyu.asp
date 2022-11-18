node(X,Y) :- size(S), X = 0..S, Y = 0..S.
clue(X,Y) :- white(X,Y).
clue(X,Y) :- black(X,Y).

near(0,1).
near(0,-1).
near(1,0).
near(-1,0).

{arc(node(X,Y), node(X+X',Y+Y'))} :- node(X,Y), near(X',Y'), node(X+X',Y+Y').
:- arc(A,B), not arc(B,A).

in(Node) :- arc(Node,_).
:- in(A), #count{B : arc(A,B)} != 2.

:- clue(X,Y), not in(node(X,Y)).

start(A',B') :- arc(A',B') = #min{arc(A,B) : arc(A,B)}.
reach(A,B) :- start(A,B).
reach(B,C) :- reach(A,B), arc(B,C), A != C.
:- in(Node), not reach(_,Node).

:- white(X,Y), arc(node(X,Y),node(X,Y+1)), not arc(node(X,Y),node(X,Y-1)).
:- white(X,Y), arc(node(X,Y),node(X,Y-1)), not arc(node(X,Y),node(X,Y+1)).
:- white(X,Y), arc(node(X,Y),node(X,Y+1)), arc(node(X,Y+1),node(X,Y+2)), arc(node(X,Y-1),node(X,Y-2)).

:- white(X,Y), arc(node(X,Y),node(X+1,Y)), not arc(node(X,Y),node(X-1,Y)).
:- white(X,Y), arc(node(X,Y),node(X-1,Y)), not arc(node(X,Y),node(X+1,Y)).
:- white(X,Y), arc(node(X,Y),node(X+1,Y)), arc(node(X+1,Y),node(X+2,Y)), arc(node(X-1,Y),node(X-2,Y)).

:- black(X,Y), arc(node(X,Y),node(X,Y+1)), arc(node(X,Y),node(X,Y-1)).
:- black(X,Y), arc(node(X,Y),node(X,Y+1)), not arc(node(X,Y+1),node(X,Y+2)).
:- black(X,Y), arc(node(X,Y),node(X,Y-1)), not arc(node(X,Y-1),node(X,Y-2)).

:- black(X,Y), arc(node(X,Y),node(X+1,Y)), arc(node(X,Y),node(X-1,Y)).
:- black(X,Y), arc(node(X,Y),node(X+1,Y)), not arc(node(X+1,Y),node(X+2,Y)).
:- black(X,Y), arc(node(X,Y),node(X-1,Y)), not arc(node(X-1,Y),node(X-2,Y)).


#show.
#show right(X,Y) : arc(node(X,Y),node(X,Y+1)).
#show bottom(X,Y) : arc(node(X,Y),node(X+1,Y)).

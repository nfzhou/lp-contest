node(X,Y) :- size(S), X = 0..S, Y = 0..S.
clue(X,Y) :- white(X,Y,_).
clue(X,Y) :- black(X,Y,_).

near(0,1).
near(0,-1).
near(1,0).
near(-1,0).

{arc(node(X,Y), node(X+X',Y+Y'))} :- node(X,Y), near(X',Y'), node(X+X',Y+Y').
:- arc(A,B), not arc(B,A).

in(Node) :- arc(Node,_).
:- in(A), #count{B : arc(A,B)} != 2.

start(A',B') :- arc(A',B') = #min{arc(A,B) : arc(A,B)}.
reach(A,B) :- start(A,B).
reach(B,C) :- reach(A,B), arc(B,C), A != C.
:- in(Node), not reach(_,Node).

right(node(X,Y), node(X,Y+1)) :- clue(X,Y), arc(node(X,Y), node(X,Y+1)).
right(node(X,Y), node(X,Y'+1)) :- right(node(X,Y), node(X,Y')), arc(node(X,Y'), node(X,Y'+1)).

left(node(X,Y), node(X,Y-1)) :- clue(X,Y), arc(node(X,Y), node(X,Y-1)).
left(node(X,Y), node(X,Y'-1)) :- left(node(X,Y), node(X,Y')), arc(node(X,Y'), node(X,Y'-1)).

up(node(X,Y), node(X-1,Y)) :- clue(X,Y), arc(node(X,Y), node(X-1,Y)).
up(node(X,Y), node(X'-1,Y)) :- up(node(X,Y), node(X',Y)), arc(node(X',Y), node(X'-1,Y)).

down(node(X,Y), node(X+1,Y)) :- clue(X,Y), arc(node(X,Y), node(X+1,Y)).
down(node(X,Y), node(X'+1,Y)) :- down(node(X,Y), node(X',Y)), arc(node(X',Y), node(X'+1,Y)).

:- white(X,Y,_), arc(node(X,Y),node(X,Y+1)), not arc(node(X,Y),node(X,Y-1)).
:- white(X,Y,_), arc(node(X,Y),node(X,Y-1)), not arc(node(X,Y),node(X,Y+1)).
:- white(X,Y,_), arc(node(X,Y),node(X+1,Y)), not arc(node(X,Y),node(X-1,Y)).
:- white(X,Y,_), arc(node(X,Y),node(X-1,Y)), not arc(node(X,Y),node(X+1,Y)).
:- white(X,Y,N), #count{NODE : up(node(X,Y),NODE); NODE : down(node(X,Y),NODE); NODE : left(node(X,Y),NODE); NODE : right(node(X,Y),NODE)} != N.

:- black(X,Y,_), arc(node(X,Y),node(X,Y+1)), arc(node(X,Y),node(X,Y-1)).
:- black(X,Y,_), arc(node(X,Y),node(X+1,Y)), arc(node(X,Y),node(X-1,Y)).
:- black(X,Y,N), #count{NODE : up(node(X,Y),NODE); NODE : down(node(X,Y),NODE); NODE : left(node(X,Y),NODE); NODE : right(node(X,Y),NODE)} != N.


#show.
#show right(X,Y) : arc(node(X,Y),node(X,Y+1)).
#show bottom(X,Y) : arc(node(X,Y),node(X+1,Y)).

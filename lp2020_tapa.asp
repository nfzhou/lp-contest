cell(1..S,1..S) :- size(S).

delta(-1..1,-1..1).

clockside((-1,-1),(-1,0)).
clockside((-1,0),(-1,1)).
clockside((-1,1),(0,1)).
clockside((0,1),(1,1)).
clockside((1,1),(1,0)).
clockside((1,0),(1,-1)).
clockside((1,-1),(0,-1)).
clockside((0,-1),(-1,-1)).

orto(0,1). orto(0,-1). orto(1,0). orto(-1,0).

{black(X,Y)} :- cell(X,Y), not clue(_, (X,Y), _).

:- black(X,Y), black(X+1,Y), black(X,Y+1), black(X+1,Y+1).

sum_of_clue(CELL,S) :- clue(_,CELL,_), S = #sum{C,I : clue(I,CELL,C)}.
:- sum_of_clue((X,Y),S), #count{DX,DY : delta(DX,DY), black(X+DX,Y+DY)} != S.


group((X,Y), (DX,DY), (DX,DY)) :- clue(_,(X,Y),_), delta(DX,DY), black(X+DX,Y+DY), clockside((DX',DY'),(DX,DY)), not black(X+DX',Y+DY').

group((X,Y), G, (DX',DY')) :- group((X,Y), G, (DX,DY)), clockside((DX,DY), (DX',DY')), black(X+DX',Y+DY').

group_size((X,Y), G, S) :- group((X,Y), G, G), S = #count{D : group((X,Y), G, D)}.
:- clue(_,CELL,C), C > 0, #sum{1,G : group_size(CELL, G, C); -1,I : clue(I,CELL,C)} != 0.

reach(X',Y') :- (X',Y') = #min{(X,Y) : black(X,Y)}.
reach(X+DX,Y+DY) :- reach(X,Y), orto(DX,DY), black(X+DX,Y+DY).
:- black(X,Y), not reach(X,Y).

#show.
#show black/2.
#show size/1.

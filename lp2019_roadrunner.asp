
open(X,Y) :- xcoord(X), ycoord(Y), not hill(X,Y).


{ laser(X,Y) } :- open(X,Y).

{ safe(X,Y) : open(X,Y) }.


start(X,Y) :- safe(X,Y), not #true : safe(X2,Y2), (X,Y)>(X2,Y2).

path(X,Y,X,Y) :- safe(X,Y), not #true : safe(X2,Y2), (X,Y)!=(X2,Y2).

{ path(X,Y,X2,Y2) } :- safe(X,Y), safe(X2,Y2), X+DX=X2, Y+DY=Y2, dir(DX,DY).
:- path(X,Y,X2,Y2), path(X,Y,X3,Y3), (X2,Y2)!=(X3,Y3).
:- path(X2,Y2,X,Y), path(X3,Y3,X,Y), (X2,Y2)!=(X3,Y3).

connected(X,Y)   :- start(X,Y).
connected(X2,Y2) :- connected(X,Y), path(X,Y,X2,Y2).

:- safe(X,Y), not connected(X,Y).
:- start(X,Y), not path(_,_,X,Y).

dir(1,0).
dir(-1,0).
dir(0,1).
dir(0,-1).


beam(X+DX,Y+DY,DX,DY) :- laser(X,Y), dir(DX,DY), open(X+DX,Y+DY).
beam(X+DX,Y+DY,DX,DY) :- beam(X,Y,DX,DY), open(X+DX,Y+DY).

:- number(X,Y,N), N != #count{ DX,DY : laser(X+DX,Y+DY), dir(DX,DY) }.


:- laser(X,Y), beam(X,Y,_,_).

:- open(X,Y), not laser(X,Y), not safe(X,Y), not beam(X,Y,_,_).

:- safe(X,Y), beam(X,Y,_,_).
:- safe(X,Y), laser(X,Y).

safecircuitlen(L) :- L = #count{ 1,X,Y: safe(X,Y) }.

#maximize{ 1,X,Y: safe(X,Y) }.

%#show laser/2.
%#show safe/2.
#show safecircuitlen/1.


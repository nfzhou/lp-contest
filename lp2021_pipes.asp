poss(straight, 0..1).
poss(turn, 0..3).
poss(source, 0..3).
poss(t, 0..3).

left(straight,0). right(straight,0).
up(straight,1). down(straight,1).
left(turn,0). down(turn,0).
left(turn,1). up(turn,1).
up(turn,2). right(turn,2).
down(turn,3). right(turn,3).
right(source,0).
down(source,1).
left(source,2).
up(source,3).
left(t,0). right(t,0). down(t,0).
left(t,1). up(t,1). down(t,1).
left(t,2). right(t,2). up(t,2).
up(t,3). right(t,3). down(t,3).


{final(I,J,T,R') : poss(T,R')} = 1 :- initial(I,J,T,R).
final(0,0..S+1,empty,0) :- size(S).
final(0..S+1,0,empty,0) :- size(S).

:- final(I,J,T,R), right(T,R), final(I,J+1,T',R'), not left(T',R').
:- final(I,J,T,R), left(T,R), final(I,J-1,T',R'), not right(T',R').
:- final(I,J,T,R), down(T,R), final(I+1,J,T',R'), not up(T',R').
:- final(I,J,T,R), up(T,R), final(I-1,J,T',R'), not down(T',R').

reach(I,J) :- (I,J) = #min{(I',J') : initial(I',J',source,_)}.
reach(I,J+1) :- reach(I,J), final(I,J,T,R), right(T,R).
reach(I,J-1) :- reach(I,J), final(I,J,T,R), left(T,R).
reach(I+1,J) :- reach(I,J), final(I,J,T,R), down(T,R).
reach(I-1,J) :- reach(I,J), final(I,J,T,R), up(T,R).
:- initial(I,J,_,_), not reach(I,J).

#show.
#show initial/4.
#show final(I,J,T,R) : final(I,J,T,R), I != 0, J != 0, I != S+1, J != S+1, size(S).

:- use_module(library(clpfd)).

loca(1,1,1).

loca(T1,R,C) :-
	T0 #= T1 - 1,
	RN #= R - 1,
	RS #= R + 1,
	CW #= C - 1,
	CE #= C + 1,
	(
	((action(T0,clockWise);action(T0,counterClockWise)), loca(T0,R,C));
	(action(T0,forward), bump(T1), loca(T0,R,C));
	(dir(T0,east), action(T0,forward), not(bump(T1)), loca(T0,R,CW));
	(dir(T0,north), action(T0,forward), not(bump(T1)), loca(T0,RS,C));
	(dir(T0,south), action(T0,forward), not(bump(T1)), loca(T0,RN,C));
	(dir(T0,west), action(T0,forward), not(bump(T1)), loca(T0,R,CE))
	
	).


dir(1,east).
dir(T1,north) :-
	T0 #= T1 - 1,
	(
	((action(T0,forward)), dir(T0,north));
	(action(T0,clockWise) , dir(T0,west));
	(action(T0,counterClockWise), dir(T0,east));
	(action(T0,hit), dir(T0,north))
	).

dir(T1,south) :-
	T0 #= T1 - 1,
	(
	((action(T0,forward)), dir(T0,south));
	(action(T0,clockWise) , dir(T0,east));
	(action(T0,counterClockWise), dir(T0,west));
	(action(T0,hit), dir(T0,south))
	).

dir(T1,east) :-
	T0 #= T1 - 1,
	(
	((action(T0,forward)), dir(T0,east));
	(action(T0,clockWise) , dir(T0,north));
	(action(T0,counterClockWise), dir(T0,south));
	(action(T0,hit), dir(T0,east))
	).

dir(T1,west) :-
	T0 #= T1 - 1,
	(
	((action(T0,forward)), dir(T0,west));
	(action(T0,clockWise) , dir(T0,south));
	(action(T0,counterClockWise), dir(T0,north));
	(action(T0,hit), dir(T0,west))
	).


wall(R, C) :- 
	RN #= R - 1,
	RS #= R + 1,
	CW #= C - 1,
	CE #= C + 1,
	bump(T1),
	T0 #= T1 - 1,
	((dir(T0, east), loca(T0, R, CW));
	(dir(T0, west), loca(T0, R, CE));
	(dir(T0, north), loca(T0, RS, C));
	(dir(T0, south), loca(T0, RN, C))).


wallInFront(T) :- 
	RN #= R - 1,
	RS #= R + 1,
	CW #= C - 1,
	CE #= C + 1,
	dir(T, L),
	loca(T, R, C),
	((L = east, wall(R, CE)) ;
	(L = west, wall(R, CW)) ;
	(L = north, wall(RN, C)) ;
	(L = south, wall(RS, C))).

noWumpus(1,1).
noWumpus(R1,C1) :-
	(R1 < 1 ; C1 < 1) ;
	(
		loca(T, R0, C0),
		not(wumpusSmell(T)),
		isNeighbour(R0, C0, R1, C1)

	).

seen(R1, C1) :-
	wumpusSight(T),
	loca(T, R0 , C0),
	(
		(dir(T, east), ((C1 is C0 + 1) ; (C1 is C0 + 2) ; (C1 is C0 + 3) ; (C1 is C0 + 4))) ;
		(dir(T, west), ((C1 is C0 - 1) ; (C1 is C0 - 2) ; (C1 is C0 - 3) ; (C1 is C0 - 4))) ;
		(dir(T, north), ((R1 is R0 + 1) ; (R1 is R0 + 2) ; (R1 is R0 + 3) ; (R1 is R0 + 4))) ;
		(dir(T, south), ((R1 is R0 - 1) ; (R1 is R0 - 2) ; (R1 is R0 - 3) ; (R1 is R0 - 4))) 
	).

isNeighbour(R0, C0, R1, C1) :- %ikinci birincinin neighbouru
	((C1 #= C0 + 1) , (R1 #= R0)) ;
	((C1 #= C0 - 1) , (R1 #= R0)) ;
	((C1 #= C0) , (R1 #= R0 - 1)) ;
	((C1 #= C0) , (R1 #= R0 + 1)).

wumpus(R, C) :-
	seen(R, C),
	isNeighbour(R1, C1, R, C),
	not(noWumpus(R,C)),
	loca(T1, R1, C1),
	wumpusSmell(T1),
	(
		otherNeighboursFalse(R1, C1, R, C);
		not(wumpusSight(T1))
	).

isWinner(T) :-
	action(T, hit),
	loca(T, R, C),
	( 
		(C1 #= C + 1, dir(T, east), wumpus(R, C1)) ;
		(C1 #= C - 1, dir(T, west), wumpus(R, C1)) ;
		(R1 #= R - 1, dir(T, north), wumpus(R1, C)) ;
		(R1 #= R + 1, dir(T, south), wumpus(R1, C))
	).


otherNeighboursFalse(R1, C1, R0, C0) :-
	((C1 #= C0 + 1) , (R1 #= R0) , noWumpus(R1, C1 + 1), noWumpus(R1 + 1, C1), noWumpus(R1 - 1, C1)); %sol komsu
	((C1 #= C0 - 1) , (R1 #= R0) , noWumpus(R1, C1 - 1), noWumpus(R1 + 1, C1), noWumpus(R1 - 1, C1)); %sag komsu
	((C1 #= C0) , (R1 #= R0 - 1) , noWumpus(R1 - 1, C1), noWumpus(R1, C1 + 1), noWumpus(R1, C1 - 1)); %alt komsu
	((C1 #= C0) , (R1 #= R0 + 1) , noWumpus(R1 + 1, C1), noWumpus(R1, C1 + 1), noWumpus(R1, C1 - 1)). %ust komsu




%see1(I, X, Y) :- (wumpusSight(I), stat(I,X,Y, e)) -> (wumpus(X-1,Y) ; wumpus(X-2,Y) ; wumpus(X-3,Y) ; wumpus(X-4,Y)) ; 
%	 stat(I,X,Y, e) -> (not(wumpus(X-1,Y)) , not(wumpus(X-2,Y)) , not(wumpus(X-3,Y)) , not(wumpus(X-4,Y))).

%see2(I, X, Y) :- (wumpusSight(I), stat(I,X,Y, w)) -> (wumpus(X+1,Y) ; wumpus(X+2,Y) ; wumpus(X+3,Y) ; wumpus(X+4,Y)) ;
%	stat(I,X,Y, w) -> (not(wumpus(X+1,Y)) , not(wumpus(X+2,Y)) , not(wumpus(X+3,Y)) , not(wumpus(X+4,Y))).

%see3(I, X, Y) :- (wumpusSight(I), stat(I,X,Y, n)) -> (wumpus(X,Y-1) ; wumpus(X,Y-2) ; wumpus(X,Y-3) ; wumpus(X,Y-4)) ;
%	stat(I,X,Y, n) -> (not(wumpus(X,Y-1)) , not(wumpus(X,Y-2)) , not(wumpus(X,Y-3)) , not(wumpus(X,Y-4))).


%see4(I, X, Y) :- (wumpusSight(I), stat(I,X,Y, s)) -> (wumpus(X,Y+1) ; wumpus(X,Y+2) ; wumpus(X,Y+3) ; wumpus(X,Y+4)) ;
%	stat(I,X,Y, s) -> (not(wumpus(X,Y+1)) , not(wumpus(X,Y+2)) , not(wumpus(X,Y+3)) , not(wumpus(X,Y+4))).


%smell(I, X, Y) :- (wumpusSmell(I), stat(I,X,Y, _)) -> (wumpus(X-1, Y) ; wumpus(X+1,Y) ; wumpus(X,Y-1) ; wumpus(X,Y+1));
%	stat(I,X,Y, _) -> (not(wumpus(X-1, Y)) , not(wumpus(X+1,Y)) , not(wumpus(X,Y-1)) , not(wumpus(X,Y+1))).

bump(0).
action(1,forward).
bump(2).
action(2,clockWise).
action(3,forward).
action(4,counterClockWise).
action(5,forward).
action(6,counterClockWise).







% ------------------------------- %
% CSE341 HW4					  %	
% Ahmed Semih Ozmekik, 171044039  %	
% --------------------------------%

% I have used non binded variables. 
% Statement below suppresses warning just for that. 
:- style_check(-singleton).


% --------- PART1 and PART2 ------------ %


% knowledge base.
% flight(X, Y): there exits a flight between X and Y.

flight(istanbul, antalya).
flight(istanbul, izmir).
flight(istanbul, gaziantep).
flight(istanbul, ankara).
flight(istanbul, van).
flight(istanbul, rize).
flight(rize, van).
flight(ankara, van).
flight(ankara, konya).
flight(konya, antalya).
flight(gaziantep, antalya).
flight(burdur, isparta).
flight(isparta, izmir).
flight(erzincan, edremit).
flight(edremit, edirne).

distance(istanbul, antalya, 483).
distance(istanbul, izmir, 329).
distance(istanbul, gaziantep, 848).
distance(istanbul, ankara, 352).
distance(istanbul, van, 1263).
distance(istanbul, rize, 968).
distance(rize, van, 373).
distance(ankara, van, 921).
distance(ankara, konya, 228).
distance(konya, antalya, 193).
distance(gaziantep, antalya, 593).
distance(burdur, isparta, 25).
distance(isparta, izmir, 309).
distance(erzincan, edremit, 740).
distance(edremit, edirne, 915).

% rules
direct_route(X, Y, D):- distance(X, Y, D).
direct_route(X, Y, D):- distance(Y, X, D).
 
% finds the all (directly and undirectly) routes between X-Y. 
route(X, Y, Routes):- 
	findall(A, find_route(X, Y, A, D), Routes).

% finds the shortest route between X-Y.
sroute(X,Y,Shortest) :-
   findall(D, find_route(X,Y,A,D), Ds), min(Ds, Shortest).

% some helpers.
find_route(X, Y, A, D) :- 
       search_route(X, Y, [X], A, D).
search_route(X, Y, P, [Y|P], D) :- 
       direct_route(X,Y,D).
search_route(X, Y, Routed, A, D) :-
	   % You can see in(E, List) helper func below in part 4.
       direct_route(X, Z, D1), not(Z == Y), not(in(Z, Routed)),           
       search_route(Z, Y, [Z|Routed],A,D2), D is D1+D2.

% min(List, Min). finds the min item of list.
min([M],M).
min([X,Y|Z],M) :-
    X =< Y, min([X|Z],M).
min([X,Y|Z],M) :-
    X > Y,  min([Y|Z],M).


% --------- PART3 ------------ %  

% facts.

% time facts: time of the course X is Y.
when(102, 10).
when(108, 12).
when(341, 14).
when(455, 16).
when(452, 17).

% place facts: place of the course X is Y.
where(102, z23).
where(108, z11).
where(341, z06).
where(455, 207).
where(452, 207).

% enrollment facts: student X is enrolled in course Y.
enroll(a, 102).
enroll(a, 108).
enroll(b, 102).
enroll(c, 108).
enroll(d, 341).
enroll(e, 455).

% rules.

schedule(S, P, T):-
	enroll(S, C),
	where(C, P),
	when(C, T).

usage(P, T):-	% time of a classroom.
	where(C, P),
	when(C, T).

conflict_class(X, Y):- % conflict helper.
	where(X, CX),
	where(Y, CY),
	X \== Y,
	CX == CY.

conflict_time(X, Y):- % conflict helper.
	where(X, TX),
	where(Y, TY),
	X \==Y,
	TX == TY.

conflict(X, Y):-
	conflict_class(X, Y),
	conflict_time(X, Y).

meet(X, Y):-
	schedule(X, PX, TX),
	schedule(Y, PY, TY),
	X \== Y,
	PX == PY,
	TX == TY.       

% --------- PART4 ------------ % 

% -- Helper Predicates -- 

% returns true if E in list.
in(E, [E|Rest]).
in(E, [Ignore|Rest]):-
	in(E, Rest).

% constructs C for C = A U B.
make_union([], C, C).
make_union([E|RestA], B, [E|C]):-	
	make_union(RestA, B, C).


% constructs C for C = A n B.
make_intersect([], B, []).

make_intersect([E|RestA], B, [E|C]):-
	element(E, B),
	make_intersect(RestA, B, C).

make_intersect([Ignore|Rest], B, C):-
	make_intersect(Rest, B, C).


% returns true if A C B.
subset([], B).
subset([E|Rest], B):-		
	element(E, B),
	subset(Rest, B).


% 4.1. returns true if element E is in S.
element(E, S):-
	in(E, S).

% 4.2. returns true if S3 is the union of S1 and S2.
union(S1, S2, S3):-
	make_union(S1, S2, C),
	equivalent(S3, C).

% 4.3. returns true if S3 is the intersection of S1 and S2.
intersect(S1, S2, S3):-
	make_intersect(S1, S2, C),
	equivalent(S3, C).

% 4.4. returns true if S1 and S2 are equivalent sets.
equivalent(S1, S2):-
	subset(S1, S2),
	subset(S2, S1).

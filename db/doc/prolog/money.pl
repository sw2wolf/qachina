%:- module(money, [winG/3, stopLoss/3]).
:- use_module(library(dcg/basics)).
%:- use_module(library(clpfd)).
%:- use_module(library(assoc)).

:- set_prolog_flag(toplevel_print_options,
	[backquoted_string(true), max_depth(9999),
	 portray(true), spacing(next_argument)]).
:- set_prolog_flag(debugger_print_options,
	[backquoted_string(true), max_depth(9999),
	 portray(true), spacing(next_argument)]).

:- set_prolog_flag(generate_debug_info, false).
%:- set_prolog_flag(verbose_file_search, true).

:- assertz(user:file_search_path(qachina, '/media/D/qachina')).
:- assertz(user:file_search_path(money, '/media/D/qachina/db/doc/money')).

:- load_files([ qachina(test_web) ], [ silent(true) ]).

sxf(0.0015).
yhs(0.001).
ghf(1.0).

%计算股票盈利
winG(Qty,Pb,Ps) :-
	sxf(SXF), yhs(YHS), ghf(GHF),
	format("You win ~2f", [Qty*Ps*(1-SXF-YHS) - 2*GHF - Qty*Pb*(1+SXF)]) .

%算权证盈利
winQ(Qty,Pb,Ps) :-
	sxf(SXF), ghf(GHF),
	format("You win ~2f", [Qty*Ps*(1-SXF) - 2*GHF - Qty*Pb*(1+SXF)]) .

%止损价
stopLoss(Qty,Pb) :- stopLoss(Qty,Pb,0.03).
stopLoss(Qty,Pb,LossRate) :-
	sxf(SXF),
	T is Qty * Pb * (1 + SXF),
	format("Stop Loss at: ~2f~n", [Pb - (T * LossRate) / Qty]),
	format("Lost Money: ~2f~n", [T * LossRate]).

show618(P1, P2, R) :-
	RP1 is rationalize(P1),
	RP2 is rationalize(P2),
	RR is rationalize(R),
	(P1=<P2 -> P is RP1+(RP2-RP1)*RR; P is RP1-(RP1-RP2)*RR),
	format("---~3f ~2f---~n",[R,P]).
div618(P1, P2) :-
	RATIO = [0.0, 0.191, 0.236, 0.382, 0.5, 0.618, 0.809, 1.0],
	(P1>P2 -> R = RATIO;
		reverse(RATIO,R)),
	maplist(show618(P1,P2), R).

%% my_comp(Comp, N1, N2) :-
%% 	( N1 =< N2 -> Comp = '>'
%% 	; N1 > N2 -> Comp = '<').
%% show618(P1, P2, R) :-
%% 	(P1=<P2 -> P is P1+(P2-P1)*R; P is P1-(P1-P2)*R),
%% 	format("---~3f ~2f---~n",[R,P]).
%% div618(P1, P2) :-
%% 	RATIO = [0.0, 0.191, 0.236, 0.382, 0.5, 0.618, 0.809, 1.0],
%% 	(P1>P2 -> R = RATIO;
%% 		predsort(my_comp, RATIO, R)),
%% 	maplist(show618(P1,P2), R).

sd(Word) :-
	(   atom(Word)
	->  true
	;   throw(error(type_error(atom, Word), _))
	),
	format(string(Cmd),'sdcv -n ~w',[Word]), shell(Cmd).

%%
%% lottery
%%
win_ssq(Count, NoRedStr, NoBlueStr) :-
	Count >= 1,
	atom2lst(NoRedStr, NoRed),
	atom2lst(NoBlueStr, NoBlue),
	numlist(1,33,R), subtract(R,NoRed,YesR),
	numlist(1,16,B), subtract(B,NoBlue,YesB),
	set_random(seed(random)),
	pick_nums(Count,YesB,OkB),
	pick_red(Count, YesR, OkB, X), length(X,Count), !,
	maplist(writeln,X),
	ssqNumF(F),
    tell(F),maplist(format('~d ~d ~d ~d ~d ~d ~d~n'),X),told.

pick_red(0,_,_,_) :- !.
pick_red(Count,YesR,OkB,[H|T]) :-
	pick_nums(6,YesR,R1), sort(R1,Red), nth1(Count,OkB,Blue),
	append(Red,[Blue],H), C1 is Count-1,
	pick_red(C1,YesR,OkB,T).

ssq_test :-
	assertz(hitnum(13001,[1,2,3,4,5,6],1)),
	assertz(hitnum(13002,[7,8,9,10,11,12],2)),
    assertz(hitnum(13002,[21,22,23,32,33,1],3)).
	%findall(HitRed, hitnum(_,HitRed,_),X), append(X, Y).

%read_file_to_codes('test.txt',X,[]), atom_codes(Y,X).
hit_ssq(ID, HitNo) :-
	atomic_list_concat([ID,' ',HitNo], NumStr),
	ssqHitNumF(File),
	setup_call_cleanup(
		open(File, read, HF),
		(   \+ has_hit_id(HF, ID)
		->  append(File), write(NumStr), nl, told
		;   true
		),
		close(HF)),
	atom2lst(HitNo,HN),
    ssqNumF(F),
	setup_call_cleanup(
		open(F, read, H),
	    all_pick_nums(H,Ns),
		close(H)), ! ,
	maplist(hit_sum(HN), Ns) .
	
hit_sum(HitNo, No) :-
	last(HitNo,B1), last(No,B2),
	(B1 == B2 -> HitB is 1; HitB is 0),
	append(RedH,[B1],HitNo), !,
	append(RedN,[B2],No), !,
	intersection(RedH, RedN, X), length(X,HitR), hit_desc(HitR,HitB,Desc),
	format('~p ~t(~p,~p)~25| ~t~p~38|~n',[No,HitR,HitB,Desc]).

ints(L) --> blanks, (integer(I), ints(Is), {L = [I|Is]} ; {L = []}).

has_hit_id(F, ID) :-
	read_line_to_codes(F, Cs),
    (   Cs == end_of_file
    ->  false
    ;   phrase(ints(Is), Cs),
		atom_number(ID, IDNo),
		(   memberchk(IDNo, Is)
		->  true
		 ;  has_hit_id(F, ID)
		)
    ).
	
all_pick_nums(F, L) :-
    read_line_to_codes(F, Cs),
    (   Cs == end_of_file
    ->  L = []
    ;   phrase(ints(Is), Cs),
        all_pick_nums(F, R),
        L = [Is|R]
    ).

pick_nums(0, _, []).
pick_nums(Count, From, [X|SelectedFromRemaining]) :-
  random_member(X, From),
  select(X, From, Remaining),
  C1 is Count - 1,
  pick_nums(C1, Remaining, SelectedFromRemaining).

atom2lst(Atom,L) :-
    atomic_list_concat(X, ' ', Atom),
	maplist(atom_number, X, L).

ssqNumF(F) :-
	absolute_file_name(money('ssqNum.txt'), F, []).

ssqHitNumF(F) :-
	absolute_file_name(money('ssqHitNum.txt'), F, []).

his :-
	ssqHitNumF(File),
	atom_concat('tail ', File, Cmd),
	shell(Cmd).

hit_desc(6,1,'1st') :- !.
hit_desc(6,0,'2nd') :- !.
hit_desc(5,1,'3rd(3000)') :- !.
hit_desc(5,0,'4th(200)') :- !.
hit_desc(4,1,'4th(200)') :- !.
hit_desc(4,0,'5th(10)') :- !.
hit_desc(3,1,'5th(10)') :- !.
hit_desc(_,1,'6th(5)') :- !.
hit_desc(_,_,'X') :- !.

%%
%% utilities
%%
qachina :-
	server(8000).
	%thread_create(shell('cd /media/D/qachina; ./start.bat'),_,[detached(true)]).

fac(N,F) :-
	N is 0, F is 1;
    N > 0, M is N - 1, fac(M,G), F is N*G.

fib(0, 0) :- !.
fib(1, 1) :- !.
fib(N, X) :- N1 is N-1, N2 is N-2, fib(N1, X1), fib(N2, X2), X is X1+X2.

binary(0,'0').
binary(1,'1').
binary(N,B) :- N>1,X is N mod 2,Y is N//2,binary(Y,B1),atom_concat(B1, X, B), !.

sum([],0).
sum([H|T],X) :- sum(T,Y), X is H + Y.

product([],1).
product([H|T],X) :- product(T,Y), X is H * Y.

sys_info :-
	current_prolog_flag(version_data, swi(Major, Minor, Patch, _)),
	format('swi-prolog version: ~w.~w.~w~n',[Major,Minor,Patch]).

% list comprehesion
%% List of Pythagorean triples : 
%% ?- V <- {X, Y, Z & X <- 1..20, Y <- X..20, Z <- Y..20 & X*X+Y*Y =:= Z*Z}.
%% V = [ (3,4,5), (5,12,13), (6,8,10), (8,15,17), (9,12,15), (12,16,20)] ;

%% List of double of x, where x^2 is greater than 50 : 
%% ?- V <- {Y & X <- 1..20 & X*X > 50, Y is 2 * X}.
%% V = [16,18,20,22,24,26,28,30,32,34,36,38,40] ;

% We need operators
:- op(700, xfx, <-).
:- op(450, xfx, ..).
:- op(1100, yfx, &).

% we need to define the intervals of numbers
Vs <- M..N :-
    integer(M),
	integer(N),
	M =< N,
	between(M, N, Vs).
 
% finally we define list comprehension
% prototype is Vs <- {Var, Dec, Pred} where
% Var is the list of variables to output
% Dec is the list of intervals of the variables
% Pred is the list of predicates
Vs <- {Var & Dec & Pred} :-
	findall(Var,  maplist(call, [Dec, Pred]), Vs).

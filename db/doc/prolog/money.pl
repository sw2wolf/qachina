%:- module(money, [winG/3, stopLoss/3]).
:- use_module(library(dcg/basics)).

:- set_prolog_flag(toplevel_print_options,
	[backquoted_string(true), max_depth(9999),
	 portray(true), spacing(next_argument)]).

:- set_prolog_flag(generate_debug_info, false).
:- set_prolog_flag(verbose_file_search, true).

%it prints out "hi Floris" in debug, not a bunch of numbers.
:- portray_text(true).

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
	%RP1 is rationalize(P1),
	%RP2 is rationalize(P2),
	%RR is rationalize(R),
	(P1=<P2 -> P is P1+(P2-P1)*R; P is P1-(P1-P2)*R),
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
	string_to_atom(NoRedStr, NoRedAtom),
	string_to_atom(NoBlueStr, NoBlueAtom),
	Count >= 1,
%
	atom2lst(NoRedAtom, NoRed),
	atom2lst(NoBlueAtom, NoBlue),
    good_red(GR1), subtract(GR1,NoRed,GR),
	numlist(1,16,B), subtract(B,NoBlue,YesB),
	numlist(1,33,R), subtract(R,NoRed,YesR),
%	
	set_random(seed(random)),
	pick_nums(Count,YesB,OkB),
	pick_red(Count, GR, YesR, OkB, Res), length(Res,Count), !,
	maplist(writeln,Res),
	ssqNumF(F),
    tell(F), maplist(format('~d ~d ~d ~d ~d ~d ~d~n'), Res), told.

pick_red(1, GoodR, _, OkB, [H|_]) :-
    pick_nums(6,GoodR,R1), sort(R1,Red), nth1(1,OkB,Blue),
	append(Red,[Blue],H), !.
pick_red(Count, GoodR, YesR, OkB, [H|T]) :-
	pick_nums(6,YesR,R1), sort(R1,Red), nth1(Count,OkB,Blue),
	append(Red,[Blue],H), C1 is Count-1,
	pick_red(C1,GoodR,YesR,OkB,T).

pick_nums(0, _, []).
pick_nums(Count, From, [X|SelectedFromRemaining]) :-
  random_member(X, From), sleep(0.001),
  select(X, From, Remaining),
  C1 is Count - 1,
  pick_nums(C1, Remaining, SelectedFromRemaining).

:- dynamic
 	hitnum/3.

good_red(GRed) :-
	ssqHitNumF(HNFile),
	setup_call_cleanup(
		open(HNFile, read, HF),
		(    hit_nums(HF),
		     findall(HR, (hitnum([_|No]),last(No,B),append(HR,[B],No)), HitRed),
			 append(HitRed, HitRedNo),
			 numlist(1,33,X), map_list_to_pairs(count_hit_red(HitRedNo), X, Y),
			 keysort(Y, Z), pairs_values(Z, ZZ), sublist(ZZ,12,32,ZZZ),
			 sort(ZZZ, GRed)
		),
		close(HF)), !,
	retractall(hitnum(_)).

count_hit_red(HitRed, RedNo, RedCnt) :-
	countOf(HitRed, RedNo, 0, RedCnt), !.

countOf([], _, Cnt, Acc) :-
	Acc = Cnt, !.
countOf([X|Rest], X, Cnt, Acc) :-
	C1 is Cnt + 1,
	countOf(Rest, X, C1, Acc).
countOf([_|Rest], X, Cnt, Acc) :-
	countOf(Rest, X, Cnt, Acc).

sublist(L1, I, J, L2):-
    sublist(L1, Temp, I, J, []),
    !,
    reverse(Temp, L2).
sublist(L1, I, J, L2):-
    length(L1, Length),
    J > Length,
    sublist(L1,I,Length,L2).

sublist([], L2, _I, _J, L2).
sublist(_L1, L2, I, J, L2):-
    I > J.
sublist(L1, L2, I, J, L2):-
    I < 0,
    sublist(L1, L2, 0, J, L2).
sublist([_L|Ls], L2, I, J, Acc):-
    I > 0,
    sublist(Ls, L2, I-1, J-1, Acc).
sublist([L|Ls], L2, I, J, Acc):-
    sublist(Ls, L2, I, J-1, [L|Acc]).

hit_nums(F) :-
	read_line_to_codes(F, Cs),
    (   Cs == end_of_file
    ->  true
    ;   phrase(ints(Is), Cs),
		assertz(hitnum(Is)),
		hit_nums(F)
    ).

hit_ssq(IDStr, HitNoStr) :-
	string_to_atom(IDStr, ID),
	string_to_atom(HitNoStr, HitNo),
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
	/**/
	good_red(GRed),
	last(HN,B1),append(RedH,[B1],HN),
	intersection(RedH, GRed, X), length(X, HitR),
	format('Good red hit ~p~n', [HitR]), format('---------------~n'),
	/**/
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
	format('~p ~t(~p,~p)~30| ~t~p~38|~n',[No,HitR,HitB,Desc]).

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

%
% misc.
%
qachina :-
	% working_directory(_, '/media/D/qachina'),
	% consult('test_web.pl'),
	server(8000).
	%thread_create(shell('cd /media/D/qachina; ./start.bat'),_,[detached(true)]).

is_leap_year(Year) :-
	R4 is Year mod 4,
	R100 is Year mod 100,
	R400 is Year mod 400,
	( (R4 = 0, R100 \= 0); R400 = 0 ).

utf8(Str) :-
	phrase(utf8_codes(Str), X),
	print(X), nl.

fac(N,F) :-
	N is 0, F is 1;
    N > 0, M is N - 1, fac(M,G), F is N*G.

fib(0, 0) :- !.
fib(1, 1) :- !.
fib(N, X) :- N1 is N-1, N2 is N-2, fib(N1, X1), fib(N2, X2), X is X1+X2.

bits(X) :- format('~2r~n', [X]).

sum([],0).
sum([H|T],X) :- sum(T,Y), X is H + Y.

product([],1).
product([H|T],X) :- product(T,Y), X is H * Y.

combLen(N, CN, Cnt) :-
	N >= CN,
	X1 is N-CN+1, numlist(X1,N,X2), product(X2,X),
	numlist(1,CN,Y2), product(Y2,Y),
	Cnt is X / Y.

sys_info :-
	current_prolog_flag(version_data, swi(Major, Minor, Patch, _)),
	format('swi-prolog version: ~w.~w.~w~n',[Major,Minor,Patch]).

dbg_mon :-
    prolog_ide(debug_monitor).

% :- initialization
% 	working_directory(_, '/media/D/qachina').

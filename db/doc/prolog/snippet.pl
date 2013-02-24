
:- use_module(library(persistency)).
:- use_module(library(aggregate)).
:- use_module(library(memfile)).
pack_query(Request) :-
	memberchk(content_type(ContentType), Request),
	sub_atom(ContentType, 0, _, _, 'text/x-prolog'), !,
	peer(Request, Peer),
	setup_call_cleanup(
	    new_memory_file(MemFile),
	    ( setup_call_cleanup(
		  open_memory_file(MemFile, write, Stream),
		  http_read_data(Request, _, [to(stream(Stream))]),
		  close(Stream)),
	      setup_call_cleanup(
		  open_memory_file(MemFile, read, In),
		  read(In, Query),
		  close(In))
	    ),
	    free_memory_file(MemFile)),
	(   catch(pack_query(Query, Peer, Reply), E, true)
	->  format('Content-type: text/x-prolog; charset=UTF8~n~n'),
	    (   var(E)
	    ->	format('~q.~n', [true(Reply)])
	    ;	format('~w.~n', [exception(E)])
	    )
	;   format('Content-type: text/x-prolog; charset=UTF8~n~n'),
	    format('false.~n')
	).

write_clf(Out, Options):-
	open(Out, write, OutS, [encoding(utf8)]),
	call_cleanup(write_records(OutS, Options), close(OutS)).

text_to_data(Text, Y/M/D) :-
	atomic_list_concat([YA, MA, DA], /, Text),
	atom_number(YA, Y),
	atom_number(MA, M),
	atom_number(DA, D).

:- meta_predicate
	limit(+, 0).
 
limit(Max, Goal) :-
	State = count(0,_),
	call(Goal),
	arg(1, State, Count),
	Count1 is Count+1,
	(   Count1 =:= Max
	->  !
	;   nb_setarg(1, State, Count1)
	).

process_create(path(git), [pull],
			 [ stdout(pipe(Out)),
			   stderr(pipe(Error))
			 ]),
read_stream_to_codes(Out, OutCodes),
read_stream_to_codes(Error, ErrorCodes),
close(Out),
close(Error).

findall(C,
		(   between(1, 1000, X),
		    C is "a" + X mod 26
		), S)

:- use_module(library(lambda)).

setify(L, Set) :-
    foldl(\X^Y^Z^(memberchk(X, Y)->Z=Y; append(Y, [X], Z)), L, [], Set).

:- ['reader.pl'].
:- ['lexer.pl'].
:- ['parser.pl'].
:- ['scheme_runtime.pl']. 

scheme(A) :- 
    file(A, Content), 
    getTokens(Content, Tokens), 
    parser(Tokens, Statements),
    createSymbolTable(Statements, SymbolTable),
    calculate(Statements, SymbolTable).

%% The Unix passwd file is a file with records spanning a single line each. The fields are separated by a single `:' character. Here is an example of a line: 
%% joe:hgdu3r3bce:53:100:Joe Johnson:/users/joe:/bin/bash

%%  The following call defines a table for it: 
?- new_table('/etc/passwd',
             [ user(atom),
               passwd(code_list),
               uid(integer),
               gid(integer),
               gecos(code_list),
               homedir(atom),
               shell(atom)
             ],
             [ field_separator(0':)
             ],
             H).

% To find all people of group 100, use: 
?- findall(User, in_table(H, [user(User), gid(100)], _), Users).

% list comprehesion
%% List of Pythagorean triples : 
%% ?- V <- {X, Y, Z & X <- 1..20, Y <- X..20, Z <- Y..20 & X*X+Y*Y =:= Z*Z}.
%% V = [ (3,4,5), (5,12,13), (6,8,10), (8,15,17), (9,12,15), (12,16,20)] ;
%% false.

%% List of double of x, where x^2 is greater than 50 : 
%% ?- V <- {Y & X <- 1..20 & X*X > 50, Y is 2 * X}.
%% V = [16,18,20,22,24,26,28,30,32,34,36,38,40] ;
%% false.

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


format('~`2t~9|~n').
%222222222

:- if(statistics(gctime, _)).
get_performance_stats(GC, T):-
	statistics(gctime, GC),		% SWI-Prolog
	statistics(cputime, T).
:- else.
get_performance_stats(GC, T):-
	statistics(garbage_collection, [_,_,TGC]),
	statistics(cputime, [TT,_]),
	GC is TGC / 1000,
	T is TT / 1000.
:- endif.

with_output_to(atom(Atom), maplist(write, [a+b, b+c])).

person('Alex', 'McBrien', male).
person('Daniel', 'Gardner', male).
person('Abbas', 'Phillips', male).
person('Paul', 'Pietzuch', male).

nameList(List):-
  nameList([], List).

nameList(IList, List):-
  (
   call(person(FName, SName, _)),
   \+ (member((SName, FName), IList))
  )-> nameList([(SName, FName)|IList], List) ; List=IList.

%Procedure nameList/1 just calls nameList/2 with an empty accumulator. Then procedure nameList/2 will call every person from the facts database and check whether the person is in the accumulator list. If it finds one such person then it recursively calls itself adding this person to the accumulator. If it does not find any person not in the input list then it unifies this accumulator with the output List of persons.

% computation of the position of the bowl.
calc(Ang, Len, X, Y) :-
	X is Len * cos(Ang)+ 250,
	Y is Len * sin(Ang) + 20.
 
 
% computation of the next angle
% if we reach 0 or pi, delta change.
next_Angle(A, D, NA, ND) :-
	NA is D + A,
	(((D > 0,   abs(pi-NA) < 0.01); (D < 0, abs(NA) < 0.01))->
	  ND = - D;
	  ND = D).

assert((fun(X, Y) :- Y is 2 * X)).
?- maplist(fun, [1,2,3,4,5], L).
L = [2,4,6,8,10].

?- append([1,2,3],[4,5,6],R).
R = [1, 2, 3, 4, 5, 6].

repeat,
	write('Your guess : '),
	read(Guess),
	(   study(Solution, Guess, Bulls, Cows)
	->  format('Bulls : ~w Cows : ~w~n', [Bulls, Cows]),
	    Bulls = LenGuess
	;   digits(Digits), Max is Digits + 1,
	    format('Guess must be of ~w digits between 1 and ~w~n',
		   [LenGuess, Max]),
	    fail).

numlist(0, 9, L).
L = [0,1,2,...]

?- process_create('/bin/ls', [], [process(PID)]), process_wait(PID,Status).
PID=2999,
Status=exit(0).

evens(D, Es) :- findall(E, (member(E, D), E mod 2 =:= 0), Es).

fizzbuzz :-
        foreach(between(1, 100, X), print_item(X)).
 
print_item(X) :-
        (  0 is X mod 15
        -> print('FizzBuzz')
        ;  0 is X mod 3
        -> print('Fizz')
        ;  0 is X mod 5
        -> print('Buzz')
        ;  print(X)
        ),
        nl.

:- use_module(library(error)).
:- dynamic arc/2.

load_arcs(File) :-
        retractall(arc(_,_)),
        open(File, read, Stream),
        call_cleanup(load_arcs(Stream),
                     close(Stream)).

load_arcs(Stream) :-
        read(Stream, T0),
        load_arcs(T0, Stream).

load_arcs(end_of_file, _) :- !.
load_arcs(arc(From, To), Stream) :- !,
        assert(arc(From, To)),
        read(Stream, T2),
        load_arcs(T2, Stream).
load_arcs(Term, Stream) :-
        type_error(arc, Term).

open("ssqNum.txt",read,F),read_stream_to_codes(F,N),write(N),close(F).

list_w([W|Ws]) --> string(W), ",", !, list_w(Ws).
list_w([W]) --> string(W).

string([]) -->  [].
string([H|T]) -->[H],string(T).

test:
?- phrase(list_w(S),"cat,dog").
S = [[99, 97, 116], [100, 111, 103]] ;
false.

%X^ means "there exists X", so the whole formula means something like "count the number of ways that permutation([1,2,3,4],X) succeeds for some X and call that number N."
aggregate(count, X^permutation([1,2,3,4], X), N).

loop :- 
	write('Type end to end'),read(Word), 
	write('Input was '),write(Word),nl, 
	(Word=end;loop).

?- X=..[write,'hello world'], call(X).
hello world X = write('hello world')

repeat, read(X), (X==end_of_file, !, fail; true).

format(atom(X),"~d",[12]).

?- with_output_to(atom(Atom), maplist(write, [a+b, b+c])).
Atom = 'a+bb+c'.

forall(member(X, ["1","2","3"]), (number_codes(Y,X), writeln(Y))).

?- atom_concat(/,X,'/abc/def').
X = 'abc/def'.

partition(=<(0), [1,-2,3,4,-8,0], X, Y).

G =.. [H,Item],G.
% can be converted to:
functor(Goal,H,1),   % unifies Goal with H(_)
arg(1,Goal,Item),    % unifies first argument of Goal with Item
call(Goal).          % use this for portability

Suits = [clubs, hearts, spades, diamonds],
Pips = [2, 3, 4, 5, 6, 7, 8, 9, 10, jack, queen, king, ace],
setof(card(Pip, Suit), (member(Suit, Suits), member(Pip, Pips)), Deck).

catch(
    forall(query(Q), (Q ->
        format('yes: ~w~n',[Q]) ;
        format('no : ~w~n',[Q]))),
    error(existence_error(procedure, _), _), format('error occurred.~n', [])).


:- use_module(library(http/http_open)).
:- use_module(library(xpath)).
 
test5(UniversityName):-
	http_open('http://en.wikipedia.org/wiki/Eotvos_Lorand_University',
		  In,
		  []),
	set_stream(In, encoding(utf8)),
	load_structure(In, HTML,
		       [ dialect(xml),
			 max_errors(-1)
		       ]),
	xpath(HTML, //h1/span(text), UniversityName).


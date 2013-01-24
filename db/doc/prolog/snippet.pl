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

.yaprc
:- multifile(library_directory/1).
library_directory('~/ftp/Prolog-inedit').

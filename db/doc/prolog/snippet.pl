
%%%
git_update -->
	{ process_create(path(git), [pull],
			 [ stdout(pipe(Out)),
			   stderr(pipe(Error))
			 ]),
	  read_stream_to_codes(Out, OutCodes),
	  read_stream_to_codes(Error, ErrorCodes),
	  close(Out),
	  close(Error)
	},
	output('', informational, OutCodes),
	output('', error, ErrorCodes).

?- process_create('/bin/ls', [], [process(PID)]), process_wait(PID,Status).
PID=2999,
Status=exit(0).

%%%
leap_year(L) :-
	partition(is_leap_year, L, LIn, LOut),
	format('leap years : ~w~n', [LIn]),
	format('not leap years : ~w~n', [LOut]).
 
is_leap_year(Year) :-
	R4 is Year mod 4,
	R100 is Year mod 100,
	R400 is Year mod 400,
	( (R4 = 0, R100 \= 0); R400 = 0 ).

%%%
%You have a bunch of places where you use univ (the =.. operator) to convert between lists and strings. Univ probably is NOT what you want here. Univ unifies a term with a list.
foo(bar, baz)  =..  [foo, bar, baz]

%%%
current_prolog_flag(encoding,X).
X = utf8.

%%%
http_replay(Log, Options) :-
	start_dispatchers(Options),
	open(Log, read, In, [encoding(utf8)]),
	call_cleanup((read(In, T0),
		      replay(T0, In)),
		     close(In)),
	join_dispatchers.

test :-
    open_null_stream(Dest),
	get_time(Now),
	call_cleanup(http_get(ClientId, Parts, _Reply,
			      [ to(stream(Dest))
			      | Options
			      ]),
		     Reason, done(Path, Reason, Now, Dest)).
	
%%%
%	Create a cookie value string with name=value, seperated by ";".
cookie_value(List, Cookie) :-
	with_output_to(string(Cookie),
		       write_cookies(List)).

write_cookies([]).
write_cookies([Name=Value|T]) :-
	format('~w=~w', [Name, Value]),
	(   T == []
	->  true
	;   format('; ', []),
	    write_cookies(T)
	).

%%%
t:- 
    empty_assoc(A0), 
    term_to_atom('$root$'/a, V1), 
    term_to_atom('$root$'/'$ui$', V2), 
    put_assoc(V1, A0, some_value, A1), 
    put_assoc(V2, A1, some_other_value, A2), 
    assertion(is_assoc(A1)), 
    assertion(is_assoc(A2)).

%%%
nothing("") --> "".
 
upper_alpha(A) --> [A], { code_type(A, upper) }.
 
lparen --> "(".
 
rparen --> ")".
 
upper_alpha_string([ A | B]) -->
    upper_alpha(A), (upper_alpha_string(B); nothing(B)).
 
parenthetic_phrase(A) -->
    lparen, upper_alpha_string(A), rparen.
 
% Here's how you use it in your application. 
% Convert string to codes before calling string_search/2.
 
string_search([], nil) :- 
    !, fail.
 
string_search(String, Atom) :-
    phrase(parenthetic_phrase(Codes), String, _), !,
    atom_codes(Atom, Codes).
 
string_search([_ | More], Atom) :-
    string_search(More, Atom).
 
 
Tested:
 
?- string_search("acme ltd (HELLO) 123", X).
X = 'HELLO'.

%%%

%There is string//1 in library(dcg/basics), exactly the same.
 
?- phrase((string(_), "(", string(S), ")", {forall(member(C,S), code_type(C, upper))} ), "hello (again) (WORLD)", _).
 
S = [87, 79, 82, 76, 68]
?- format('~s',[$S]).
WORLD

%%%
assertz(count(_,1)).
numlist(1,33,X), map_list_to_pairs(count,X,L).

%%%
% Let us now use a DCG to relate a binary tree to the in-order sequence of its node names. Let us assume a binary tree consists of leaves of the form nil and inner nodes of the form node(Name, Left, Right), where Left and Right are themselves binary trees. To obtain the in-order sequence of node names, consider: 
tree_nodes(nil) --> [].
tree_nodes(node(Name, Left, Right)) -->
    tree_nodes(Left),
    [Name],
    tree_nodes(Right).
    
%Example: 
?- phrase(tree_nodes(node(a, node(b, nil,
                                 node(c, nil, nil)),
                             node(d, nil, nil))), Ns).
Ns = [b, c, a, d].
    
%You can obtain other orders by moving the terminal [Name] in the DCG body.

%%%
predicate_property(ssq_test,X).
X = interpreted ;
X = visible ;
X = file(/media/D/qachina/db/doc/prolog/money.pl) ;
X = line_count(96) ;
X = nodebug ;
X = number_of_clauses(1) ;
X = number_of_rules(1) ;
false.
%%%

setup_call_cleanup(
	    pack_open_entry(Pack, Name, Stream),
	    read_stream_to_codes(Stream, Codes),
	    close(Stream)),
%%%

test2 :-
   atom_to_memory_file('t:-writeln(1).', H),
   open_memory_file(H, read, S, [free_on_close(true)]),
   load_files(a_name, [stream(S)]),
   close(S),
   t.
%%%

pack_version_hashes(Pack, VersionAHashesPairs) :-
	setof(SHA1, sha1_pack(SHA1, Pack), Hashes),
	map_list_to_pairs(sha1_version, Hashes, VersionHashPairs),
	keysort(VersionHashPairs, Sorted),
	group_pairs_by_key(Sorted, VersionHashesPairs),
	reverse(VersionHashesPairs, RevPairs),
	maplist(atomic_version_hashes, RevPairs, VersionAHashesPairs).

%%%
(   catch(pack_query(Query, Peer, Reply), E, true)
	->  format('Content-type: text/x-prolog; charset=UTF8~n~n'),
	    (   var(E)
	    ->	format('~q.~n', [true(Reply)])
	    ;	format('~q.~n', [exception(E)])
	    )
	;   format('Content-type: text/x-prolog; charset=UTF8~n~n'),
	    format('false.~n')
).
%%%
% load the multi-threaded http server
:- use_module(library(http/thread_httpd)).
% and the standard handler dispatcher
:- use_module(library(http/http_dispatch)).
 
:- use_module(library(http/html_write)).
:- use_module(library(thread_pool)).
:- use_module(server_stats).
:- use_module(library(http/http_session)).
 
:- thread_pool_create(media,   20, []).
 
start :-
   http_set_session_options([enabled(false)]),
   http_server(http_dispatch, [port(80), workers(200), timeout(1),
   keep_alive_timeout(1)]).
 
:- http_handler('/tnsf.png', give_em_the_grape, [spawn(media), priority(10)]).
 
:- http_handler(/, holder, [prefix]).
 
give_em_the_grape(Request) :-
      http_reply_file('tnsf.jpg', [cache(true)], Request).
 
holder(_Request) :-
    reply_html_page(title('Transsexual Rights'),
       div(p('Please support transsexual women\'s basic human right to live our lives.'))
    ).
 
 
:- http_handler('/admin', admin_pg , [spawn(media), priority(20)]).
 
admin_pg(_Request) :-
    reply_html_page(title('admin'),
    [\http_session_table,
     \http_server_statistics,
     \http_server_pool_table]).

%%%
?- use_module(library(http/html_write)).
true.

?- phrase(html(p('some stuff')), X).
X = [nl(2), <, p, >, nl(1), some stuff, </, p, >].

?- print_html($X).


<p>
some stuff</p>
true.

%%%
:- use_module(library(rbtrees)).
:- use_module(library(nb_rbtrees)).
 
ordkey :- rb_empty(R),
   forall(member(W, [ls,mkdir,cd,ftp]),
     ( atom_length(W, K),
       (  nb_rb_get_node(R, K, N)
       -> nb_rb_node_value(N, Ws),
          nb_rb_set_node_value(N, [W|Ws])
       ;  nb_rb_insert(R, K, [W])
       )
    )), rb_visit(R, L), writeln(L).
 
?- ordkey.
[2-[cd,ls],3-[ftp],5-[mkdir]]

%%%
binary(X) :- format('~2r~n', [X]).
main :- maplist(binary, [5,50,9000]), halt.

%%%
%bit operators
is(X, 5 << 1).
is(X, 5 >> 1).
is(X, 5 /\ 1).  % and
is(X, 5 \/ 1).  % or
is(X, 5 xor 1).
is(X, 5 \ 1).   % not

%%%
timediff(DateTime1, DateTime2, Sec) :-
        date_time_stamp(DateTime1, TimeStamp1),
        date_time_stamp(DateTime2, TimeStamp2),
        Sec is TimeStamp2 - TimeStamp1.

?- timediff(date(2001, 03, 04, 23, 0, 32, 0, -, -),
            date(2001, 03, 04, 23, 1, 33, 0, -, -), Sec).
Sec = 61.0.

%%%
%I need all subsets of a list in the order of ascending length
set_ascending_length_subset(Set, Sub) :-
           length(Set, N),
           between(0, N, L),
           length(Sub, L),
           phrase(subset(Set), Sub).
 
subset([])     --> [].
subset([L|Ls]) --> ( [L] ; []), subset(Ls).
 
?- findall(Sub, set_ascending_length_subset([a,b,c], Sub), Subs).
%@ Subs = [[], [a], [b], [c], [a, b], [a, c], [b, c], [a|...]].

%%%
add(A,B,R):-
    R is A + B.
 
mul(A,B,R):-
    R is A * B.
 
% define fold now.
fold([], Act, Init, Init).
 
fold(Lst, Act, Init, Res):-
    head(Lst,Hd),
    tail(Lst,Tl),
    apply(Act,[Init, Hd, Ra]),
    fold(Tl, Act, Ra, Res).
 
sumproduct(Lst, Sum, Prod):-
    fold(Lst,mul,1, Prod),
    fold(Lst,add,0, Sum).
 
?- sumproduct([1,2,3,4],Sum,Prod).
Sum = 10,
Prod = 24 .

%%%
:- module(load_csv,
    [load_csv_file/4
    ]).
 
:- use_module(library(csv)).
 
load_csv_file(File, Functor, Skip, ColumnTypes) :-
    FirstLine is Skip + 1,
    forall(
        load_table_row(File, Functor, ColumnTypes, Line, Row),
        (   Line >= FirstLine
        ->  assertz(Row)
        ;   true
        )
    ).
 
load_table_row(File, Functor, ColumnTypes, Line, Row) :-
    csv_read_file_row(
        File,
        R,
        [convert(false),line(Line)]
    ),
    R =.. [row|RawColumns],
    maplist(convert_field, ColumnTypes, RawColumns, Columns),
    Row =.. [Functor|Columns].
 
convert_field(atom, Field, Field).
convert_field(number, Field, Number) :-
    atom_number(Field, Number).

%%%
?- member(X," _"), setof(T,code_type(X,T),L).
X = 32,
L = [ascii, space, white, to_lower(32), to_upper(32)] ;
X = 95,
L = [ascii, csym, csymf, graph, prolog_atom_start,
prolog_identifier_continue, prolog_var_start, punct, to_lower(...)|...].

%%%
like(What) --> "I like ", list(What), ".", list(_).

list([]) --> [].
list([L|Ls]) --> [L], list(Ls).

We can use this DCG to parse a given string, which is a list of character codes:
?- phrase(like(What), "I like it. The rest is ignored").
What = [105, 116] ;
false.

As expected, What is unified with the character codes for i and t.

Using library(pio), we can transparently parse from a file with the same DCG. Assume that the file 'like.txt' starts with the string "I like it."
?- use_module(library(pio)).
true.

?- phrase_from_file(like(What), 'like.txt').
What = [105, 116] ;
false.

%%%
singleassignment:-                   
    functor(Array,array,100), % create a term with 100 free Variables as arguments
                              % index of arguments start at 1
    arg(1 ,Array,a),          % put an a at position 1 
    arg(12,Array,b),          % put an b at position 12
    arg(1 ,Array,Value1),     % get the value at position 1
    print(Value1),nl,         % will print Value1 and therefore a followed by a newline 
    arg(4 ,Array,Value2),     % get the value at position 4 which is a free Variable
    print(Value2),nl. % will print that it is a free Variable followed by a newline

%%%
binary(X) :- format('~2r~n', [X]).
main :- maplist(binary, [5,50,9000]), halt.

%%%
:- use_module(library(clpfd)).
 
circle :-
	bagof([X,Y], init(X,Y), BL),
	length(BL, N),
	length(L, 100),
	maplist(choose(BL, N), L),
	draw_circle(L). 

% point selection
choose(BL, N, V) :-
	I is random(N),
	nth0(I, BL, V).
 
% to find all couples of numbers verifying 
% 100 <= x^2 + y^2 <= 225
init(X1, Y1) :-
	X in -15..15,
	Y in -15..15,
	X*X + Y*Y #>= 100,
	X*X + Y*Y #=< 225,
	label([X,Y]),
	X1 is 10 * X + 200,
	Y1 is 10 * Y + 200.
 
 
draw_circle(L) :-
	new(D, window('Circle')),
	send(D, size,size(400,400)),
	forall(member([X,Y], L),
	       (   new(C, circle(4)),
		   send(C, fill_pattern, colour(@default, 0, 0, 0)),
		   send(C, center(point(X,Y))),
		   send(D, display, C))),
	send(D, open).

%%%
read_file_to_codes('/media/D/qachina/db/doc/money/ssqHitNum.txt', X, []).

%%%
match_regex(Regex, String, Result) :-
    send(Regex, match, String),
    findall(W, (between(1,9, Z),
                 get(Regex, register_value, String, Z, name, W)),
             Result), !.
 
amatchesregexb(Astring, Bregex, Result) :-
    use_module(library(pce)),
    setup_call_cleanup(new(Binternalregex, regex(Bregex)),
               match_regex(Binternalregex, Astring, Result),
               free(Binternalregex)).
 
However, when I used it I got the following results:
 
130 ?- amatchesregexb('ab12cd', '.*([0-9]+).*', Result).
Result = ['2'].
 
131 ?- amatchesregexb('ab12cd', '[^0-9]*([0-9]+).*', Result).
Result = ['12'].
 
132 ?-
 
% I expected the first example capture **([0-9]+) to cap**ture '12', but
% it only captured '2'.*
 
% I had to add [^0-9] in front of the numeric capture to successfully
% capture both digits '12'.

% This regex behavior is correct and isn't specific to Prolog.  A * matches
% greedily.  That is, it consumes as many characters as possible while still
% allowing the whole pattern to match.  In the first example, .* can consume
% the 1 and the entire regex still matches.  Using [^0-9]* prevents that part
% of the pattern from matching the 1.

%------
%to create a stand-alone executable that starts by executing main/0 and for which the source is loaded through load.pl, use the command
swipl --goal="server(7000)" --stand_alone=true --foreign=save -o server_pl
-c server.pl

swipl --goal=main --stand_alone=true --quiet -o myprog -c load.pl

% This performs exactly the same as executing 
swipl
?- [load].
?- qsave_program(myprog,
                 [ goal(main),
                   stand_alone(true)
                 ]).
?- halt.

swipl-ld -o calc calc.c calc.pl -lgmp -lrt -lm -lncurses -ldl -lunwind

%%%
?- debug_message_context(+time).
 
%Using strace on the threaded program is a bit tricky, but can be done by selecting one of the HTTP threads and getting its thread-id using e.g.,
 
?- thread_signal('httpd@5555_2',
	   (current_prolog_flag(system_thread_id, P),
	    writeln(P))).
 
%That writes (in my case): 14776. 
%Now, use (in another terminal): 
% strace -e '!futex' -T -p 14776

%%%
phrase(utf8_codes("é"), L),
phrase(utf8_codes(L), L2),
forall(member(C,L2), format(' ~8r', [C])).

%%%
:- meta_predicate
	monitor(0, +, -).
 
monitor(Goal, MaxTime, Status) :-
	thread_self(Self),
	term_variables(Goal, Vars),
	thread_create(
	    run(Goal, Vars, Self, Id),
	    Id,
	    [ global(50),
	      local(50)
	    ]),
	monitor_thread(Goal, Vars, Id, MaxTime, 0, Status).
 
run(Goal, Vars, Main, Me) :-
	call_cleanup(Goal, thread_send_message(Main, done(Me, Vars))).
 
 
monitor_thread(Goal, Vars, Id, MaxTime, UsedTime, Status) :-
	thread_self(Self),
	Left is MaxTime-UsedTime,
	(   thread_get_message(Self, done(Id, Vars), [timeout(Left)])
	->  thread_join(Id, Status)
	;   thread_statistics(Id, cputime, CPU),
	    (	CPU > MaxTime
	    ->  thread_signal(Id, abort),
		thread_join(Id, _),
		Status = timeout
	    ;   monitor_thread(Goal, Vars, Id, MaxTime, CPU, Status)
	    )
	).
 
%There is a lot of room to tweak on this.  Here are some goals:
 
?- monitor(numlist(1, 1000, L), 0.1, Status).
L = [1, 2, 3, 4, 5, 6, 7, 8, 9|...],
Status = true.
 
?- monitor((numlist(1, 1000, L),fail), 0.1, Status).
Status = false.
 
?- monitor((numlist(1, 1000, L),A is 1/0), 0.1, Status).
Status = exception(error(evaluation_error(zero_divisor), context(prolog_stack([frame(5, call(system: (is)/2), _G2585 is 1/0), frame(3, clause(<clause>(0x2200e00), 5), setup_call_catcher_cleanup(system:true, user: ..., _G2603, user: ...)), frame(0, meta_call, 0)]), _G2566))).
 
%[ Note that you get these details by using library(prolog_stack) ].
 
?- monitor(true, 0.1, Status).
Status = true.
 
?- time(forall(between(1, 1000, _), monitor(true, 0.1, Status))).
% 9,002 inferences, 0.030 CPU in 0.042 seconds (72% CPU, 297799 Lips)
true.
 
?- monitor((repeat, fail), 0.1, Status).
Status = timeout.
 
%The only weird thing is that
 
?-time(monitor((repeat, fail), 0.1, Status)).
 
%seems to deadlock.  I'll try to find out why.
%Found the deadlock (which was due to a bug in the code below).  I've
%extended it a bit, gave it a (hopefully) reasonable name and uploaded
%it as a pack.

% pack_install(Pack, [interactive(false),upgrade(true)|InstallOptions])
?- pack_install(resbound).
?- [library(resource_bounds)].
?- resource_bounded_call(numlist(1, 1000000, L), 1, Status, [global(1000)]).
Status = stack_overflow(global).
 
%More examples, see http://www.swi-prolog.org/pack/list?p=resbound
 
%Let us finish with the overhead:
 
?- time(forall(between(1, 10000, _),
                 resource_bounded_call((repeat, fail), 0.001, Status, []))).
% 290,164 inferences, 0.833 CPU in 11.474 seconds (7% CPU, 348396 Lips)
true.

%%%

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

%%%
?- d(sin(x^2)+5,x,Y).
Y = cos(x ^ 2) * (1 * 2 * x ^ 1) + 0

d(U+V,X,DU+DV) :- !, 
    d(U,X,DU),
    d(V,X,DV).
d(U-V,X,DU-DV) :- !,
    d(U,X,DU),
    d(V,X,DV).
d(U*V,X,DU*V+U*DV) :- !,
    d(U,X,DU),
    d(V,X,DV).
d(U/V,X,(DU*V-U*DV)/(^(V,2))) :- !,
    d(U,X,DU),
    d(V,X,DV).
d(^(U,N),X,DU*N*(^(U,N1))) :- !, 
    integer(N),
    N1 is N-1,
    d(U,X,DU).
d(-U,X,-DU) :- !,
    d(U,X,DU).
d(exp(U),X,exp(U)*DU) :- !,
    d(U,X,DU).
d(log(U),X,DU/U) :- !,
    d(U,X,DU).
d(sin(U),X,cos(U)*DU):-!,
    d(U,X,DU).
d(cos(U),X,-sin(U)*DU):-!,
    d(U,X,DU).

d(X,X,1) :- !.
d(_,_,0).

%------

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

% Procedure nameList/1 just calls nameList/2 with an empty accumulator. Then procedure nameList/2 will call every person from the facts database and check whether the person is in the accumulator list. If it finds one such person then it recursively calls itself adding this person to the accumulator. If it does not find any person not in the input list then it unifies this accumulator with the output List of persons.

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
  ) -> nameList([(SName, FName)|IList], List) ; List=IList.

% computation of the position of the bowl.
calc(Ang, Len, X, Y) :-
	X is Len * cos(Ang)+ 250,
	Y is Len * sin(Ang) + 20.

% computation of the next angle if we reach 0 or pi, delta change.
next_Angle(A, D, NA, ND) :-
	NA is D + A,
	(((D > 0, abs(pi-NA) < 0.01); (D < 0, abs(NA) < 0.01)) ->
	  ND = -D;
	  ND = D).

assert((fun(X, Y) :- Y is 2 * X)).
?- maplist(fun, [1,2,3,4,5], L).
L = [2,4,6,8,10].

?- append([1,2,3],[4,5,6],R).
R = [1, 2, 3, 4, 5, 6].

%%%
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

evens(D, Es) :- findall(E, (member(E, D), E mod 2 =:= 0), Es).

%%%
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

%%%
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

%%%
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

%%%
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

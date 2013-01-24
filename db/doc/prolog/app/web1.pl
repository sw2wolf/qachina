:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_log)).

:- http_handler(root(handle), handle_rpc, []).

:- set_setting(http:logfile, '/home/sw2wolf/www/log.txt').

http_json:json_type('application/json').

% sample prolog program
f1(1).
f1(2).
f1(4).
f1(5).
f2(3).
f2(4).
f2(6).

server(Port) :-
    http_server(http_dispatch, [port(Port)]).

handle_rpc(Request) :-
     http_read_json(Request, JSONIn),
     json_to_prolog(JSONIn, PrologIn),
     evaluate(PrologIn, PrologOut), % application body
     PrologOut = JSONOut,
     reply_json(JSONOut).

evaluate(PrologIn, PrologOut) :-
     PrologIn = json([args=Query, password=PW, method=MethodName]),
     MethodName = eval,
     PW = blah,
     atom_to_term(Query, Term, Bindings),
     Goal =.. [findall, Bindings, Term, IR],
     call(Goal),
     sort(IR, Result),
     Result = PrologOut,
     format(atom(StringResult), "~q", [Result]),
     PrologOut = json([result=StringResult]).
    


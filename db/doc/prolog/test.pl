:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).

%:- http_handler(/, say_hi, []).
:- http_handler(root(.), say_hi, []).

% And, just for clarity, define a second handler
% this one can by reached at http://127.0.0.1:8000/taco
:- http_handler(root(taco), say_taco, []).

server(Port) :-
	http_server(http_dispatch, [port(Port)]).

say_hi(_Request) :-
    format('Content-type: text/html~n~n'),
    format('<html><head><title>Howdy</title></head><body><h2>A Simple Web Page</h2><p>With some text.</p></body></html>~n').


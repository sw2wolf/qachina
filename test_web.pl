:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).

:- use_module(http_cgi).

:- http_handler(root(hello1), say_hi1, []).
:- http_handler(root(hello2), say_hi2, []).

server(Port) :-
	http_server(http_dispatch, [port(Port)]).

say_hi1(_Request) :-
	format('Content-type: text/plain~n~n'),
	format('Hello World!~n').

say_hi2(_Request) :-
	reply_html_page(title('Hello World'),
			[ h1('Hello World'),
			  p(['This example demonstrates generating HTML ',
			     'messages from Prolog'
			    ])
			]).

cgi_bin(_) :- 'nav.py' .

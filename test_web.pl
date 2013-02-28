:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
 
:- use_module(library(http/http_files)).
:- use_module(http_cgi).

%  css location is already defined by the libs as serving files
%  from the file path alias css
%  So, we add an additional
%  file path to the file path alias css rather than defining our
%  own handler for css
%
%  root(css) is a nono because root isn't a defined file path  8cD
 
user:file_search_path(css, css).
user:file_search_path(cgi_bin, '/media/D/qachina/cgi-bin/').

http:location(files, '/f', []).
http:location(css, '/css', []).
 
% this serves files from the directory db under the working directory
:- http_handler(files(.), http_reply_from_files('db', []), [prefix]).
:- http_handler(css(.), http_reply_from_files('css', []), [prefix]).
 
server(Port) :-
	http_server(http_dispatch, [port(Port)]).

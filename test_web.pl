:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(http_cgi).

:- encoding(utf8).

% flag to ensure we only start server once
:- dynamic started/0.

user:file_search_path(cgi_bin, '/media/D/qachina/cgi-bin/').
user:file_search_path(document_root, '/media/D/qachina/').

% :- multifile
% 	http_cgi:environment/2.

http_cgi:environment('PATH', '/bin:/usr/bin:/usr/local/bin').

:- http_handler(root(.), serve_page(document_root),	[prefix]).

server(Port) :-
	started,!,
	format(user_error, 'Already running - browse http://127.0.0.1:~w/\n', [Port]).

server(Port) :-
	http_server(http_dispatch, [port(Port), timeout(3600)]),
	assert(started).

%%	serve_page(+Alias, +Request)
%
%	HTTP handler for files below document-root.
serve_page(Alias, Request) :-
	memberchk(path_info(Relative), Request),
	Spec =.. [ Alias, Relative ],
	http_safe_file(Spec, []),
	find_file(Relative, File), !,
	http_reply_file(File, [unsafe(true)], Request).
serve_page(Alias, Request) :-
	\+ memberchk(path_info(_), Request), !,
	serve_page(Alias, [path_info('index.html')|Request]).
serve_page(_, Request) :-
	memberchk(path(Path), Request),
	existence_error(http_location, Path).

%%	find_file(+Relative, -File) is det.
%
%	Translate Relative into a File in the document-root tree. If the
%	given extension is .html, also look for   .txt files that can be
%	translated into HTML.
find_file(Relative, File) :-
	absolute_file_name(document_root(Relative),
			   File,
			   [ access(read),
			     file_errors(fail)
			   ]), !.
find_file(Relative, File) :-
	file_name_extension(Base, html, Relative),
	file_name_extension(Base, txt, WikiFile),
	absolute_file_name(document_root(WikiFile),
			   File,
			   [ access(read),
			     file_errors(fail)
			   ]), !.
find_file(Relative, File) :-
	absolute_file_name(document_root(Relative),
			   File,
			   [ access(read),
			     file_errors(fail),
			     file_type(directory)
			   ]), !.

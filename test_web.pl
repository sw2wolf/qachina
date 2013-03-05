:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
 
:- use_module(library(http/http_files)).
:- use_module(http_cgi).

% flag to ensure we only start server once
:- dynamic started/0.

%  css location is already defined by the libs as serving files
%  from the file path alias css
%  So, we add an additional
%  file path to the file path alias css rather than defining our
%  own handler for css
%
%  root(css) is a nono because root isn't a defined file path  8cD
 
user:file_search_path(css, css).
user:file_search_path(cgi_bin, '/media/D/qachina/cgi-bin/').
user:file_search_path(document_root, '/media/D/qachina/').

http:location(files, '/f', []).
http:location(css, '/css', []).

:- multifile
	http_cgi:environment/2.

%% http_cgi:environment('PROJECT_ROOT', Root) :-		% gitweb
%% 	git_project_root(Root).
%% http_cgi:environment('GIT_PROJECT_ROOT', Root) :-	% git-http
%% 	git_project_root(Root).
%% http_cgi:environment('GITWEB_CONFIG', Config) :-
%% 	absolute_file_name(gitweb('gitweb.conf'), Config,
%% 			   [ access(read)
%% 			   ]).
http_cgi:environment('PATH', '/bin:/usr/bin:/usr/local/bin').

:- http_handler(root(.), serve_page(document_root),
		[prefix, priority(10), spawn(wiki)]).
 
% this serves files from the directory db under the working directory
:- http_handler(files(.), http_reply_from_files('db', []), [prefix]).
:- http_handler(css(.), http_reply_from_files('css', []), [prefix]).

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
	%serve_file(File, Request).
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
			     file_errors(fail)
			   ]).
find_file(Relative, File) :-
	absolute_file_name(document_root(Relative),
			   File,
			   [ access(read),
			     file_errors(fail),
			     file_type(directory)
			   ]).

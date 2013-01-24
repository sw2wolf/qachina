:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).

http:location(images, root(images), []).
user:file_search_path(icons, '/home/carlo/prolog').
:- http_handler(images(.), serve_files_in_directory(icons), [prefix]).

server(Port) :-
    http_server(http_dispatch, [port(Port)]).

:- http_handler(root(.), render_base, []).
:- http_handler('/form', process_form, []).

process_form(Request) :-
    http_parameters(Request,
            [name(Name,[atom])]),
            reply_html_page('Posted data: ',['',Name]).

render_base(_Request) :-
    reply_html_page(
        title('Naslov'),
        img([src='image/pi.jpg', alt='PI'])
    ).


intro -->
    html([p(ol([li('select a path'),
            li('scan resources from it'),
            li('RDF-ize them'),
            li('browse with foldable SVG')
           ])),
          \sep,
          'png before', img(src='images/swipl.png'), 'png after',
          \sep,
          'jpeg before', img(src='/images/swipl.jpeg'), 'jpeg after'
         ]).

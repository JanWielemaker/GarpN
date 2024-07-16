:- module(gui,
          []).
:- use_module(library(main)).
:- use_module(library(http/http_server)).
:- use_module(library(http/http_files)).
:- use_module(library(http/htmx)).
:- use_module(library(option)).

http:location(htmx, root(htmx), []).

:- initialization(main, main).

main(Argv) :-
    argv_options(Argv, _Pos, Options),
    http_server(Options),
    (   option(interactive(true), Options)
    ->  cli_enable_development_system
    ;   thread_get_message(quit)
    ).

:- http_handler(root(.),    http_redirect(see_other, root(home)), []).
:- http_handler(root(home), home, []).
:- http_handler(root(.),
                http_reply_from_files(web, [not_found(404)]),
                [prefix]).
:- http_handler(root('node_modules/'),
                http_reply_from_files(node_modules, [not_found(404)]),
                [prefix]).

home(_Request) :-
    reply_html_page([ title('Garp numerical simulator'),
                      link([rel(stylesheet), href('simulator.css')]),
                      link([rel(icon), type('image/png'), sizes('32x32'),
                            href('https://www.swi-prolog.org/icons/favicon.ico')])
                    ],
                    [ \home,
                      script([type('text/javascript'), src('/node_modules/htmx.org/dist/htmx.js')], []),
%                     script([type('text/javascript'), src('simulator.js')], []),
                      script([type('text/javascript'), src('plotly-2.32.0.min.js')], [])
                    ]).

home -->
    html(h1("Hello")).

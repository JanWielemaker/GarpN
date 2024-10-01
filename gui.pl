:- module(gui,
          []).
:- use_module(library(main)).
:- use_module(library(http/http_server)).
:- use_module(library(http/http_files)).
:- use_module(library(http/htmx)).
:- use_module(library(http/http_json)).
:- use_module(library(option)).
:- use_module(library(http/js_write)).
:- use_module(library(apply)).
:- use_module(library(dicts)).
:- use_module(library(readutil)).

:- use_module(gsim).

http:location(htmx, root(htmx), []).

:- initialization(main, main).
:- dynamic model_file/1.

main(Argv) :-
    argv_options(Argv, Pos, Options),
    set_model_file(Pos),
    http_server(Options),
    (   option(interactive(true), Options)
    ->  cli_enable_development_system
    ;   thread_get_message(quit)
    ).

opt_type(port,        port,        integer).
opt_type(p,           port,        integer).
opt_type(interactive, interactive, boolean).
opt_type(i,           interactive, boolean).

opt_help(help(usage),
         " [option..] file").

set_model_file([File]) =>
    asserta(model_file(File)).
set_model_file(_) =>
    argv_usage(debug),
    halt(1).

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
    html([ h1("Garp numerical simulator"),
           form(['hx-post'('/htmx/run'),
                 'hx-target'('#plot')
                ],
                [ \model_area,
                  div(class(controls),
                      [ label(for(iterations),
                              '# Iterations'),
                        input([ type(number),
                                name(iterations),
                                min(10),
                                value(100),
                                max(100_000)
                              ]),
                        input([ type(submit),
                                value("Run!")
                              ])
                      ])
                ]),
           div([id(plot)], []),
           \js_script({|javascript||
                       let data;
                       let layout;
                       let plot;
                      |})
         ]).

model_area -->
    { default_model(Model)
    },
    html(div(class(model),
             textarea([ name(model)
                      ], Model))).

default_model(Model) :-
    model_file(File),
    !,
    read_file_to_string(File, Model, []).
default_model("").

:- http_handler(htmx(run), run, []).

run(Request) :-
    http_parameters(Request,
                    [ iterations(Iterations, [integer]),
                      model(Model, [])
                    ]),
    Sample is ceiling(Iterations/1000),
    run(string(Model), Series,
        [ iterations(Iterations),
          sample(Sample)
        ]),
    plotly_traces(Series, Traces),
    reply_htmx(\plot(Traces)).

plot(Traces) -->
    js_script({|javascript(Traces)||
               data = Traces;
               layout = {
                         // title: "Nice plot"
                     };
               plot = Plotly.newPlot('plot', data, layout);
              |}).

plotly_traces(Series, Traces) :-
    Series = [First|_],
    dict_keys(First, Keys),
    convlist(serie(Series), Keys, Traces).

serie(Series, Key, trace{x:Times, y:Values, mode:lines, name:Key}) :-
    Key \== t,
    maplist(get_dict(t), Series, Times),
    maplist(get_dict(Key), Series, Values).


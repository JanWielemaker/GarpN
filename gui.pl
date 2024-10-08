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
:- use_module(library(pairs)).

:- use_module(gsim).
:- use_module(map).

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
                                value(1000),
                                max(100_000)
                              ]),
                        input([ type(hidden),
                                name(track),
                                value(all)
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
                      track(Track, [oneof([all,initialized]),
                                    default(initialized)]),
                      sample(Sample, [integer, optional(true)]),
                      model(Model, [])
                    ]),
    (   var(Sample)
    ->  Sample is ceiling(Iterations/1000)
    ;   true
    ),
    id_mapping(IdMapping),
    simulate(string(Model), Series,
             [ iterations(Iterations),
               track(Track),
               sample(Sample),
               id_mapping(IdMapping)
             ]),
    plotly_traces(Series, Traces),
    reply_htmx(\plot(Traces)).

plot(Traces) -->
    js_script({|javascript(Traces)||
               data = Traces;
               layout = {
                   title: "Nice plot",
                   yaxis: {
                       title: 'Value'
                          },
                   yaxis2: {
                       title: 'Derivative',
                       overlaying: 'y',
                       side: 'right'
                           }
                        };
               plot = Plotly.newPlot('plot', data, layout);
              |}).

plotly_traces(Series, Traces) :-
    Series = [First|_],
    dict_keys(First, Keys),
    convlist(serie(Series), Keys, Traces).

serie(Series, Key, trace{x:Times, y:Values, mode:lines, name:Key, yaxis:YAxis}) :-
    Key \== t,
    convlist(tv(Key), Series, TVs),
    pairs_keys_values(TVs, Times, Values),
    key_yaxis(Key, YAxis).

key_yaxis(Key, y2) :-
    sub_atom(Key, _, _, _, growth),
    !.
key_yaxis(_, y).

tv(Key, State, T-V) :-
    get_dict(t, State, T),
    get_dict(Key, State, V),
    number(V).

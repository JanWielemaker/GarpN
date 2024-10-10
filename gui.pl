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
                      script([type('text/javascript'), src('simulator.js')], []),
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
                        ' ',
                        \methods,
                        input([ type(hidden),
                                name(track),
                                value(all)
                              ]),
                        ' ',
                        input([ type(submit),
                                value("Run!")
                              ])
                      ])
                ]),
           div(id(plot), []),
           \js_script({|javascript||
                       let data;
                       let layout;
                       let plot;
                      |})
         ]).

methods -->
    html([ label(for(method), 'Method'),
           select(name(method),
                  [ option([value(euler), selected], 'Euler'),
                    option(value(rk4),  'RK4')
                  ])
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
                      method(Method, [oneof([euler,rk4]), default(euler)]),
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
               method(Method),
               track(Track),
               sample(Sample),
               id_mapping(IdMapping)
             ]),
    js_id_mapping(IdMapping, JSMapping),
    plotly_traces(Series, VTraces, DTraces, JSMapping),
    reply_htmx([ div([id(hrule),class(ruler)], []),
                 div([id(vrule),class(ruler)], []),
                 div(id(values), []),
                 div(id(derivatives), []),
                 \plot(values, "Number of", VTraces),
                 \plot(derivatives, "Growth", DTraces),
                 \js_script({|javascript||initRulers("plot")|})
               ]).

js_id_mapping(Dict, JDict) :-
    dict_pairs(Dict, _, Pairs),
    maplist(jid, Pairs, JPairs),
    dict_pairs(JDict, #, JPairs).

jid(K-V, K-A) :-
    format(string(A), '~w', [V]).

plot(Target, Title, Traces) -->
    js_script({|javascript(Target,Title,Traces)||
               data = Traces;
               layout = { title: Title };
               plot = Plotly.newPlot(Target, data, layout);
              |}).

plotly_traces(Series, VTraces, DTraces, JSMapping) :-
    Series = [First|_],
    dict_keys(First, Keys),
    split_keys(Keys, VKeys, DKeys),
    maplist(serie(Series, JSMapping), VKeys, VTraces),
    maplist(serie(Series, JSMapping), DKeys, DTraces).

split_keys([], [], []).
split_keys([t|T], VL, DL) :-
    !,
    split_keys(T, VL, DL).
split_keys([V|T], [V|VL], DL) :-
    sub_atom(V, _, _, _, number_of),
    !,
    split_keys(T, VL, DL).
split_keys([V|T], VL, [V|DL]) :-
    sub_atom(V, _, _, _, growth),
    !,
    split_keys(T, VL, DL).

serie(Series, JSMapping, Key, trace{x:Times, y:Values, mode:lines, name:JSMapping.Key}) :-
    convlist(tv(Key), Series, TVs),
    pairs_keys_values(TVs, Times, Values).

tv(Key, State, T-V) :-
    get_dict(t, State, T),
    get_dict(Key, State, V),
    number(V).

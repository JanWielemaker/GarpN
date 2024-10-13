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

http:location(garp, root(garp), []).
http:location(htmx, garp(htmx), []).

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

:- http_handler(root(.),    http_redirect(see_other, garp(home)), []).
:- http_handler(garp(.),    http_redirect(see_other, garp(home)), []).
:- http_handler(garp(home), home, []).
:- http_handler(garp(.),
                http_reply_from_files(web, [not_found(404)]),
                [prefix]).
:- http_handler(garp('node_modules/'),
                http_reply_from_files(node_modules, [not_found(404)]),
                [prefix]).

home(_Request) :-
    reply_html_page([ title('Garp numerical simulator'),
                      link([rel(stylesheet), href('/garp/simulator.css')]),
                      link([rel(icon), type('image/png'), sizes('32x32'),
                            href('https://www.swi-prolog.org/icons/favicon.ico')])
                    ],
                    [ \home,
                      script([type('text/javascript'),
                              src('/garp/node_modules/htmx.org/dist/htmx.js')], []),
                      script([type('text/javascript'),
                              src('/garp/node_modules/htmx.org/dist/ext/response-targets.js')], []),
                      script([type('text/javascript'),
                              src('/garp/simulator.js')], []),
                      script([type('text/javascript'),
                              src('/garp/plotly-2.32.0.min.js')], [])
                    ]).

home -->
    html([ h1("Garp numerical simulator"),
           div('hx-ext'('response-targets'),
               [
           form(['hx-post'('/garp/htmx/run'),
                 'hx-target'('#results'),
                 'hx-target-500'('#errors'),
                 'hx-on-htmx-before-request'('clear_output()')
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
           div([id(errors),class(narrow)], []),
           div(id(results), []),
           \js_script({|javascript||
                       let data;
                       let layout;
                       let plot;
                      |})
         ])]).

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
    html(div(class([model,narrow]),
             textarea([ name(model),
                        id(model)
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
    Options = [ iterations(Iterations),
                method(Method),
                track(Track),
                sample(Sample),
                id_mapping(IdMapping)
              ],
    call_time(simulate(string(Model), Series, Options), Time),
    js_id_mapping(IdMapping, JSMapping),
    plotly_traces(Series, VTraces, DTraces, JSMapping),
    reply_htmx([ hr([]),
                 \stats(Series, Time),
                 \download_links(Model, Options),
                 div(id(plot),
                     [ div([id(hrule),class(ruler)], []),
                       div([id(vrule),class(ruler)], []),
                       div(id(values), []),
                       div(id(derivatives), []),
                       \traces(VTraces, DTraces)
                     ]),
                 \js_script({|javascript||initRulers("plot")|})
               ]).

js_id_mapping(Dict, JDict) :-
    dict_pairs(Dict, _, Pairs),
    maplist(jid, Pairs, JPairs),
    dict_pairs(JDict, #, JPairs).

jid(K-V, K-A) :-
    format(string(A), '~w', [V]).

traces(VTraces, []) -->
    !,
    plot(values, @(null), VTraces).
traces(VTraces, DTraces) -->
    plot(values, "Number of", VTraces),
    plot(derivatives, "Growth", DTraces).

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
split_keys([V|T], [V|VL], DL) :-
    split_keys(T, VL, DL).

serie(Series, JSMapping, Key,
      trace{x:Times, y:Values, mode:lines, name:JSMapping.get(Key, Key)}) :-
    convlist(tv(Key), Series, TVs),
    pairs_keys_values(TVs, Times, Values).

tv(Key, State, T-V) :-
    get_dict(t, State, T),
    get_dict(Key, State, V),
    number(V).

stats(Series, Time) -->
    { length(Series, Count)
    },
    html(div(class([stats,narrow]),
             'Generated ~D samples in ~3f seconds'-[Count, Time.cpu])).

:- dynamic
    saved/3.

download_links(Model, Options) -->
    { variant_sha1(Model+Options, SHA1),
      (   saved(SHA1, Model, Options)
      ->  true
      ;   asserta(saved(SHA1, Model, Options))
      ),
      http_link_to_id(csv, path_postfix(SHA1), HREF)
    },
    html(div(class([downloads,narrow]),
             [ a([href(HREF), download("garp.csv")],
                 "Download as CSV")
             ])).

:- http_handler(garp(download/csv/SHA1), download_csv(SHA1), [id(csv)]).

download_csv(SHA1, _Request) :-
    saved(SHA1, Model, Options),
    simulate(string(Model), Series, Options),
    Series = [First|_],
    dict_keys(First, Keys0),
    order_keys(Keys0, Keys),
    dicts_to_compounds(Series, Keys, dict_fill(-), Compounds0),
    maplist(round_float_row(4), Compounds0, Compounds),
    format('Content-type: text/csv~n~n'),
    option(id_mapping(IdMapping), Options, _{}),
    maplist(key_label(IdMapping), Keys, Labels),
    Title =.. [row|Labels],
    csv_write_stream(current_output, [Title|Compounds],
                     [ separator(0',)]).

key_label(_, t, "Time") :-
    !.
key_label(IdMapping, Key, Label) :-
    format(atom(Label), '~w', [IdMapping.get(Key)]),
    !.
key_label(_, Key, Key).

order_keys(Keys0, Keys) :-
    map_list_to_pairs(csv_column_rank, Keys0, Pairs),
    keysort(Pairs, PairsS),
    pairs_values(PairsS, Keys).

csv_column_rank(t, 0).
csv_column_rank(Key, 1) :- sub_atom(Key, _, _, _, number_of), !.
csv_column_rank(Key, 2) :- sub_atom(Key, _, _, _, growth), !.
csv_column_rank(_,   3).

round_float_row(N, Row0, Row) :-
    same_functor(Row0, Row),
    functor(Row0, _, Arity),
    round_float_cols(1, Arity, N, Row0, Row).

round_float_cols(I, Arity, N, Row0, Row) :-
    I =< Arity,
    !,
    arg(I, Row0, F0),
    arg(I, Row, F),
    (   float(F0)
    ->  round_float(N, F0, F)
    ;   F = F0
    ),
    I2 is I+1,
    round_float_cols(I2, Arity, N, Row0, Row).
round_float_cols(_, _, _, _, _).

round_float(_Decimals, Value, Rounded) :-
    Value =:= 0,
    !,
    Rounded = 0.0.
round_float(Decimals, Value, Rounded) :-
    Value < 0,
    !,
    round_float(Decimals, -Value, MinRounded),
    Rounded is -MinRounded.
round_float(Decimals, Value, Rounded) :-
    FracDecimals is Decimals-floor(log10(Value))-1,
    (   FracDecimals > 0
    ->  Mult is 10.0^FracDecimals,
        Rounded is round(Value*Mult)/Mult
    ;   Mult is 10.0^(-FracDecimals),
        Rounded is round(Value/Mult)*Mult
    ).

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
:- use_module(library(csv)).
:- use_module(library(lists)).
:- use_module(library(statistics)).
:- use_module(library(http/http_client)).
:- use_module(library(dcg/high_order)).

:- use_module(gsim).
:- use_module(map).
:- use_module(csv_util).

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
    { default_model(Model)
    },
    html([ h1("Garp numerical simulator"),
           div('hx-ext'('response-targets'),
               [
           form(['hx-post'('/garp/htmx/run'),
                 'hx-target'('#results'),
                 'hx-target-500'('#errors'),
                 'hx-on-htmx-before-request'('clear_output()')
                ],
                [ \model_area(Model),
                  div([id(quantity_controls), class(narrow)],
                      \q_menu(Model)),
                  div(class([controls, narrow]),
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
                       let SHA1;
                      |})
         ])]).

methods -->
    html([ label(for(method), 'Method'),
           select(name(method),
                  [ option([value(euler), selected], 'Euler'),
                    option(value(rk4),  'RK4')
                  ])
         ]).

model_area(Model) -->
    html(div(class([model,narrow]),
             textarea([ name(model),
                        id(model),
                        'hx-post'('/garp/htmx/analyze'),
                        'hx-trigger'('input changed delay:500ms'),
                        'hx-target'('#quantity_controls'),
                        placeholder('Your numerical model')
                      ], Model))).

default_model(Model) :-
    model_file(File),
    !,
    read_file_to_string(File, Model, []).
default_model("").

:- http_handler(htmx(analyze), analyze, []).
:- http_handler(htmx(run), run, []).
:- http_handler(htmx(info), info, []).

%!  analyze(+Request)
%
%   Analyze the model and fill the quantity controls.

analyze(Request) :-
    http_read_data(Request, Data, []),
    memberchk(model=Source, Data),
    reply_htmx(\q_menu(Source)).

q_menu(Source) -->
    { id_mapping(IdMapping),
      catch(read_model(string(Source), Formulas, _Constants, _State0,
                       [ id_mapping(IdMapping) ]),
            error(_,_), fail),
      dict_keys(Formulas, Keys),
      delete(Keys, t, Quantities0),
      order_keys(Quantities0, Quantities)
    },
    html(table(class(quantities),
               [ tr([th(class(quantity), 'Quantity'), th('Link to Garp')]),
                 \sequence(q_control(IdMapping), Quantities)
               ])).
q_menu(_) -->
    [].

q_control(IdMapping, Key) -->
    { key_label(IdMapping, Key, Label),
      atom_concat(d_, Key, Name)
    },
    html(tr([ th(class([quantity,name]), Label),
              td(\derivatives_select(Name))
            ])).

derivatives_select(Name) -->
    html(select(name(Name),
                [ option(value(-1), 'Off'),
                  option(value(0),  'Value only'),
                  option([value(1), selected], 'Value and 1st derivative'),
                  option(value(2),  'Value, 1st and 2nd derivative')
                ])).


%!  info(+Request)
%
%

info(Request) :-
    http_read_data(Request, Data, []),
    _{time:TimeAtom, sha1:SHA1} :< Data,
    atom_number(TimeAtom, Time),
    saved(SHA1, Model, Options),
    q_series(string(Model), QSeries, [link_garp_states(true)|Options]),
    (   phrase(info_seq(Time, States), QSeries, _)
    ->  reply_htmx(\state_table(States, Options))
    ;   reply_htmx('Could not find matching states at T=~3f'-[Time])
    ).

info_seq(Time, States) -->
    ...,
    linked(Before), not_linked_list(BL), timed(Time,State),
    not_linked_list(AL), linked(After), !,
    { append([[Before], BL, [State], AL, [After]], States) }.

linked(State) -->
    [State],
    { State.get(garp_states) = [_|_] }.

not_linked_list([]) -->
    [].
not_linked_list([H|T]) -->
    not_linked(H),
    not_linked_list(T).

not_linked(State) -->
    [State],
    { \+ State.get(garp_states) = [_|_] }.

timed(Time, State) -->
    [State],
    peek(Next),
    { State.t =< Time, Next.t >= Time }.

... --> [] ; [_], ... .
peek(X), [X] --> [X].

%!  state_table(+States, +Options)// is det.
%
%   Print an HTML table of states.

state_table(States, Options) -->
    { option(id_mapping(IdMapping), Options, _{}),
      States = [First|_],
      dict_keys(First, Keys),
      order_keys(Keys, Ordered)
    },
    html(table(class(states),
               [ tr(\sequence(state_header(1, First, IdMapping), Ordered)),
                 tr(\sequence(state_header(2, First, IdMapping), Ordered))
               | \sequence(state_row(Ordered), States)
               ])).

state_header(Nth, Row, IdMapping, Key) -->
    { Value = Row.Key,
      compound(Value),
      compound_name_arguments(Value, d, Ds)
    },
    !,
    (   {Nth == 1}
    ->  { length(Ds, Cols),
          key_label(IdMapping, Key, Label)
        },
        html(th([colspan(Cols)], Label))
    ;   derivative_headers(Ds, 0)
    ).
state_header(1, _Row, IdMapping, Key) -->
    !,
    { key_label(IdMapping, Key, Label) },
    html(th(rowspan(2), Label)).
state_header(2, _Row, _IdMapping, _Key) -->
    [].

derivative_headers([], _) -->
    [].
derivative_headers([_|T], 0) -->
    !,
    html(th('Value')),
    derivative_headers(T, 1).
derivative_headers([_|T], N) -->
    !,
    html(th('D~d'-[N])),
    {N1 is N+1},
    derivative_headers(T, N1).

state_row(Keys, Row) -->
    html(tr(\sequence(state_cell(Row), Keys))).

state_cell(Row, Key) -->
    { Value = Row.Key,
      compound(Value),
      compound_name_arguments(Value, d, Ds)
    },
    !,
    sequence(cell_value, Ds).
state_cell(Row, Key) -->
    cell_value(Row.Key).

cell_value(Value) -->
    { float(Value),
      !,
      round_float(5, Value, Rounded)
    },
    html(td(class(float), '~2f'-[Rounded])).
cell_value(Value) -->
    html(td(class(qualitative), '~p'-[Value])).


%!  run(+Request)
%
%   Run the simulation

run(Request) :-
    http_parameters(Request,
                    [ iterations(Iterations, [integer]),
                      track(Track, [oneof([all,initialized]),
                                    default(initialized)]),
                      method(Method, [oneof([euler,rk4]), default(euler)]),
                      sample(Sample, [integer, optional(true)]),
                      rulers(ShowRulers, [boolean, default(false)]),
                      derivative(D, [between(-1,3), default(1)]),
                      model(Model, [])
                    ],
                    [ form_data(Form)
                    ]),
    (   var(Sample)
    ->  Sample is ceiling(Iterations/1000)
    ;   true
    ),
    form_derivatives(Form, Derivatives),
    id_mapping(IdMapping),
    Options = [ d(D),
                match(Derivatives),
                iterations(Iterations),
                method(Method),
                track(Track),
                sample(Sample),
                id_mapping(IdMapping)
              ],
    call_time(simulate(string(Model), Series, Options), Time),
    annotate_garp_states(Series, Shapes, Options),
    js_id_mapping(IdMapping, JSMapping),
    plotly_traces(Series, VTraces, DTraces, JSMapping),
    reply_htmx([ hr([]),
                 \stats(Series, Time),
                 \download_links(Model, Options),
                 div([ id(plot),
                       'hx-vals'('js:{time: plotly_clicked_at.x, sha1:SHA1}'),
                       'hx-post'('/garp/htmx/info'),
                       'hx-trigger'('clicked-x'),
                       'hx-target'('#info')
                     ],
                     [ \rulers(ShowRulers),
                       div(id(plotly), []),
                       \traces(VTraces, DTraces, Shapes),
                       \js_script({|javascript||initShapes("plotly")|})
                     ]),
                 div([id(info),class(narrow)], [&(nbsp)])
               ]).

js_id_mapping(Dict, JDict) :-
    dict_pairs(Dict, _, Pairs),
    maplist(jid, Pairs, JPairs),
    dict_pairs(JDict, #, JPairs).

jid(K-V, K-A) :-
    format(string(A), '~w', [V]).

traces(VTraces, DTraces, Shapes) -->
    { append(VTraces, DTraces, Traces)
    },
    plot(plotly, "Number of", Traces, Shapes).

plot(Target, Title, Traces, Shapes) -->
    js_script({|javascript(Target,Title,Traces,Shapes)||
               data = Traces;
               layout = { // title: Title,
                          shapes: Shapes,
                          margin: { t: 30, b: 5 },
                          grid: { rows: 2,
                                  columns: 1,
                                  pattern: 'independent',
                                  roworder: 'top to bottom'
                                },
                          xaxis2: { matches: 'x' }
                        };
               plot = Plotly.newPlot(Target, data, layout);
              |}).

plotly_traces(Series, VTraces, DTraces, JSMapping) :-
    Series = [First|_],
    dict_keys(First, Keys),
    split_keys(Keys, VKeys, DKeys),
    maplist(serie(x-y,   Series, JSMapping), VKeys, VTraces),
    maplist(serie(x2-y2, Series, JSMapping), DKeys, DTraces).

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

serie(Axis, Series, JSMapping, Key, Trace) :-
    Trace0 = trace{x:Times, y:Values,
                   mode:lines, name:JSMapping.get(Key, Key)},
    convlist(tv(Key), Series, TVs),
    pairs_keys_values(TVs, Times, Values),
    set_axis(Axis, Trace0, Trace).

set_axis(x-y, Trace, Trace) :- !.
set_axis(X-Y, Trace0, Trace) :-
    Trace = Trace0.put(_{xaxis:X, yaxis:Y}).

tv(Key, State, T-V) :-
    get_dict(t, State, T),
    get_dict(Key, State, V),
    number(V).

		 /*******************************
		 *        MAPPING TO GARP	*
		 *******************************/

%!  form_derivatives(+FormData, -Derivatives:dict) is det.
%
%   Get the derivatives want to match for a specific quantity.
%
%   @arg Derivatives is a dict `Quantity -> Level`, where `Level`
%   is 0..3.

form_derivatives(Form, Derivatives) :-
    convlist(form_attr_derivative, Form, Pairs),
    dict_pairs(Derivatives, #, Pairs).

form_attr_derivative(Name=Value, Key-D) :-
    atom_concat(d_, Key, Name),
    atom_number(Value, D),
    between(-1, 3, D).

%!  annotate_garp_states(+Series, -Shapes, +Options) is det.

annotate_garp_states(Series, Shapes, Options) :-
    option(d(D), Options, 1),
    D > 0,
    !,
    nq_series(Series, QSeries, [link_garp_states(true)|Options]),
    plotly_shapes(QSeries, Shapes).
annotate_garp_states(_, @(null), _).

%!  plotly_shapes(+QSeries, -Shapes) is det.

plotly_shapes(QSeries, Shapes) :-
    QSeries = [First|_],
    foldl(plotly_shape, QSeries, Shapes0, First, _),
    exclude(==(null), Shapes0, Shapes).

plotly_shape(QState, Shape, Prev, QState) :-
    mapping_color(Prev, Color, Label),
    !,
    Shape = #{ type: rect,
               xref: x,
               yref: paper,
               x0: Prev.t,
               y0: 0,
               x1: QState.t,
               y1: 1,
               fillcolor: Color,
               opacity: 0.2,
               line: #{ width: 1
                      },
               label: Label,
               layer: above
             }.
plotly_shape(QState, null, _Prev, QState).

mapping_color(QState, Color, Label) :-
    mapping_color_(QState.get(garp_states), Color, Label).

mapping_color_([],  '#ff7c7d', null) :- !.
mapping_color_([State], '#7eff7d', Label) :-
    !,
    rect_label(State, Label).
mapping_color_(States,   '#7e7bff', Label) :-
    atomics_to_string(States,",",String),
    rect_label(String, Label).

rect_label(String,
           #{ text: String,
              textposition: 'top center'
            }).

rulers(false) -->
    [].
rulers(true) -->
    html([ div([id(hrule),class(ruler)], []),
           div([id(vrule),class(ruler)], []),
           \js_script({|javascript||initRulers("plot")|})
         ]).

%!  stats(+Series, +Time)// is det.
%
%   Print the statistics of the simulation

stats(Series, Time) -->
    { length(Series, Count)
    },
    html(div(class([stats,narrow]),
             'Generated ~D samples in ~3f seconds'-[Count, Time.cpu])).


		 /*******************************
		 *            DOWNLOAD		*
		 *******************************/

:- dynamic
    saved/3.

download_links(Model, Options) -->
    { variant_sha1(Model+Options, SHA1),
      (   saved(SHA1, Model, Options)
      ->  true
      ;   asserta(saved(SHA1, Model, Options))
      ),
      http_link_to_id(csv,     path_postfix(SHA1), HREF),
      http_link_to_id(csvmap,  path_postfix(SHA1), MHREF),
      http_link_to_id(csvgarp, path_postfix(SHA1), GHREF),
      JSSHA1 = SHA1
    },
    html([ \js_script({|javascript(JSSHA1)||SHA1 = JSSHA1;|}),
           div(class([downloads,narrow]),
               [ ul([ li(a([href(HREF), download("garp-num.csv")],
                           "Download as CSV")),
                      li(a([href(MHREF), download("garp-map.csv")],
                           "Mapping to Garp as CSV")),
                      li(a([href(GHREF), download("garp.csv")],
                           "Garp states as CSV"))
                    ])
               ])
         ]).

:- http_handler(garp(download/csv/SHA1), download_csv(SHA1), [id(csv)]).
:- http_handler(garp(download/csv/map/SHA1), download_map(SHA1), [id(csvmap)]).
:- http_handler(garp(download/csv/garp/SHA1), download_garp(SHA1), [id(csvgarp)]).

%!  download_csv(+SHA1, +Request) is det.
%
%   Download simulation values as CSV.

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

%!  download_map(+SHA1, +Request) is det.
%
%   Download the mapping to Garp as CSV.

download_map(SHA1, _Request) :-
    saved(SHA1, Model, Options),
    q_series(string(Model), QSeries, [link_garp_states(true)|Options]),
    option(id_mapping(IdMapping), Options, _{}),
    q_series_table(QSeries, Table, IdMapping),
    format('Content-type: text/csv~n~n'),
    csv_write_stream(current_output, Table,
                     [ separator(0',)]).

%!  download_garp(+SHA1, +Request) is det.
%
%   Download the garp states as CSV.   Currently hard-wired to only take
%   the 1st derivative.  Higher  derivatives   currently  only  harm the
%   result.

download_garp(SHA1, _Request) :-
    saved(SHA1, _Model, Options),
    findall(State-Values, qstate(State, Values, [d(1)|Options]), Pairs),
    maplist(state_into_dict, Pairs, Data),
    option(id_mapping(IdMapping), Options, _{}),
    q_series_table(Data, Table, IdMapping),
    format('Content-type: text/csv~n~n'),
    csv_write_stream(current_output, Table,
                     [ separator(0',)]).

state_into_dict(State-Dict0, Dict) :-
    Dict = Dict0.put(state, State).

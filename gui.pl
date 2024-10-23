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
    { default_model(Model, Source)
    },
    html([ h1("Garp numerical simulator"),
           div(class(narrow), \model_menu(Model)),
           div('hx-ext'('response-targets'),
               [ form(['hx-post'('/garp/htmx/run'),
                 'hx-target'('#results'),
                 'hx-target-500'('#errors'),
                 'hx-on-htmx-before-request'('clear_output()')
                ],
                [ \model_area(Source),
                  div([id(quantity_controls), class(narrow)],
                      \q_menu(Model, Source)),
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
                        input([ type(hidden), name(track), value(all) ]),
                        input([ type(hidden), name(model), id(model), value(Model) ]),
                        ' ',
                        input([ type(submit),
                                value("Run!")
                              ])
                      ])
                ]),
                 div([id(errors),class(narrow)], []),
                 div(id(results), []),
                 div(id(script), []),
                 \js_script({|javascript(Model)||
                             let model = Model;
                             let data;
                             let layout;
                             let plot;
                             let SHA1;
                            |})
               ])
         ]).

model_menu(Default) -->
    { findall(File, directory_member(numeric, File,
                                     [ extensions([pl]) ]), Files)
    },
    html(select([ 'hx-get'('/garp/htmx/set-model'),
                  'hx-trigger'(change),
                  'hx-target'('#quantity_controls'),
                  name(model)
                ],
                \sequence(model_option(Default), Files))).

model_option(Default, File) -->
    { file_base_name(File, Base),
      file_name_extension(Model, _, Base),
      (   Model == Default
      ->  T = [selected]
      ;   T = []
      )
    },
    html(option([value(Model)|T], Model)).

methods -->
    html([ label(for(method), 'Method'),
           select(name(method),
                  [ option([value(euler), selected], 'Euler'),
                    option(value(rk4),  'RK4')
                  ])
         ]).

model_area(Model) -->
    html(div(class([model,narrow]),
             textarea([ name(source),
                        id(source),
                        'hx-vals'('js:getModel()'),
                        'hx-post'('/garp/htmx/analyze'),
                        'hx-trigger'('input changed delay:1000ms'),
                        'hx-target'('#quantity_controls'),
                        placeholder('Your numerical model')
                      ], Model))).

default_model(Model, Source) :-
    model_file(File),
    !,
    file_base_name(File, Base),
    file_name_extension(Model, _, Base),
    read_file_to_string(File, Source, []).
default_model(none, "").

:- http_handler(htmx(analyze), analyze, []).
:- http_handler(htmx(run), run, []).
:- http_handler(htmx(info), info, []).
:- http_handler(htmx('set-model'), set_model, []).

%!  set_model(+Request)
%
%   Change the model

set_model(Request) :-
    http_parameters(Request, [ model(Model, []) ]),
    numeric_model_file(Model, File),
    read_file_to_string(File, Source, []),
    reply_htmx(
        [ \q_menu(Model, Source),
          \js_script({|javascript(Model,Source)||setModel(Model,Source)|})
        ]).

numeric_model_file(Model, File) :-
    format(atom(File), 'numeric/~w.pl', [Model]).


%!  analyze(+Request)
%
%   Analyze the model and fill the quantity controls.

analyze(Request) :-
    http_read_data(Request, Data, []),
    _{model: Model, source: Source} :< Data,
    reply_htmx(\q_menu(Model, Source)).

q_menu(Model, Source) -->
    { id_mapping(Model, IdMapping),
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
q_menu(_, _) -->
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
%   Print info afte the user  clicks  in   the  chart  at a certain time
%   point.

info(Request) :-
    http_read_data(Request, Data, []),
    _{time:TimeAtom, sha1:SHA1} :< Data,
    atom_number(TimeAtom, Time),
    saved(SHA1, Model, Options),
    q_series(string(Model), QSeries,
             [ link_garp_states(true),
               garp_states(QStates)
             | Options
             ]),
    (   phrase(info_seq(Time, States0), QSeries, _)
    ->  add_garp_states(States0, QStates, States),
        reply_htmx(\state_table(States, Options))
    ;   phrase((...,timed(Time,State),...), QSeries, _),
        add_garp_states(State, QStates, States)
    ->  reply_htmx(\state_table(States, Options))
    ;   reply_htmx('Could not find matching states at T=~3f'-[Time])
    ).

%!  info_seq(+Time, -States)// is semidet.
%
%   True when States is a sequence   of  qualitative states derived from
%   the model, where the first and last   states  are linked to Garp and
%   the sequence contains Time.

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

%!  add_garp_states(+QStates, +GarpStates:list(pair(integer,dict)),
%!                  -States) is det.

add_garp_states(QStates, GStates, States), is_list(QStates) =>
    once(append([First|Skipped], [Last], QStates)),
    min_list(First.garp_states, Min),
    max_list(Last.garp_states, Max),
    include(in_state_range(Min,Max), GStates, GStates1),
    maplist(add_state_no, GStates1, GStates2),
    maplist(align_keys(First), GStates2, GStates3),
    interpolate(Skipped, GStates3, Compare),
    append([[First],Compare,[Last]], States).
add_garp_states(QState, GStates, States),
    is_dict(QState), QState.garp_states = [_|_] =>
    include(in_state_set(QState.garp_states), GStates, GStates1),
    maplist(add_state_no, GStates1, GStates2),
    maplist(align_keys(QState), GStates2, GStates3),
    States = [QState|GStates3].
add_garp_states(QState, _GStates, States),
    is_dict(QState), QState.garp_states == [] =>
    States = [QState].                  % todo: find candidates.

in_state_range(Min, Max, Id-_) :-
    Id > Min,
    Id < Max.

in_state_set(Set, Id-_) :-
    memberchk(Id, Set).

add_state_no(Id-State0, State) :-
    State = State0.put(garp_states, Id).

align_keys(First, GState0, GState) :-
    dict_pairs(GState0, Tag, GPairs0),
    convlist(align_key(First), GPairs0, GPairs),
    dict_pairs(GState, Tag, GPairs).

align_key(QState, K-V0, K-V) :-
    QV = QState.get(K),
    !,
    align_derivatives(QV, V0, V).

align_derivatives(QV, V0, V),
    compound(QV),
    compound_name_arguments(QV,d,QD),
    compound_name_arguments(V0,d,D0) =>
    length(QD, L),
    length(D1, L),
    append(D1, _, D0),
    compound_name_arguments(V,d,D1).
align_derivatives(_QV, V0, V) =>
    V = V0.

interpolate([], L, R) =>
    R = L.
interpolate(L, [], R) =>
    R = L.
interpolate([H0|T0], [H1|T1], R) =>
    R = [H0,H1|T],
    interpolate(T0, T1, T).


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
               | \state_rows(States, Ordered)
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
    { h_label(value, Label) },
    html(th(Label)),
    derivative_headers(T, 1).
derivative_headers([_|T], N) -->
    !,
    { h_label(d(N), Label) },
    html(th(Label)),
    {N1 is N+1},
    derivative_headers(T, N1).

state_rows([], _) -->
    [].
state_rows([S,G|T], Keys) -->
    { \+ is_garp_row(S),
      is_garp_row(G),
      !,
      state_row(Keys, S, _, SCells),
      state_row(Keys, G, _, GCells),
      pairs_keys_values(Pairs, SCells, GCells)
    },
    html(tr(class(simulation), \sequence(cmp_value(1), Pairs))),
    html(tr(class(garp),       \sequence(cmp_value(2), Pairs))),
    state_rows(T, Keys).
state_rows([H|T], Keys) -->
    { state_row(Keys, H, _, Cells) },
    html(tr(\sequence(cell_value([]), Cells))),
    state_rows(T, Keys).

is_garp_row(Row) :-
    \+ _Time = Row.get(t).

cmp_value(1, S-G) -->
    { S == G },
    !,
    cell_value([class(match),rowspan(2)], S).
cmp_value(2, S-G) -->
    { S == G },
    !.
cmp_value(1, (C-T)-_) -->
    { no_cmp_column(C) },
    !,
    cell_value([class([simulation])], C-T).
cmp_value(2, _-(C-T)) -->
    { no_cmp_column(C) },
    !,
    cell_value([class([garp])], C-T).
cmp_value(1, S-_) -->
    !,
    cell_value([class([nomatch,simulation])], S).
cmp_value(2, _-G) -->
    cell_value([class([nomatch,garp])], G).

no_cmp_column(t).
no_cmp_column(garp_states).


cell_value(Attrs, _K-Value) -->
    { var(Value) },
    html(td(Attrs, &(nbsp))).
cell_value(Attrs, _K-Value) -->
    { float(Value),
      !,
      round_float(5, Value, Rounded),
      join_attrs([class(float)], Attrs, Attrs1)
    },
    html(td(Attrs1, '~2f'-[Rounded])).
cell_value(Attrs, _K-Value) -->
    { clause(v_label(Value,_,_),_),
      !,
      join_attrs([class(qualitative)], Attrs, Attrs1)
    },
    html(td(Attrs1, \v_label(Value))).
cell_value(Attrs, garp_states-[]) -->
    { join_attrs([class(['garp-link', nomatch])], Attrs, Attrs1)
    },
    html(td(Attrs1, '?')).
cell_value(Attrs, garp_states-[Match]) -->
    { join_attrs([class(['garp-link', match])], Attrs, Attrs1)
    },
    html(td(Attrs1, '~p'-[Match])).
cell_value(Attrs, garp_states-Value) -->
    { Value = [_,_|_],
      !,
      atomics_to_string(Value, ",", Label),
      join_attrs([class(['garp-link', ambiguous])], Attrs, Attrs1)
    },
    html(td(Attrs1, Label)).
cell_value(Attrs, garp_states-Value) -->
    { integer(Value),
      !,
      join_attrs([class(['garp-link', garp])], Attrs, Attrs1)
    },
    html(td(Attrs1, '~p'-[Value])).
cell_value(Attrs, _K-Value) -->
    { join_attrs([], Attrs, Attrs1)
    },
    html(td(Attrs1, '~p'-[Value])).

join_attrs(Attrs0, Attrs1, [class(C)|Attrs]) :-
    select(class(C0), Attrs0, Attrs00),
    select(class(C1), Attrs1, Attrs10),
    !,
    flatten([C0, C1], C),
    append(Attrs00, Attrs10, Attrs).
join_attrs(Attrs0, Attrs1, Attrs) :-
    append(Attrs0, Attrs1, Attrs).

h_label(d(1), '\U0001D4ED¹').
h_label(d(2), '\U0001D4ED²').
h_label(d(3), '\U0001D4ED³').
h_label(value, '\U0001D4E5').

v_label(plus) --> html(span(class(plus), '\u25B2')).
v_label(min)  --> html(span(class(min),  '\u25BC')).
v_label(zero) --> html(span(class(zero), '0')).

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
                      source(Source, []),
                      model(Model, [])
                    ],
                    [ form_data(Form)
                    ]),
    (   var(Sample)
    ->  Sample is ceiling(Iterations/1000)
    ;   true
    ),
    form_derivatives(Form, Derivatives),
    id_mapping(Model, IdMapping),
    Options = [ model(Model),
                d(D),
                match(Derivatives),
                iterations(Iterations),
                method(Method),
                track(Track),
                sample(Sample),
                id_mapping(IdMapping)
              ],
    call_time(simulate(string(Source), Series, Options), Time),
    annotate_garp_states(Series, Shapes, Options),
    js_id_mapping(IdMapping, JSMapping),
    plotly_traces(Series, VTraces, DTraces, JSMapping),
    reply_htmx([ hr([]),
                 \stats(Series, Time),
                 \download_links(Source, Options),
                 div([ id(plot),
                       'hx-vals'('js:{time: plotly_clicked_at.x, sha1:SHA1}'),
                       'hx-post'('/garp/htmx/info'),
                       'hx-trigger'('clicked-x'),
                       'hx-target'('#info')
                     ],
                     [ \rulers(ShowRulers),
                       div([id(plotly),class(plotly)], []),
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
                          hovermode: "x",
                          /* grid: { rows: 1,
                                  columns: 1,
                                  pattern: 'independent',
                                  roworder: 'top to bottom'
                                },
                          xaxis2: { matches: 'x' } */
                        };
               plot = Plotly.newPlot(Target, data, layout);
              |}).

plotly_traces(Series, Traces, [], JSMapping) :-
    Series = [First|_],
    dict_keys(First, Keys0),
    delete(Keys0, t, Keys),
    maplist(range(Series), Keys, Ranges),
    pairs_keys_values(Ranges, Mins, Maxs),
    min_list(Mins, Min),
    max_list(Maxs, Max),
    maplist(rescale(Min-Max), Ranges, Scales),
    maplist(serie(x-y, Series, JSMapping), Keys, Scales, Traces).

range(Series, Key, Min-Max) :-
    maplist(get_dict(Key), Series, Ys),
    min_list(Ys, Min),
    max_list(Ys, Max).

rescale(GMin-GMax, Min-Max, Scale) :-
    GDiff is GMax-GMin,
    scale(Scale),
    SMin is Min*Scale,
    SMax is Max*Scale,
    SDiff is SMax - SMin,
    fair(GDiff, SDiff),
    !.

fair(Candidate, Ref), Ref =\= 0 =>
    abs(Candidate/Ref) < 5,
    abs(Candidate/Ref) > 0.2.
fair(Candidate, Ref), Candidate =\= 0 =>
    abs(Ref/Candidate) < 5,
    abs(Ref/Candidate) > 0.2.
fair(_, _) =>
    true.


scale(X) :-                             % 1, 5, 10, 50, ...
    between(0, 20, S),
    (   X is 10^S
    ;   X is 5*10^S
    ).

serie(Axis, Series, JSMapping, Key, Scale, Trace) :-
    Trace0 = trace{x:Times, y:Values, mode:lines, name:Label},
    serie_label(JSMapping, Key, Scale, Label),
    convlist(tv(Key, Scale), Series, TVs),
    pairs_keys_values(TVs, Times, Values),
    set_axis(Axis, Trace0, Trace).

serie_label(JSMapping, Key, Scale, Label) :-
    Label0 = JSMapping.get(Key, Key),
    (   Scale =:= 1
    ->  Label = Label0
    ;   format(string(Label), '~w (*~w)', [Label0, Scale])
    ).

set_axis(x-y, Trace, Trace) :- !.
set_axis(X-Y, Trace0, Trace) :-
    Trace = Trace0.put(_{xaxis:X, yaxis:Y}).

tv(Key, Scale, State, T-V) :-
    get_dict(t, State, T),
    get_dict(Key, State, V0),
    number(V0),
    V is V0*Scale.

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

download_links(Source, Options) -->
    { variant_sha1(Source+Options, SHA1),
      (   saved(SHA1, Source, Options)
      ->  true
      ;   asserta(saved(SHA1, Source, Options))
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
    option(model(Model), Options, engine),
    findall(State-Values, qstate(Model, State, Values, [d(1)|Options]), Pairs),
    maplist(state_into_dict, Pairs, Data),
    option(id_mapping(IdMapping), Options, _{}),
    q_series_table(Data, Table, IdMapping),
    format('Content-type: text/csv~n~n'),
    csv_write_stream(current_output, Table,
                     [ separator(0',)]).

state_into_dict(State-Dict0, Dict) :-
    Dict = Dict0.put(state, State).

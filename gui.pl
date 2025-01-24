:- module(gui,
          []).
:- use_module(library(main)).
:- use_module(library(http/http_server)).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_server_health)).
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
:- use_module(library(exceptions)).
:- use_module(library(filesex)).
:- use_module(library(http/json)).

:- use_module(gsim).
:- use_module(map).
:- use_module(csv_util).
:- use_module(equations).
:- use_module(model).
:- if(current_prolog_flag(dynalearn, true)).
:- use_module(dynalearn).
:- use_module(library(error)).
:- use_module(library(listing)).
:- use_module(library(terms)).

:- endif.

http:location(garp, root(garp), []).
http:location(htmx, garp(htmx), []).

:- http_handler(root(.),    http_redirect(see_other, garp(home)), []).
:- http_handler(garp(.),    http_redirect(see_other, garp(home)), []).
:- http_handler(garp(home), home, []).
:- http_handler(garp(.),
                http_reply_from_files(web, [not_found(404)]),
                [prefix]).
:- http_handler(garp('node_modules/'),
                http_reply_from_files(node_modules, [not_found(404)]),
                [prefix]).
:- http_handler(garp(health), server_health,
                [id(server_health)]).

home(_Request) :-
    reply_html_page([ title('GarpN: the Garp numerical simulator'),
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
                              src('/garp/node_modules/htmx.org/dist/ext/json-enc.js')], []),
                      script([type('text/javascript'),
                              src('/garp/simulator.js')], []),
                      script([type('text/javascript'),
                              src('/garp/plotly-2.32.0.min.js')], []),
                      \refresh
                    ]).

home -->
    { Model = none
    },
    html([ h1("Garp numerical simulator"),
           div([ 'hx-ext'('response-targets'),
                 'hx-target-error'('#errors')
               ],
               [ div([class([narrow,content])
                     ],
                     [ \model_menus(Model),
                       div([class('model-separator'), clear(both)], []),
                       div([ form(['hx-post'('/garp/htmx/run'),
                                   'hx-vals'('js:{"ml_source": ml_value_string(), \c
                                                  "qspaces": get_jqspaces()}'),
                                   'hx-target'('#results'),
                                   'hx-on-htmx-before-request'('clear_output()')
                                  ],
                                  [ div([id('ml-model')],
                                        \mathlive_model(Model, Source, [])),
                                    section(class(columns),
                                            [ div([class(left)],
                                                  [ div([id(quantity_controls)],
                                                        \q_menu(Model, Source, []))
                                                  ]),
                                              div([class(right)],
                                                  \right_controls(Model))
                                            ])
                                  ])
                           ])
                     ]),
                 div(id(results), []),
                 div(id(script), [])
               ]),
           \js_script({|javascript||
                       let data;
                       let layout;
                       let plot;
                       let SHA1;
                      |})
         ]).

refresh -->
    js_script({|javascript||
               addEventListener("DOMContentLoaded", (ev) => {
                 document.getElementById("model-menu").dispatchEvent(new Event("change"));
               });
              |}).

%!  model_menus(+Default)//
%
%   Add the menus above the model. One for loading an existing model and
%   one to (re-)start the numerical model.

model_menus(Default) -->
    html(div(class('model-menus'),
             [ \model_menu(Default),
               \init_model_menu
             ])).

:- if(current_prolog_flag(dynalearn, true)).
% Fill menu from Dynalearn

model_menu(_Default) -->
    { dynalearn_models(Models)
    },
    html(select([ 'hx-get'('/garp/htmx/set-model'),
                  'hx-trigger'(change),
                  'hx-target'('#quantity_controls'),
                  id('model-menu'),
                  name(model)
                ],
                [ option(value(none), 'Please select a model from Dynalearn')
                | \sequence(dl_project, Models.result)
                ])).

dl_project(Project) -->
    sequence(dl_model(Project.project_code), Project.models).

dl_model(Project, Model) -->
    html(option([value(Model.id)],
                '[~w] ~w (~w)'-[Project, Model.title, Model.user])).

:- else.
% Fill menu from local files, using local Garp

model_menu(Default) -->
    { findall(File, directory_member(garp, File,
                                     [ extensions([db]) ]), Files0),
      sort(Files0, Files)
    },
    html(select([ 'hx-get'('/garp/htmx/set-model'),
                  'hx-trigger'(change),
                  'hx-target'('#quantity_controls'),
                  name(model)
                ],
                \model_options(Default, Files))).

model_options(none, Files) ==>
    html(option([value(none), selected, disabled(true)],
                'Please select a Model')),
    sequence(model_option(_), Files).
model_options(Default, Files) ==>
    sequence(model_option(Default), Files).

model_option(Default, File) -->
    { file_base_name(File, Base),
      file_name_extension(Model, _, Base),
      (   Model == Default
      ->  T = [selected]
      ;   T = []
      )
    },
    html(option([value(Model)|T], Model)).

:- endif.

%!  init_model_menu//
%
%   Provide options for populating the numerical model.

init_model_menu -->
    !,
    html(span(class('init-model-buttons'),
              [ \refresh_model_button,
                \model_button('\U0001F9F9', wipe_model,    "Clear model"),
                \save_model_button,
                \model_button('\U0001F4E5', load_model, "Load reference model"),
                \model_button('\u2728',     propose_model, "Propose model")
              ])).

model_button(Label, Target, Title) -->
    { http_link_to_id(Target, [], HREF) },
    html(button([ 'hx-get'(HREF),
                  'hx-vals'('js:{model: currentModel()}'),
                  'hx-target'('#quantity_controls'),
                  title(Title)
                ], Label)).

save_model_button -->
    { http_link_to_id(save_model, [], HREF) },
    html(button([ 'hx-get'(HREF),
                  'hx-vals'('js:{model: currentModel(), \c
                                 ml_source: ml_value_string(), \c
                                 qspaces: get_jqspaces()}'),
                  'hx-target'('#status'),
                  title('Save as reference model')
                ], '\U0001F4E4')).

%!  right_controls(+Model)//
%
%   Controls for the right div below the numerical model.

right_controls(Model) -->
    html([ div(id('qspace-controls'),
               \qspace_controls(Model, [])),
           \run_controls(Model),
           div([id(errors)], []),
           div([id(status)], [])
         ]).

%!  run_controls(++Model)// is det.
%
%   Emit the controls for running the simulation

run_controls(Model) -->
    html(div(class([controls]),
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
               input([ type(hidden), name(track), value(all)]),
               input([ type(hidden), name(model), id(model),
                       value(Model)
                     ]),
               ' ',
               input([ type(submit),
                       value("Run!")
                     ])
             ])).

%!  methods//
%
%   Create the menu for the numerical simulation technique.

methods -->
    html([ label(for(method), 'Method'),
           select(name(method),
                  [ option([value(euler), selected], 'Euler'),
                    option(value(rk4),  'RK4')
                  ])
         ]).

%!  mathlive_model(+Model, +Source, +Options)// is det.
%
%   Emit the model area as a set of mathlife equations.

mathlive_model(Model, Source, Options), var(Source) ==>
    { id_mapping(Model, IdMapping),
      default_nrels(Terms)
    },
     html(div(\equations(Terms, [id_mapping(IdMapping)|Options]))).
mathlive_model(Model, Source, Options) ==>
    { read_model_to_terms(Source, Terms),
      id_mapping(Model, IdMapping)
    },
    html(div(\equations(Terms, [id_mapping(IdMapping)|Options]))).

:- http_handler(htmx(analyze), analyze, []).
:- http_handler(htmx(run), run_model, []).
:- http_handler(htmx('mapping-table'), mapping_table, []).
:- http_handler(htmx('set-model'),     set_model_handler, []).
:- http_handler(htmx('save-model'),    save_model, []).
:- http_handler(htmx('load-model'),    load_model, []).
:- http_handler(htmx('wipe-model'),    wipe_model, []).
:- http_handler(htmx('propose-model'), propose_model, []).

%!  set_model_handler(+Request)
%
%   Change the model

set_model_handler(Request) :-
    http_parameters(Request, [ model(Model, []) ]),
    set_model(Model).

set_model(Model) :-
    (   Model == none
    ->  true
    ;   numeric_model_file(Model, File),
        exists_file(File)
    ->  read_model_to_terms(file(File), Terms0),
        partition(is_qspace, Terms0, QSpaces, Terms),
        Source = terms(Terms)
    ;   Source = _,
        QSpaces = []
    ),
    set_model(Model, Source, [saved_qspaces(QSpaces)]).

is_qspace(qspace(_Q,_Values)) => true.
is_qspace(_) => fail.

%!  set_model(+Model:atom, +Source, +Options) is det.
%
%   Set the model we work on.
%
%   @arg Source is the (Prolog) source for the numerical model.
%   This is either a string or a term terms(Terms).

set_model(Model, Source, Options) :-
    flush_dynalearn_model(Model),
    reply_htmx(
        [ \q_menu(Model, Source, Options),
          div([id('ml-model'), 'hx-swap-oob'(true)],
              \mathlive_model(Model, Source, Options)),
          div([id('qspace-controls'), 'hx-swap-oob'(true)],
              \qspace_controls(Model, Options)),
          \js_script({|javascript(Model)||setModel(Model)|})
        ]).

:- if(\+current_predicate(flush_dynalearn_model/1)).
flush_dynalearn_model(_).
:- endif.

numeric_model_file(Model, File) :-
    format(atom(File), 'numeric/~w.pl', [Model]).

%!  propose_model(+Request)
%
%   Create a new model based on the qualitative model

propose_model(Request) :-
     http_parameters(Request,
                     [ model(Model, [])
                     ]),
     init_model(Model, Terms),
     set_model(Model, terms(Terms), [grouped(true)]).

%!  load_model(Request)
%
%   Load the (reference) model

load_model(Request) :-
    http_parameters(Request,
                     [ model(Model, [])
                     ]),
    set_model(Model).

:- if(current_prolog_flag(dynalearn, true)).
:- http_handler(htmx('refresh-model'), refresh_model, []).

refresh_model_button -->
    { http_link_to_id(refresh_model, [], HREF) },
    html(button([ 'hx-get'(HREF),
                  'hx-vals'('js:{model: currentModel(), \c
                                 qspaces: get_jqspaces()}'),
                  'hx-target'('#qspace-controls'),
                  title('Reload qualitative model from Dynalearn')
                ], '\U0001F504')).


%!  refresh_model(Request)
%
%   Invalidate the model as we got it from Dynalearn.  Updates
%   the quantity spaces.

refresh_model(Request) :-
    http_parameters(Request,
                     [ model(Model, []),
                       qspaces(JQspaces, [default("[]")])
                     ]),
    jqspaces_to_prolog(Model, JQspaces, Qspaces),
    flush_dynalearn_model(Model),
    reply_htmx([ \qspace_controls(Model, [qspaces(Qspaces)]),
                 \htmx_oob(status, 'Reloaded model from Dynalearn')
               ]).
:- else.

refresh_model_button -->
    [].

:- endif.

%!  save_model(Request)
%
%   Save the current model as reference model

save_model(Request) :-
    http_parameters(Request,
                     [ model(Model, []),
                       ml_source(LaTeX, []),
                       qspaces(JQspaces, [default("[]")])
                     ]),
    numeric_model_file(Model, File),
    latex_to_prolog_source(LaTeX, Source),
    jqspaces_to_prolog(Model, JQspaces, Prolog),
    setup_call_cleanup(
        open(File, write, Out, [encoding(utf8)]),
        (   format(Out, '~s', [Source]),
            write_qspaces(Out, Prolog)
        ),
        close(Out)),
    reply_htmx('Saved model').

write_qspaces(Out, Prolog) :-
    format(Out, '~n% Quantity spaces~n~n', []),
    forall(member(QSpace, Prolog),
           portray_clause(Out, QSpace)).

%!  jqspaces_to_prolog(+Model, +JQspaces, -Prolog)
%
%   Turn the data from  the  quantity  space   control  into  a  list of
%   qspace(Quantity,  Values),  where  points  in    Values  are  either
%   point(Name=Value) or point(Nam).

jqspaces_to_prolog(Model, JQSpaces, Prolog) :-
    atom_json_dict(JQSpaces, List,
                   [ value_string_as(atom)
                   ]),
    maplist(jqspace_to_prolog(Model), List, Prolog).

jqspace_to_prolog(Model, Dict, qspace(Quantity, Values)) :-
    _{ qspace_id: QSpaceId,
       quantity: QDict,
       values:AValues
     } :< Dict,
     m_qspace(Model, QSpaceId, _QName, Values0),
     jq_to_prolog(QDict, Quantity),
     maplist(jqvalue_no_ex, AValues, NPoints),
     num_points_no_ex(Values0, NPoints, Values).

jq_to_prolog(Dict, Q), _{entity:E, attrib:A} :< Dict =>
    Q =.. [A,E].
jq_to_prolog(Name, Q), atom(Name) =>
    Q = Name.

jqvalue_no_ex(AValue, NValue) :-
    atom_number(AValue, NValue),
    !.
jqvalue_no_ex(_, _).

num_points_no_ex([], [], []).
num_points_no_ex([point(Name)|T0], [Num|T1], [point(P)|T]) :-
    !,
    (   number(Num)
    ->  P = (Name=Num)
    ;   P = Name
    ),
    num_points_no_ex(T0, T1, T).
num_points_no_ex([H|T0], T1, [H|T]) :-
    num_points_no_ex(T0, T1, T).


%!  wipe_model(+Request)
%
%   Clear the model, adding the default equations for time.

wipe_model(Request) :-
    http_parameters(Request,
                     [ model(Model, [])
                     ]),
    set_model(Model, _, []).

%!  analyze(+Request)
%
%   Analyze the model and fill the quantity controls.

analyze(Request) :-
    http_read_json_dict(Request, Data, []),
    _{model:ModelS, ml_data:MlData} :< Data,
    atom_string(Model, ModelS),
    (   JQSpaces = Data.get(qspaces)
    ->  qspaces(Model, JQSpaces, QSpaces),
        Options = [qspaces(QSpaces)]
    ;   Options = []
    ),
    latex_to_prolog_ex(MlData, Prolog),
    reply_htmx([ \q_menu(Model, terms(Prolog), Options),
                 \js_script({|javascript(Model,Source)||
                             setModel(Model,Source)|})
               ]).

%!  q_menu(+Model, +Source, +Options)// is det.
%
%   Emit the found quantities with a menu that asks whether or not to
%   consider mapping this quantity to Garp.

q_menu(Model, Source, Options) -->
    { nonvar(Source),                            % there is a model
      id_mapping(Model, IdMapping),
      catch(read_model(Source, Formulas, _Constants, _State0,
                       [ id_mapping(IdMapping) ]),
            model:model_error, Ball, true)
    },
    (   {var(Ball)}
    ->  { dict_keys(Formulas, Keys),
          maplist(key_quantity(IdMapping), Keys, Quantities0),
          sort(Quantities0, Quantities1),
          delete(Quantities1, t, Quantities2),
          order_keys(IdMapping, Quantities2, Quantities)
        },
        html(table(class(quantities),
                   [ tr([ th([class(quantity), rowspan(2)], 'Quantity'),
                          th([colspan(3)], 'Link to Qualitative states')
                        ]),
                     tr([\sequence(derivative_header, [0,1,2])]),
                     \sequence(q_control(Model, IdMapping, Options), Quantities)
                   ]))
    ;   model_issues(Ball)
    ).
q_menu(_, _, _) -->
    [].

key_quantity(IdMapping, Key, Quantity) :-
    derivative_key(Key, Quantity, IdMapping),
    !.
key_quantity(_, Key, Key).


q_control(Model, IdMapping, Options, Key) -->
    { (   option(qspaces(QSpaces), Options),
          QSpace = QSpaces.get(Key)
      ->  true
      ;   m_qspace(Model, Key, _QName, QSpace)
      ->  true
      ;   QSpace = []
      )
    },
    html(tr(class('quantity-link'),
            [ th(class([quantity,name]), \key_label(IdMapping,Key))
            | \derivatives_select(Key, QSpace)
            ])).

key_label(IdMapping, Key) -->
    { Term = IdMapping.get(Key),
      compound(Term),
      compound_name_arguments(Term, Attr, [Ent]),
      !
    },
    html(span(class('q-attrib'),
              [ span(class('entity-attr'), Attr),
                span(class('entity'), Ent)
              ])).
key_label(IdMapping, Key) -->
    { key_label(IdMapping, Key, Label) },
    html(span(class('q-plain'), Label)).

derivatives_select(Name, QSpace) -->
    { (   QSpace = [Interval],
          atom(Interval)
      ->  VOpts = []
      ;   VOpts = [checked]
      ),
      atom_concat('__v_', Name,  NValue),
      atom_concat('__d1_', Name, ND1),
      atom_concat('__d2_', Name, ND2)
    },
    html([ td(class('link'), input([type(checkbox), name(NValue) | VOpts])),
           td(class('link'), input([type(checkbox), name(ND1), checked])),
           td(class('link'), input([type(checkbox), name(ND2)]))
         ]).

%!  qspace_controls(+Model, +Options)// is det.
%
%   Provides controls to define quantity space  points for which we need
%   a value.

qspace_controls(none, _) -->
    !.
qspace_controls(Model, Options) -->
    { id_mapping(Model, IdMapping),
      findall(QspaceId-Values,
              incomplete_qspace(Model, QspaceId, Values),
              Pairs),
      Pairs \== []
    },
    !,
    html(div(class('qspace-header'), 'Quantity spaces')),
    sequence(qspace_control(IdMapping, Options), Pairs).
qspace_controls(_, _) -->
    [].

incomplete_qspace(Model, QspaceId, Values) :-
    m_qspace(Model, QspaceId, _QSpaceName, Values),
    (   member(point(P), Values),
        \+ qspace_point_value(P, _)
    ->  true
    ).

qspace_control(IdMapping, Options, QspaceId-Values) -->
    { (   option(saved_qspaces(QSpaces), Options),
          Q = IdMapping.get(QspaceId),
          memberchk(qspace(Q, Savedvalues), QSpaces)
      ->  true
      ;   Savedvalues = []
      )
    },
    html(div([ class('qspace-control'),
               id(QspaceId)
             ],
             [ span(class('qspace-quantity'),
                    \key_label(IdMapping,QspaceId))
             | \sequence(qspace_element(Savedvalues),
                         html(span(class('qspace-sep'), '<')),
                         Values)
             ])).

qspace_element(Savedvalues, point(Name)) ==>
    { (   qspace_point_value(Name, PreDef)
      ->  Extra = [value(PreDef), disabled]
      ;   memberchk(point(Name=SavedVal), Savedvalues)
      ->  Extra = [value(SavedVal)]
      ;   Extra = []
      )
    },
    html(div(class('qspace-point'),
             [ input([ class('qspace-point'),
                       type(number),
                       name(Name),
                       size(3)
                     | Extra
                     ]),
               br([]), span(Name)
             ])).
qspace_element(_, Name), atom(Name) ==>
    html(span(class('qspace-interval'), Name)).

%!  model_issues(+Error)//

model_issues(model_error(no_time_formulas)) ==>
    html(div(class(warning),
             'No time formulas')).
model_issues(model_error(invalid(Invalid))),
    partition(is_placeholder, Invalid, PlaceHolders, []) ==>
    { length(PlaceHolders, Count) },
    html(div(class(warning),
             'Warning: ~D constants or initial values are not specified'-[Count])).

model_issues(Error) ==>
    { message_to_string(Error, Msg)
    },
    html(pre(class(error), Msg)).


%!  mapping_table(+Request)
%
%   Print a table with the mapping to Garp  after the user clicks in the
%   chart at a certain time point.

mapping_table(Request) :-
    http_read_data(Request, Data, []),
    _{time:TimeAtom, sha1:SHA1} :< Data,
    atom_number(TimeAtom, Time),
    mapping_table(SHA1, Time).

mapping_table(SHA1, Time) :-
    saved(SHA1, Model, Options),
    q_series(Model, QSeries,
             [ link_garp_states(true)
             | Options
             ]),
    full_garp_states(GarpStates, Options),
    (   phrase(info_seq(Time, States0), QSeries, _)
    ->  add_garp_states(States0, GarpStates, States, Cmp),
        reply_htmx(\state_table(States, [Cmp|Options]))
    ;   phrase((...,timed(Time,State),...), QSeries, _)
    ->  add_garp_states(State, GarpStates, States, Cmp),
        reply_htmx(\state_table(States, [Cmp|Options]))
    ;   reply_htmx('Could not find matching states at T=~3f'-[Time])
    ).

%!  full_garp_states(-GarpStates, +Options) is det.
%
%   GarpStates is a list of Garp states up to the 2nd derivative.

full_garp_states(GarpStates, Options) :-
    option(model(ModelName), Options, engine),
    select_option(match(Match), Options, Options1),
    mapdict(d2, Match, Match1),
    findall(Id-QState, qstate(ModelName, Id, QState,
                              [match(Match1)|Options1]),
            GarpStates).

d2(_Key, _V, [0,1]).

%!  info_seq(+Time, -States)// is semidet.
%
%   True when States is a sequence   of  qualitative states derived from
%   the model, where the first and last   states  are linked to Garp and
%   the sequence contains Time.

info_seq(Time, States) -->
    ...,
    linked(Before), not_linked_list(BL), timed(Time,State),
    { \+ State.get(garp_states) = [_|_] },
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
    { get_dict(t, State, d(TState,_,_,_)),
      TState =< Time,
      get_dict(t, Next, d(TNext,_,_,_)),
      TNext >= Time
    }.

... --> [] ; [_], ... .
peek(X), [X] --> [X].

%!  add_garp_states(+QStates, +GarpStates:list(pair(integer,dict)),
%!                  -States, -Compare) is det.

add_garp_states(QStates, GStates, States, Cmp), is_list(QStates) =>
    Cmp = cmp(true),
    once(append([First|Skipped], [Last], QStates)),
    min_list(First.garp_states, Min),
    max_list(Last.garp_states, Max),
    include(in_state_range(Min,Max), GStates, GStates1),
    maplist(add_state_no, GStates1, GStates2),
    interpolate_nq(Skipped, GStates2, Compare),
    append([[First],Compare,[Last]], States).
add_garp_states(QState, GStates, States, Cmp),
    is_dict(QState), QState.garp_states = [_|_] =>
    Cmp = cmp_to(QState),
    include(in_state_set(QState.garp_states), GStates, GStates1),
    maplist(add_state_no, GStates1, GStates2),
    States = [QState|GStates2].
add_garp_states(QState, GStates, States, Cmp),
    is_dict(QState), QState.garp_states == [] =>
    Cmp = cmp_to(QState),
    maplist(add_state_no, GStates, GStates1),
    States = [QState|GStates1].                  % todo: find candidates.

in_state_range(Min, Max, Id-_) :-
    Id > Min,
    Id < Max.

in_state_set(Set, Id-_) :-
    memberchk(Id, Set).

add_state_no(Id-State0, State) :-
    State = State0.put(garp_states, Id).

%!  interpolate_nq(+Numeric, +Qualitative, -Rows) is det.

interpolate_nq([], [], R) =>
    R = [].
interpolate_nq([Last], L, R) =>
    R = [cmp_to(Last, [Last|L])].
interpolate_nq([], L, R) =>
    R = L.
interpolate_nq(L, [], R) =>
    R = L.
interpolate_nq([H0|T0], [H1|T1], R) =>
    R = [cmp_to(H0, [H0,H1])|T],
    interpolate_nq(T0, T1, T).


%!  state_table(+States, +Options)// is det.
%
%   Print an HTML table of states.

state_table(States, Options) -->
    { plain_rows(States, StatesPlain),
      option(id_mapping(IdMapping), Options, _{}),
      (   option(keys(Keys), Options)
      ->  true
      ;   maplist(dict_keys, StatesPlain, KeysL),
          append(KeysL, AllKeys),
          sort(AllKeys, Keys0),
          order_keys(IdMapping, Keys0, Keys)
      ),
      (   option(match(DerDict), Options)
      ->  true
      ;   key_derivatives(StatesPlain, Keys, DerDict)
      ),
      maplist(key_ders(DerDict), Keys, KeyDers),
      maplist(key_obj_attr(IdMapping), Keys, Objs, _Attrs),
      obj_colspans(Objs, KeyDers, ObjSpans)
    },
    html(table(class(states),
               [ tr(\sequence(obj_header, ObjSpans)),
                 tr(\sequence(state_header(1, DerDict, IdMapping), Keys)),
                 tr(\sequence(state_header(2, DerDict, IdMapping), Keys))
               | \state_rows(States, KeyDers, Options)
               ])).

key_ders(DerDict, Key, Key-Ders) :-
    Ders = DerDict.get(Key),
    !.
key_ders(_, Key, Key-[0]).

key_derivatives(StatesPlain, Keys, DerDict) :-
     maplist(series_key_derivative(StatesPlain), Keys, KeyMaxDers),
     maplist(numlist(0), KeyMaxDers, KeyDers),
     pairs_keys_values(Pairs, Keys, KeyDers),
     dict_pairs(DerDict, #, Pairs).

%!  plain_rows(+Rows, -Plain) is det.
%
%   Expand cmp_to(Ref, Rows) terms in Rows.

:- det(plain_rows/2).
plain_rows(Rows, Plain) :-
    phrase(plain_rows(Rows), Plain).

plain_rows([]) -->
    !.
plain_rows([H|T]) -->
    !,
    plain_rows(H),
    plain_rows(T).
plain_rows(cmp_to(_,Rows)) -->
    !,
    plain_rows(Rows).
plain_rows(Row) -->
    [Row].

obj_colspans([], _, []).
obj_colspans([O|OT], [_Key-Ders|KeyDers], [O-Colspan|CST]) :-
    length(Ders, Colspan0),
    obj_colspan(O, OT, OT1, KeyDers, KeyDers1, Colspan0, Colspan),
    obj_colspans(OT1, KeyDers1, CST).

obj_colspan(O, [O|OT], OT1, [_Key-Ders|KeyDers], KeyDers1, Colspan0, Colspan) =>
    length(Ders, New),
    Colspan1 is Colspan0 + New,
    obj_colspan(O, OT, OT1, KeyDers, KeyDers1, Colspan1, Colspan).
obj_colspan(_, OT, OT1, KeyDers, KeyDers1, Colspan0, Colspan) =>
    OT1 = OT,
    KeyDers1 = KeyDers,
    Colspan = Colspan0.

obj_header(Obj-1) -->
    { var(Obj) },
    !,
    html(th([])).
obj_header(Obj-1) -->
    html(th(Obj)).
obj_header(Obj-AttrCount) -->
    html(th([class(entity), colspan(AttrCount)], Obj)).

%!  state_header(+Nth, +DerDict, +IdMapping, +Key)//
%
%   Emit the Nth header row for Key

state_header(Nth, DerDict, IdMapping, Key) -->
    { Ders = DerDict.get(Key,[]),
      Ders \== []
    },
    !,
    (   {Nth == 1}                         % 1st or 2nd header line
    ->  { length(Ders, Cols),
          key_obj_attr(IdMapping, Key, Obj, Attr),
          (   var(Obj)
          ->  key_label(IdMapping, Key, Label),
              Extra = []
          ;   Label = Attr,
              Extra = [class(property)]
          )
        },
        html(th([colspan(Cols)|Extra], Label))
    ;   sequence(derivative_header, Ders)
    ).
state_header(2, _Rows, IdMapping, Key) -->
    !,
    { key_label(IdMapping, Key, Label) },
    html(th(Label)).
state_header(1, _Rows, _IdMapping, _Key) -->
    html(th([])).

derivative_header(0) ==>
    th_label(value).
derivative_header(Der) ==>
    th_label(d(Der)).

%!  state_rows(+Rows, +KeysDers:list(pair(Key-Ders)), +Options)// is det.

state_rows([], _, _) -->
    [].
state_rows([cmp_to(Ref, Rows)|T], KeysDers, Options) -->
    { merge_options([cmp_to(Ref)], Options, Options1)
    },
    state_rows(Rows, KeysDers, Options1),
    state_rows(T, KeysDers, Options).
state_rows([H|T], KeysDers, Options) -->
    { option(cmp_to(State), Options),
      State \== H,
      !,
      state_row(KeysDers, State, _, RefCells),
      state_row(KeysDers, H,     _, Cells),
      pairs_keys_values(Pairs, RefCells, Cells)
    },
    html(tr(class(garp), \sequence(cmp_value(2, 1), Pairs))),
    state_rows(T, KeysDers, Options).
state_rows([H|T], KeysDers, Options) -->
    { state_row(KeysDers, H, _, Cells) },
    html(tr(\sequence(cell_value([]), Cells))),
    state_rows(T, KeysDers, Options).

cmp_value(1, RS, S-G) -->
    { S == G },
    !,
    cell_value([class(match),rowspan(RS)], S).
cmp_value(2, 1, S-G) -->
    { S == G },
    !,
    cell_value([class([match,garp])], S).
cmp_value(2, _, S-G) -->
    { S == G },
    !.
cmp_value(1, _, (C-T)-_) -->
    { no_cmp_column(C) },
    !,
    cell_value([class([simulation])], C-T).
cmp_value(2, _, _-(C-T)) -->
    { no_cmp_column(C) },
    !,
    cell_value([class([garp])], C-T).
cmp_value(1, _, S-G) -->                   % for ambiguous and matched states
    { empty(S) ; empty(G) },
    !,
    cell_value([class([simulation])], S).
cmp_value(2, _, S-G) -->
    { empty(S) ; empty(G) },
    !,
    cell_value([class([garp])], G).
cmp_value(1, _, S-_) -->
    !,
    cell_value([class([nomatch,simulation])], S).
cmp_value(2, _, _-G) -->
    cell_value([class([nomatch,garp])], G).

empty(_K-V), var(V) => true.
empty(_) => fail.

no_cmp_column(t).
no_cmp_column(garp_states).


cell_value(Attrs, Pair) -->
    { empty(Pair) },
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
cell_value(Attrs, _K-point(Name)) -->
    !,
    { join_attrs([class(qualitative)], Attrs, Attrs1)
    },
    html(td(Attrs1, span(class('qspace-point'), Name))).
cell_value(Attrs, _K-error(Error)) -->
    !,
    { join_attrs([class(qualitative)], Attrs, Attrs1)
    },
    html(td(Attrs1, span(class('qspace-error'), Error))).
cell_value(Attrs, garp_states-[]) -->
    { join_attrs([class(['garp-link', nomatch])], Attrs, Attrs1)
    },
    html(td(Attrs1, '?')).
cell_value(Attrs, garp_states-[Match]) -->
    { join_attrs([class(['garp-link', match])], Attrs, Attrs1)
    },
    html(td(Attrs1, '<~p>'-[Match])).
cell_value(Attrs, garp_states-Value) -->
    { Value = [_,_|_],
      !,
      atomics_to_string(Value, ",", Label),
      join_attrs([class(['garp-link', ambiguous])], Attrs, Attrs1)
    },
    html(td(Attrs1, ['<',Label,'>'])).
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

th_label(Key) -->
    { h_label(Key, Label) },
    html(th(Label)).

h_label(d(1), '\U0001D4ED¹').
h_label(d(2), '\U0001D4ED²').
h_label(d(3), '\U0001D4ED³').
h_label(value, '\U0001D4E5').

v_label(plus) --> html(span(class(plus), '\u25B2')).
v_label(min)  --> html(span(class(min),  '\u25BC')).
v_label(zero) --> html(span(class(zero), '0')).

%!  run_model(+Request)
%
%   Run the simulation

run_model(Request) :-
    http_parameters(Request,
                    [ iterations(Iterations, [integer]),
                      track(Track, [oneof([all,initialized]),
                                    default(initialized)]),
                      method(Method, [oneof([euler,rk4]), default(euler)]),
                      sample(Sample, [integer, optional(true)]),
                      rulers(ShowRulers, [boolean, default(false)]),
                      ml_source(MlSource, []),
                      qspaces(JQspaces, []),
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
    qspaces(Model, JQspaces, QSpaces),
    Options = [ model(Model),
                match(Derivatives),
                iterations(Iterations),
                method(Method),
                track(Track),
                sample(Sample),
                id_mapping(IdMapping),
                qspaces(QSpaces)
              ],
    latex_to_prolog_ex(MlSource, Equations),
    call_time(simulate(terms(Equations), Series0, Options), Time),
    init_derivatives(Series0, Series, IdMapping),
    annotate_garp_states(Series, Shapes, Options),
    plotly_traces(Series, Traces, IdMapping),
    reply_htmx([ hr([]),
                 \stats(Series, Time),
                 div([ id(plot),
                       'hx-vals'('js:{time: plotly_clicked_at.x, sha1:SHA1}'),
                       'hx-post'('/garp/htmx/mapping-table'),
                       'hx-trigger'('clicked-x'),
                       'hx-target'('#mapping-table')
                     ],
                     [ \rulers(ShowRulers),
                       div([id(plotly),class(plotly)], []),
                       \plot(plotly, @(null), Traces, Shapes),
                       \js_script({|javascript||initShapes("plotly")|})
                     ]),
                 div([id('mapping-table'),class(narrow)], [&(nbsp)]),
                 \download_links(terms(Equations), Options)
               ]).

%!  qspaces(+Model, +JQSpaces, -QSpaces) is det.
%
%   @arg QSpaces is a dict Quantity -> QualitativeValues.

qspaces(Model, JQSpaces, QSpaces) :-
    jqspaces(JQSpaces, QPoints),
    findall(Q-Values, qspace(Model, QPoints, Q, Values), Pairs),
    dict_pairs(QSpaces, #, Pairs).

qspace(Model, QPoints, Q, Values) :-
    m_qspace(Model, Q, _QName, Values0),
    (   Points = QPoints.get(Q)
    ->  num_points(Values0, Points, Values)
    ;   Values = Values0
    ).

num_points([], [], []).
num_points([point(Name)|T0], [Num|T1], [point(Name=Num)|T]) :-
    !,
    num_points(T0, T1, T).
num_points([H|T0], T1, [H|T]) :-
    num_points(T0, T1, T).

jqspaces(JQSpaces, QPoints) :-
    atom_json_dict(JQSpaces, List,
                   [ value_string_as(atom)
                   ]),
    maplist(jqspace, List, QList),
    dict_pairs(QPoints, #, QList).

jqspace(Dict, QSpace-Points) :-
    _{ qspace_id:QSpace,
       values:AValues
     } :< Dict,
     (   maplist(atom_number, AValues, Points)
     ->  true
     ;   type_error(numbers, AValues)
     ).


%!  plot(+Target, +Title, +Traces, +Shapes)//
%
%   Emit the JavaScript that  realizes  the   Plotly  plot  holding  all
%   quantities  and  derivatives  and  the    mapping   shapes  to  Garp
%   qualitative states.
%
%   @arg Target is the id of the target `div` element
%   @arg Title is the title for the entire plot or `null`
%   @arg Traces is a list of `trace` dicts, each representing a single
%   quantity or derivative.

plot(Target, Title, Traces, Shapes) -->
    js_script({|javascript(Target,Title,Traces,Shapes)||
               data = Traces;
               layout = { title: Title,
                          shapes: Shapes,
                          margin: { t: 30, b: 25 },
                          hovermode: "x"
                        };
               plot = Plotly.newPlot(Target, data, layout);
              |}).

%!  select_traces(+Series, -Selected, +IdMapping) is det.
%
%   Find the traces we want to display.  Selected is a list of one of
%   `Key`, d1(Key), d2(Key) or d3(Key).

select_traces(Series, Traces, IdMapping) :-
    Series = [First|_],
    dict_keys(First, Keys0),
    delete(Keys0, t, Keys1),
    order_keys(IdMapping, Keys1, Keys),
    key_traces(Keys, Series, Traces).

key_traces([], _, []).
key_traces([H|T], Series, Traces) :-
    key1_traces(H, 0, Series, Traces, TTail),
    key_traces(T, Series, TTail).

key1_traces(Key, D, Series, Traces, Tail) :-
    (   series_has_values(Key, D, Series)
    ->  mkd(Key, D, H),
        Traces = [H|T],
        Found = true
    ;   Traces = T,
        Found = false
    ),
    (   d_next(D, D1, Found)
    ->  key1_traces(Key, D1, Series, T, Tail)
    ;   T = Tail
    ).

series_has_values(Key, D, Series) :-
    Arg is D+1,
    member(Entry, Series),
    get_dict(Key, Entry, DA),
    arg(Arg, DA, Value),
    nonvar(Value),                      % Speedup
    normal_number(Value),
    !.

mkd(Key, 0, Key).
mkd(Key, 1, d1(Key)).
mkd(Key, 2, d2(Key)).
mkd(Key, 3, d3(Key)).

d_next(0, 1, _).
d_next(1, 2, true).
d_next(2, 3, true).

%!  plotly_traces(+Series, -Traces, +IdMapping) is det.
%
%   Generate the Plotly traces from Series.
%
%   @arg Series is a list of dicts.  Each   dict  maps keys on to a term
%   d(V,D1,D2,D3),  where  any  of  these  may  be  unbound  or  contain
%   non-normal floats.

plotly_traces(Series, Traces, IdMapping) :-
    select_traces(Series, Selected, IdMapping),
    plotly_traces(Series, Selected, Traces, IdMapping).

plotly_traces(Series, Selected, Traces, IdMapping) :-
    maplist(range(Series), Selected, Ranges),
    pairs_keys_values(Ranges, Mins, Maxs),
    min_list_normal(Mins, Min),
    max_list_normal(Maxs, Max),
    maplist(rescale(Min-Max), Ranges, Scales),
    maplist(serie(x-y, Series, IdMapping), Selected, Scales, Traces).

range(Series, Key, Min-Max) :-
    maplist(get_value(Key), Series, Ys),
    min_list_normal(Ys, Min),
    max_list_normal(Ys, Max).

get_value(Key, Dict, Value), atom(Key) =>
    get_dict(Key, Dict, d(Value,_,_,_)).
get_value(d1(Key), Dict, Value), atom(Key) =>
    get_dict(Key, Dict, d(_,Value,_,_)).
get_value(d2(Key), Dict, Value), atom(Key) =>
    get_dict(Key, Dict, d(_,_,Value,_)).
get_value(d3(Key), Dict, Value), atom(Key) =>
    get_dict(Key, Dict, d(_,_,_,Value)).

rescale(_, Min-Max, Scale), Min =:= 0, Max =:= 0 =>
    Scale = 1.
rescale(GMin-GMax, Min-Max, Scale), Min =:= Max =>
    Mid is (GMax-GMin)/2,
    scale(Scale),
    Val is Min*Scale,
    fair(Mid, Val).
rescale(GMin-GMax, Min-Max, Scale) =>
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
    between(0, 200, S),
    (   X is 10^S
    ;   X is 5*10^S
    ).

%!  serie(+Axis, +Series, +IdMapping, +DKey, +Scale, -Trace) is det.
%
%   @arg DKey is a plain Key, d1(Key), d2(Key) or d3(Key)

serie(Axis, Series, IdMapping, DKey, Scale, Trace) :-
    Trace0 = trace{x:Times, y:Values, mode:lines, name:Label},
    serie_label(IdMapping, DKey, Scale, Label),
    convlist(tv(DKey, Scale), Series, TVs),
    pairs_keys_values(TVs, Times, Values),
    set_axis(Axis, Trace0, Trace).

serie_label(IdMapping, DKey, Scale, Label) :-
    dkey_label(IdMapping, DKey, Label0),
    (   Scale =:= 1
    ->  Label = Label0
    ;   format(string(Label), '~w (*~w)', [Label0, Scale])
    ).

dkey_label(IdMapping, DKey, Label), atom(DKey) =>
    key_label(IdMapping, DKey, Label).
dkey_label(IdMapping, d1(Key), Label) =>
    key_label(IdMapping, Key, Label0),
    atom_concat('Δ', Label0, Label).
dkey_label(IdMapping, d2(Key), Label) =>
    key_label(IdMapping, Key, Label0),
    atom_concat('Δ²', Label0, Label).
dkey_label(IdMapping, d3(Key), Label) =>
    key_label(IdMapping, Key, Label0),
    atom_concat('Δ³', Label0, Label).

set_axis(x-y, Trace, Trace) :- !.
set_axis(X-Y, Trace0, Trace) :-
    Trace = Trace0.put(_{xaxis:X, yaxis:Y}).

tv(DKey, Scale, State, T-V) :-
    get_dict(t, State, d(T,_,_,_)),
    get_value(DKey, State, V0),
    normal_number(V0),
    V is V0*Scale.

		 /*******************************
		 *        MAPPING TO GARP	*
		 *******************************/

%!  form_derivatives(+FormData, -Derivatives:dict) is det.
%
%   Get the derivatives want to match for a specific quantity.
%
%   @arg Derivatives is a dict `Quantity -> List`, where `List`
%   contains 0 (value), 1 (1st derivative), ....

form_derivatives(Form, Derivatives) :-
    convlist(form_attr_derivative, Form, Pairs),
    keysort(Pairs, Sorted),
    group_pairs_by_key(Sorted, ByKey),
    maplist(order_derivatives, ByKey, ByKeySets),
    dict_pairs(Derivatives, #, ByKeySets).

form_attr_derivative(Name=on, Key-0) :-
    atom_concat('__v_', Key, Name).
form_attr_derivative(Name=on, Key-1) :-
    atom_concat('__d1_', Key, Name).
form_attr_derivative(Name=on, Key-2) :-
    atom_concat('__d2_', Key, Name).

order_derivatives(Key-List, Key-Set) :-
    sort(List, Set).

%!  annotate_garp_states(+Series, -Shapes, +Options) is det.

annotate_garp_states(Series, Shapes, Options) :-
    option(match(Derivatives), Options),
    List = Derivatives._,
    List \== [],
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
    get_dict(t, Prev,   d(T0,_,_,_)),
    get_dict(t, QState, d(T1,_,_,_)),
    Shape = #{ type: rect,
               xref: x,
               yref: paper,
               x0: T0,
               y0: 0,
               x1: T1,
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
    simulate(Model, Series, Options),
    option(id_mapping(IdMapping), Options, _{}),
    Series = [First|_],
    dict_keys(First, Keys0),
    order_keys(IdMapping, Keys0, Keys),
    dicts_to_compounds(Series, Keys, dict_fill(-), Compounds0),
    maplist(round_float_row(4), Compounds0, Compounds),
    format('Content-type: text/csv~n~n'),
    maplist(key_label(IdMapping), Keys, Labels),
    Title =.. [row|Labels],
    csv_write_stream(current_output, [Title|Compounds],
                     [ separator(0',)]).

%!  download_map(+SHA1, +Request) is det.
%
%   Download the mapping to Garp as CSV.

download_map(SHA1, _Request) :-
    saved(SHA1, Model, Options),
    q_series(Model, QSeries, [link_garp_states(true)|Options]),
    option(id_mapping(IdMapping), Options, _{}),
    q_series_table(QSeries, Table, IdMapping),
    mapsubterms(csv_map, Table, CSVTable),
    format('Content-type: text/csv~n~n'),
    csv_write_stream(current_output, CSVTable,
                     [ separator(0',)]).

csv_map(point(zero), 0) :-
    !.
csv_map(point(Name), String) :-
    format(string(String), '\u25CF~w', [Name]).

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
    mapsubterms(csv_map, Table, CSVTable),
    format('Content-type: text/csv~n~n'),
    csv_write_stream(current_output, CSVTable,
                     [ separator(0',)]).

state_into_dict(State-Dict0, Dict) :-
    Dict = Dict0.put(state, State).

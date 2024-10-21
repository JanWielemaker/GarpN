:- module(map,
          [ id_mapping/1,               % -Mapping:dict
            q_series/2,                 % -QSeries, +Options
            q_series/3,                 % +Model, -QSeries, +Options
            qstate/3,                   % +State, -Values, +Options
            link_garp_states/3,         % +QSeries0, -QSeries, +Options
            q_series_table/3,           % +QSeries, -Table, +IdMapping
            nq_series/3,                % +Series, -QSeries, +Options
            save_garp_results/1,        % +File
            id_mapping/2,               % +Model, -Mapping
            qstate/4                    % +Model, ?Id, -Values, +Options
          ]).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(pairs)).

:- use_module(gsim).
:- use_module(csv_util).
:- use_module(library(aggregate)).
:- use_module(library(dicts)).
:- use_module(library(dcg/high_order)).

/** <module> Map qualitative and quantitative (simulation) model
*/

%!  id_mapping(-Mapping:dict) is det.
%
%   Compute the mapping of  Garp  quantity   names  to  number_of(Q)  or
%   growth(Q) terms.

id_mapping(Mapping) :-
    id_mapping_(engine, Mapping).

id_mapping_(Module, Mapping) :-
    findall(Id-Term, id_map(Module, Id, Term), Pairs),
    dict_pairs(Mapping, _, Pairs).

id_map(Module, Id, Term) :-
    Module:qspace(Id, Term4, _Values, fail),
    Term4 =.. [DV,Q|_],
    Term =.. [DV,Q].

%!  qstate(?Id, -Values, +Options)
%
%   Get the qualitative state from Garp  in   the  same  notation as our
%   simulator.

qstate(State, Values, Options) :-
    engine:state(State, _),
    option(d(N), Options, 3),
    option(match(Match), Options, #{}),
    findall(Qid-Value, qstate_value(Match, N, State, Qid, Value), Pairs),
    dict_pairs(Values, _, Pairs).

qstate_value(Match, N, State, Qid, Value) :-
    state_quantity_value(State, Dict),
    Qid = Dict.get(name),
    (   N2 = Match.get(Qid)
    ->  N2 > 0,
        qstate_value(N2, Dict, Value)
    ;   qstate_value(N, Dict, Value)
    ).

qstate_value(0, Dict, V) =>
    _{value:V0, derivative: _{1:_,2:_,3:_}} :< Dict,
    unknown_var(V0, V).
qstate_value(1, Dict, R) =>
    R = d(V,D1),
    _{value:V0, derivative: _{1:D10,2:_,3:_}} :< Dict,
    unknown_var(V0, V),
    unknown_var(D10, D1).
qstate_value(2, Dict, R) =>
    R = d(V,D1,D2),
    _{value:V0, derivative: _{1:D10,2:D20,3:_}} :< Dict,
    unknown_var(V0, V),
    unknown_var(D10, D1),
    unknown_var(D20, D2).
qstate_value(3, Dict, R) =>
    R = d(V,D1,D2,D3),
    _{value:V0, derivative: _{1:D10,2:D20,3:D30}} :< Dict,
    unknown_var(V0, V),
    unknown_var(D10, D1),
    unknown_var(D20, D2),
    unknown_var(D30, D3).

state_quantity_value(State, Dict) :-
    visualize:state_quantity_value(State, Dict).


unknown_var(unk, _) => true.
unknown_var(unknown, _) => true.
unknown_var(nil, _) => true.
unknown_var(?, _) => true.
unknown_var(pos, Val) => Val = plus.    % used for derivatives
unknown_var(neg, Val) => Val = min.
unknown_var(plus, Val) => Val = plus.   % quantity space values
unknown_var(zero, Val) => Val = zero.
unknown_var(min,  Val) => Val = min.

smd_data(params, smd(_Name, _SE, parameters(P), _V, _R, _SS, _IS), Result) => Result = P.
smd_data(values, smd(_Name, _SE, _P, par_values(V), _R, _SS, _IS), Result) => Result = V.
smd_data(store,  smd(_Name, _SE, _P, _V, _R, _SS, store(IS)),      Result) => Result = store(IS).

%!  save_garp_results(+Model)
%
%   Save the results of the simulation to File, so we can compare
%   without running

save_garp_results(Model) :-
    saved_model_file(Model, File),
    setup_call_cleanup(
        open(File, write, Out),
        save_garp_to_stream(Out, Model),
        close(Out)).

save_garp_to_stream(Out, Module) :-
    format(Out, ':- module(~q, []).~n~n', [Module]),
    forall(engine:qspace(Id, Term4, Values, Fail),
           format(Out, '~q.~n',  [qspace(Id, Term4, Values, Fail)])),
    nl(Out),
    forall(qstate(State, Values, []),
           format(Out, '~q.~n',  [qstate(State, Values)])).

saved_model_file(Model, File) :-
    format(atom(File), 'garp/~w.db', [Model]).

%!  id_mapping(+Model, -Mapping) is det.
%
%   Extract the identifier mapping from a   saved Garp simulation. If no
%   model is saved, we use the current model.

id_mapping(Model, Mapping) :-
    Model \== engine,
    current_predicate(Model:qspace/4),
    !,
    id_mapping_(Model, Mapping).
id_mapping(Model, Mapping) :-
    Model \== engine,
    saved_model_file(Model, File),
    exists_file(File),
    $,
    use_module(File, []),
    current_predicate(Model:qspace/4),
    id_mapping_(Model, Mapping).
id_mapping(_Model, Mapping) :-
    id_mapping(Mapping).

%!  qstate(+Model, ?Id, -Values, +Options)
%
%   Extract qualitative states from a saved Garp simulation If no model
%   is saved, we use the current model.

qstate(Model, State, Values, Options) :-
    Model \== engine,
    current_predicate(Model:qstate/2),
    !,
    option(d(N), Options, 3),
    Model:qstate(State, Values0),
    mapdict(keep_derivatives_(N), Values0, Values).
qstate(Model, State, Values, Options) :-
    Model \== engine,
    saved_model_file(Model, File),
    exists_file(File),
    $current_predicate(Model:qstate/2),
    !,
    qstate(Model, State, Values, Options).
qstate(_Model, State, Values, Options) :-
    qstate(State, Values, Options).

keep_derivatives_(N, _K, V0, V) :-
    keep_derivatives(N, V0, V).


		 /*******************************
		 *       NUM -> QUALITATIVE	*
		 *******************************/

series_qualitative(Series, Qualitative) :-
    maplist(state_qualitative, Series, Qualitative).

state_qualitative(Dict, QDict) :-
    dict_pairs(Dict, _, Pairs),
    maplist(value_qualitative, Pairs, QPairs),
    dict_pairs(QDict, _, QPairs).

value_qualitative(Q-V, Q-D) :-
    to_qualitative(Q, V, D).

to_qualitative(Q, d(V,D1), R) =>
    R = d(QV,QD1),
    to_qualitative(Q, V, QV),
    to_qualitative(_, D1, QD1).
to_qualitative(Q, d(V,D1,D2), R) =>
    R = d(QV,QD1,QD2),
    to_qualitative(Q, V, QV),
    to_qualitative(_, D1, QD1),
    to_qualitative(_, D2, QD2).
to_qualitative(Q, d(V,D1,D2,D3), R) =>
    R = d(QV,QD1,QD2,QD3),
    to_qualitative(Q, V, QV),
    to_qualitative(_, D1, QD1),
    to_qualitative(_, D2, QD2),
    to_qualitative(_, D3, QD3).
to_qualitative(t, T, R) => R = T.
to_qualitative(_, V, _), var(V)  => true.
to_qualitative(_, V, D), V > 0   => D = plus.
to_qualitative(_, V, D), V < 0   => D = min.
to_qualitative(_, V, D), V =:= 0 => D = zero.

%!  opt_link_garp_states(+QSeries0, -QSeries, +Options) is det.

opt_link_garp_states(QSeries0, QSeries, Options) :-
    option(link_garp_states(true), Options),
    !,
    link_garp_states(QSeries0, QSeries, Options).
opt_link_garp_states(Series, Series, _).

%!  link_garp_states(+QSeries0, -QSeries, +Options) is det.

link_garp_states(QSeries0, QSeries, Options) :-
    option(model(Model), Options, engine),
    findall(Id-State, qstate(Model, Id, State, Options), GarpStates),
    option(garp_states(GarpStates), Options, _),
    maplist(add_state(GarpStates), QSeries0, QSeries).

add_state(GarpStates, State0, State) :-
    include(matching_state(State0), GarpStates, Matching),
    pairs_keys(Matching, StateIds),
    State = State0.put(garp_states, StateIds).

matching_state(State, _Id-GarpState) :-
    \+ \+ State >:< GarpState.

%!  q_series(+Model, -QSeries, +Options) is det.
%
%   Options
%
%     - d(N)
%     Add up to the Nth derivative.  Default 3.
%     - model(Model)
%     (Saved) Garp model to compare against.

q_series(Source, QSeries, Options) :-
    option(model(Model), Options, engine),
    id_mapping(Model, Mapping),
    simulate(Source, Series,
             [ track(all),
               id_mapping(Mapping)
             | Options
             ]),
    nq_series(Series, QSeries, Options).

%!  nq_series(+Series, -QSeries, +Options) is det.
%
%   Map the numeric Series into a qualitative QSeries.  Options:
%
%     - d(N)
%     Add up to the Nth derivative.  Default 3.
%     - match(Derivatives)
%     Derivatives is a dict `Quantity -> Derivative`, where
%     `Derivative` is -1..3 that determines how many derivatives
%     to match.  -1 does not match the quantity at all.
%     - link_garp_states(+Bool)
%     Find related Garp states.

nq_series(Series, QSeries, Options) :-
    deleted_unmatched(Series, Series1, Options),
    add_derivatives(Series1, SeriesD, Options),
    series_qualitative(SeriesD, QSeries0),
    simplify_qseries(QSeries0, QSeries1),
    opt_link_garp_states(QSeries1, QSeries, Options).

%!  deleted_unmatched(+AllSeries, -Series, +Options)
%
%   Delete the quantities set to -1 in the match(Match) option.

deleted_unmatched(Series0, Series, Options) :-
    option(match(Match), Options, #{}),
    findall(K-_, get_dict(K, Match, -1), Del),
    Del \== [],
    !,
    dict_pairs(DelDict, _, Del),
    maplist(delete_keys(DelDict), Series0, Series).
deleted_unmatched(Series, Series, _).

delete_keys(Del, Dict0, Dict) :-
    dict_same_keys(Del, Free),
    select_dict(Free, Dict0, Dict).

%!  add_derivatives(+Series0, -Series, +Options) is det.

add_derivatives(Series0, Series, Options) :-
    max_derivative_used(Options, MaxD),
    add_derivatives_(MaxD, Series0, Series1),
    (   option(match(Match), Options),
        Match._ > 0
    ->  maplist(strip_derivatives(Match), Series1, Series)
    ;   Series = Series1
    ).

max_derivative_used(Options, D) :-
    option(d(N), Options, 3),
    option(match(Match), Options, #{}),
    dict_pairs(Match, _, Pairs),
    pairs_values(Pairs, Values),
    max_list([N|Values], D).

add_derivatives_(0, Series, Series) :-
    !.
add_derivatives_(N, Series0, Series) :-
    add_derivative(Series0, Series1),
    N2 is N - 1,
    add_derivatives_(N2, Series1, Series).

strip_derivatives(Match, State0, State) :-
    mapdict(strip_derivatives_(Match), State0, State).

strip_derivatives_(Match, K, V0, V) :-
    Keep = Match.get(K),
    !,
    keep_derivatives(Keep, V0, V).
strip_derivatives_(_, _, V, V).

keep_derivatives(0, D, V) =>
    arg(1, D, V).
keep_derivatives(1, D, R) =>
    R = d(V,D1),
    arg(1, D, V),
    arg(2, D, D1).
keep_derivatives(2, D, R) =>
    R = d(V,D1,D2),
    arg(1, D, V),
    arg(2, D, D1),
    arg(3, D, D2).
keep_derivatives(3, D, R) =>
    R = d(V,D1,D2,D3),
    arg(1, D, V),
    arg(2, D, D1),
    arg(3, D, D2),
    arg(4, D, D3).

%!  simplify_qseries(+QSeries0, -QSeries) is det.
%
%   Tasks:
%
%     - Remove subsequent qualitative states that are equal.
%     - Add intermediates if a continuous quantity moves in
%       the quantity space.
%
%   @tbd: swap insert_points/2.   For that we need to keep
%   first and last of am equal series to keep the start and
%   end time.

simplify_qseries(Series0, Series) :-
    insert_points(Series0, Series1),
    removes_equal_sequences(Series1, Series).

removes_equal_sequences([], T) => T = [].
removes_equal_sequences([S1,S2], T) => T = [S1,S2]. % keep last
removes_equal_sequences([S1,S2|T0], T), same_qstate(S1, S2) =>
    removes_equal_sequences([S1|T0], T).
removes_equal_sequences([S1|T0], T) =>
    T = [S1|T1],
    removes_equal_sequences(T0, T1).

same_qstate(S1, S2),
    is_dict(S1, Tag),
    is_dict(S2, Tag),
    del_dict(t, S1, _, A),
    del_dict(t, S2, _, B) =>
    A == B.

insert_points([], []).
insert_points([S1,S2|T0], [S1,Si|T]) :-
    insert_point(S1, S2, Si),
    !,
    insert_points([S2|T0], T).
insert_points([S1|T0], [S1|T]) :-
    insert_points(T0, T).

insert_point(S1, S2, Si) :-
    dict_pairs(S1, _, Pairs),
    maplist(insert_value(S2, Done), Pairs, PairsI),
    Done == true,
    dict_pairs(Si, _, PairsI).

:- det(insert_value/4).
insert_value(S2, _, t-V1, t-Vi) :-
    !,
    get_dict(t, S2, V2),
    Vi is (V1+V2)/2.
insert_value(S2, Done, K-V1, K-Vi) :-
    get_dict(K, S2, V2),
    insert_value_(V1, V2, Vi, Done).

insert_value_(d(V1,D11,D12,D13), d(V2,D21,D22,D23), R, Done) =>
    R = d(Vi,D1i,D2i,D3i),
    insert_value_(V1, V2, Vi, Done),
    insert_value_(D11, D21, D1i, Done),
    insert_value_(D12, D22, D2i, Done),
    insert_value_(D13, D23, D3i, Done).
insert_value_(d(V1,D11,D12), d(V2,D21,D22), R, Done) =>
    R = d(Vi, D1i, D2i),
    insert_value_(V1, V2, Vi, Done),
    insert_value_(D11, D21, D1i, Done),
    insert_value_(D12, D22, D2i, Done).
insert_value_(d(V1,D11), d(V2,D21), R, Done) =>
    R = d(Vi, D1i),
    insert_value_(V1, V2, Vi, Done),
    insert_value_(D11, D21, D1i, Done).
insert_value_(min, plus, Vi, Done) => Vi = zero, Done = true.
insert_value_(plus, min, Vi, Done) => Vi = zero, Done = true.
insert_value_(V, V, Vi, _Done) => Vi = V.
insert_value_(zero, V, Vi, _Done) => Vi = V.
insert_value_(V, zero, Vi, _Done) => Vi = V.
insert_value_(Var, _, _, _Done), var(Var) => true.
insert_value_(_, Var, _, _Done), var(Var) => true.


		 /*******************************
		 *              CSV		*
		 *******************************/

%!  q_series_table(+Qseries, -Table, +IdMapping)

q_series_table(QSeries, Table) :-
    id_mapping(IdMapping),
    q_series_table(QSeries, Table, IdMapping).

q_series_table(QSeries, [Title|Rows], IdMapping) :-
    QSeries = [First|_],
    dict_keys(First, Keys0),
    order_keys(Keys0, Keys),
    phrase(q_title_row(Keys, First, IdMapping), TitleCells),
    Title =.. [row|TitleCells],
    maplist(q_sample_row(Keys), QSeries, Rows).

q_sample_row(Keys, QSample, Row) :-
    phrase(q_sample_cols(Keys, QSample), Cells),
    Row =.. [row|Cells].

q_sample_cols([], _) -->
    !.
q_sample_cols([H|T], QSample) -->
    q_sample_cell(QSample.H),
    q_sample_cols(T, QSample).

q_sample_cell(V) -->
    { compound(V),
      compound_name_arguments(V, d, Args)
    },
    !,
    sequence(one, Args).
q_sample_cell(H) -->
    { is_list(H),
      !,
      atomics_to_string(H, ",", V)
    },
    [V].
q_sample_cell(H) -->
    { float(H),
      !,
      round_float(4, H, V)
    },
    [V].
q_sample_cell(H) -->
    [H].

q_title_row([], _, _) --> [].
q_title_row([H|T], First, IdMapping) -->
    q_title_cell(H, First, IdMapping),
    q_title_row(T, First, IdMapping).

q_title_cell(Key, Sample, IdMapping) -->
    { compound(Sample.Key),
      compound_name_arguments(Sample.Key, d, Args),
      !,
      key_label(IdMapping, Key, Lbl0)
    },
    derivative_titles(Args, 0, Lbl0).
q_title_cell(Key, _Sample, IdMapping) -->
    { key_label(IdMapping, Key, Label)
    },
    [Label].

one(C) -->
    [C].

derivative_titles([], _, _) -->
    [].
derivative_titles([_|T], N, Lbl0) -->
    derivative_title(N,Lbl0),
    {N2 is N+1},
    derivative_titles(T, N2, Lbl0).

derivative_title(0, Lbl0) -->
    !,
    [ Lbl0 ].
derivative_title(N, Lbl0) -->
    { format(string(Label), '~w (D~d)', [Lbl0, N])
    },
    [ Label ].

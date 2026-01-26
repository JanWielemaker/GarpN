:- module(map,
          [ id_mapping/1,               % -Mapping:dict
            id_mapping/2,               % +Model, -Mapping
            q_series/3,                 % +Model, -QSeries, +Options
            link_garp_states/3,         % +QSeries0, -QSeries, +Options
            q_series_table/5,           % +Model, +QSeries, -Table,
                                        % +IdMapping, +Options
            nq_series/3,                % +Series, -QSeries, +Options
            qstate/4,                   % +Model, ?Id, -Values, +Options
            zero_asymptote/2,           % +Values, +Options
            q_rel/2,                    % +Model, -Rel
            q_input_state/2,            % +Model, -Dict
            q_exogenous/3,              % +Model, ?Quantity, ?Exegenous
            exogenous/1,                % ?Class
            m_qspace/4,                 % +ModelId, ?QspaceId, ?Qspace, ?Values
            qspace_point_value/2,       % +Name, -Number
            linked_state//2,            % ?State, ?To
            not_linked_states//1,       % -States
            validate_correspondences/3, % +QSeriesIn, -QSeries, +Options
            q_partial_ordering/3        % +ModelId, -Ordering, +Options
          ]).
:- if(\+current_prolog_flag(dynalearn, true)).
:- export(save_garp_results/1).
:- export(saved_model_file/2).
:- endif.

:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(pairs)).
:- use_module(library(dicts)).
:- use_module(library(dcg/high_order)).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(listing)).
:- use_module(library(aggregate)).
:- use_module(library(pprint)).
:- use_module(library(dcg/basics)).

:- use_module(gsim).
:- use_module(csv_util).
:- use_module(model, [correspondences/2]).

/** <module> Map qualitative and quantitative (simulation) model
*/

%!  id_mapping(-Mapping:dict) is det.
%!  id_mapping(+Model, -Mapping:dict) is det.
%
%   Compute the mapping of  Garp  quantity   names  to  number_of(Q)  or
%   growth(Q) terms.

id_mapping(Mapping) :-
    id_mapping(engine, Mapping).

id_mapping(none, Mapping) =>
    Mapping = #{}.
id_mapping(Model, Mapping) =>
    findall(Id-Term, id_map(Model, Id, Term), Pairs),
    dict_pairs(Mapping, _, Pairs).

id_map(Model, Id, Term) :-
    m_qspace(Model, Id, Term4, _Values),
    Term4 =.. [DV,Q|_],
    Term =.. [DV,Q].

:- if(\+current_prolog_flag(dynalearn, true)).
%!  qstate(?Id, -Values, +Options)
%
%   Get the qualitative state from Garp  in   the  same  notation as our
%   simulator.  Always runs in `engine`.

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
    ->  N2 >= 0,
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
unknown_var(interval, _) => true.
unknown_var(?, _) => true.
unknown_var(pos, Val) => Val = plus.    % used for derivatives
unknown_var(neg, Val) => Val = min.
unknown_var(plus, Val) => Val = plus.   % quantity space values
unknown_var(zero, Val) => Val = zero.
unknown_var(min,  Val) => Val = min.
unknown_var(Val0, Val), qspace_val(Val0) => Val = Val0.

qspace_val(Val) :-
    m_qspace(engine, _QspaceId, _Qspace, Values),
    (   memberchk(point(Val), Values)
    ->  true
    ;   memberchk(Val, Values)
    ).


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

    format(Out, '%!  qspace(?ParameterInstance, ?ParameterDef, \c
                            ?ValueList, ?Fail).~n~n', []),
    forall(engine:qspace(Id, Term4, Values, Fail),
           format(Out, '~q.~n',  [qspace(Id, Term4, Values, Fail)])),

    save_scenario(Out),
    save_garp_relations(Out),
    save_exogenous(Out),

    format(Out, '~n~n%!   qstate(?State, ?Values).~n~n', []),
    forall(qstate(State, Values, []),
           portray_clause(Out, qstate(State, Values))),

    format(Out, '~n~n%!   qstate_from(?State, ?From:list).~n~n', []),
    forall(engine:state_from(S, S0),
           portray_clause(Out, qstate_from(S,S0))),

    format(Out, '~n~n%!   qstate_to(?State, ?Cause).~n~n', []),
    forall(engine:state_to(S, Cause),
           print_term(qstate_to(S,Cause),
                      [ output(Out),
                        fullstop(true),
                        nl(true)
                      ])).

save_scenario(Out) :-
    q_input_state(engine, Dict),
    format(Out, '~n~n%!   input_state(-Dict).~n~n', []),
    portray_clause(Out, input_state(Dict)).

input_value(value(Q, _, Value, _), Q-Value).

save_exogenous(Out) :-
    format(Out, '~n~n%!   exogenous(?Quantity, ?Function).~n~n', []),
    findall(exogenous(Q,F), q_exogenous(engine, Q, F), Terms),
    (   Terms == []
    ->  format(Out, ':- multifile exogenous/2.~n', [])
    ;   forall(member(Clause, Terms),
               portray_clause(Out, Clause))
    ).

save_garp_relations(Out) :-
    engine_relations(Relations),
    format(Out, '~n~n%!   qrel(?Rel).~n~n', []),
    (   Relations \== []
    ->  forall(member(Rel, Relations),
               format(Out, '~q.~n', [qrel(Rel)]))
    ;   portray_clause(Out, (qrel(norel) :- fail))
    ).

engine_relations(Relations) :-
    engine:state(1, SMD),
    arg(_, SMD, par_relations(Relations)),
    !.

saved_model_file(Model, File) :-
    format(atom(File), 'garp/~w.db', [Model]).
:- endif.            % \+ current_prolog_flag(dynalearn, true)

%!  m_qspace(+ModelId, ?QspaceId, ?Qspace, ?Values) is nondet.

:- if(current_prolog_flag(dynalearn, true)).
m_qspace(none, _QspaceId, _Qspace, _Values) =>
    fail.
m_qspace(ModelId, QspaceId, Qspace, Values) =>
    dynalearn_model(ModelId, ModelData),
    member(qspace(QspaceId, Qspace, Values, fail), ModelData.qspaces).
:- else.
m_qspace(engine, QspaceId, Qspace, Values) =>
    engine:qspace(QspaceId, Qspace, Values, fail).
m_qspace(Model, QspaceId, Qspace, Values) =>
    ensure_loaded_model(Model, qspace/4),
    Model:qspace(QspaceId, Qspace, Values, fail).
:- endif.

%!  q_rel(+Model, -Rel) is nondet.
%
%   True when Rel is a relation in Model.

:- if(current_prolog_flag(dynalearn, true)).
q_rel(ModelId, QRel) :-
    dynalearn_model(ModelId, ModelData),
    member(QRel, ModelData.qrels).
:- else.
q_rel(engine, Rel) :-
    !,
    engine_relations(Relations),
    member(Rel, Relations).
q_rel(Model, Rel) :-
    ensure_loaded_model(Model, qrel/1),
    Model:qrel(Rel).

ensure_loaded_model(Model, PI) :-
    current_predicate(Model:PI),
    !.
ensure_loaded_model(Model, PI) :-
    saved_model_file(Model, File),
    exists_file(File),
    use_module(File, []),
    assertion(current_predicate(Model:PI)).
:- endif.

%!  qstate(+ModelId, ?StateId, -Values, +Options) is nondet.
%
%   Extract qualitative states from a saved  Garp simulation If no model
%   is saved, we use the current model.  This predicate adds the virtual
%   state `0` to represent the initial state.

qstate(none, _Id, _Values, _Options) =>
    fail.
qstate(ModelId, StateId, Values, Options) =>
    (   StateId = 0,
        q_input_state(ModelId, Values0)
    ;   qstate_(ModelId, StateId, Values0, Options)
    ),
    select_derivatives(Values0, Values, Options).

:- if(current_prolog_flag(dynalearn, true)).
qstate_(ModelId, State, Values, _Options) =>
    dynalearn_model(ModelId, ModelData),
    member(qstate(State, Values), ModelData.results.qstates).
:- else.
qstate_(engine, State, Values, Options) =>
    qstate(State, Values, Options).
qstate_(Model, State, Values, _Options) =>
    ensure_loaded_model(Model, qstate/2),
    Model:qstate(State, Values).
:- endif.

select_derivatives(Values0, Values, Options) :-
    option(match(Match), Options, #{}),
    dict_pairs(Values0, Tag, Pairs0),
    convlist(keep_derivatives_(Match), Pairs0, Pairs),
    dict_pairs(Values, Tag, Pairs).

keep_derivatives_(Match, K-V0, K-V) :-
    (   N2 = Match.get(K)
    ->  N2 \== [],
        keep_derivatives(N2, V0, V)
    ;   keep_derivatives([0,1], V0, V)
    ).

%!  q_input_state(+ModelId, -Dict)
%
%   True when Dict is a dict holding   the  (quantity space) start value
%   for each quantity.

:- if(current_prolog_flag(dynalearn, true)).
q_input_state(ModelId, Dict) :-
    dynalearn_model(ModelId, ModelData),
    Dict = ModelData.get(input_state).
:- else.
q_input_state(engine, Dict) =>
    engine:scenario_state(SMD),
    once(arg(_, SMD, par_values(Values))),
    maplist(input_value, Values, Pairs),
    dict_pairs(Dict, _, Pairs).
q_input_state(Model, Dict) =>
    ensure_loaded_model(Model, input_state/1),
    Model:input_state(Dict).
:- endif.

%!  q_exogenous(+Model, ?Quantity, ?Exegenous) is nondet.
%
%   True when Quantity is  determined   exogenously.  Possible exogenous
%   functions are pre-wired and specified by exogenous/1.

:- if(current_prolog_flag(dynalearn, true)).
q_exogenous(ModelId, Quantity, Exegenous) :-
    dynalearn_model(ModelId, ModelData),
    member(exogenous(Quantity, Exegenous), ModelData.exogenous).
:- else.
q_exogenous(engine, Quantity, Exegenous) =>
    engine:state(1, SMD),
    arg(6, SMD, system_structures(Structures)),
    member(Structure, Structures),
    arg(3, Structure, conditions(Conditions)),
    memberchk(system_elements(SEList), Conditions),
    arg(4, Structure, givens(Givens)),
    member(parameters(Params), Givens),
    member(Param, Params),
    arg(1, Param, Entity),
    arg(2, Param, Quantity),
    (   member(has_attribute(Entity, has_assumption, Instance), SEList),
        se_isa(Instance, Exegenous, SEList),
        exogenous(Exegenous)
    ->  true
    ).
q_exogenous(Model, Quantity, Exegenous) =>
    ensure_loaded_model(Model, exogenous/2),
    Model:exogenous(Quantity, Exegenous).

se_isa(Instance, Instance, _).
se_isa(Instance, Super, SEList) :-
    member(instance(Instance, Parent), SEList),
    se_isa(Parent, Super, SEList).
:- endif.

%!  m_qstate_from(+ModelId, ?State, ?FromList) is nondet.
%
%   True when State can be reached from FromList.

m_qstate_from(ModelId, State, FromList) :-
    m_qstate_from_(ModelId, State, FromList0),
    add_initial_state(FromList0, FromList).

add_initial_state([], From) => From = [0].
add_initial_state(From0, From) => From0 = From.

:- if(current_prolog_flag(dynalearn, true)).
m_qstate_from_(ModelId, State, From) :-
    dynalearn_model(ModelId, ModelData),
    member(qstate_from(State, From), ModelData.results.qstate_graph).
:- else.
m_qstate_from_(engine, State, From) =>
    engine:qstate_from(State, From).
m_qstate_from_(Model, State, From) =>
    ensure_loaded_model(Model, qstate_from/2),
    Model:qstate_from(State, From).
:- endif.

%!  q_partial_ordering(+ModelId, -Ordering:list(list(atom)), +Options)
%!                     is det.
%
%   Estabilish a partial ordering of quantities based on the first state
%   in which they have a value.  Options:
%
%     - constants(remove)
%       Remove constants quantities from the set.

q_partial_ordering(ModelId, Ordering, Options) :-
    findall(State-Values,
            qstate(ModelId, State, Values, [d(1)|Options]),
            Pairs),
    maplist(state_into_dict, Pairs, Data0),
    dicts_to_same_keys(Data0, q_unknown, Data),
    Data = [First|_],
    dict_keys(First, Keys),
    map_list_to_pairs(changed_at(First, Data), Keys, KeyPairs),
    keysort(KeyPairs, SortedPairs),
    group_pairs_by_key(SortedPairs, Keyed),
    (   option(constants(remove), Options)
    ->  delete(Keyed, 0-_, Keyed1)
    ;   Keyed1 = Keyed
    ),
    pairs_values(Keyed1, Ordering).

state_into_dict(State-Dict0, Dict) :-
    Dict = Dict0.put(state, State).

q_unknown(_Key, _Dict, d(_,_,_,_)).

changed_at(First, Data, Key, Nth) :-
    d(V0,_,_,_) = First.get(Key, _),
    nth0(Nth, Data, Record),
    d(V1,_,_,_) = Record.get(Key),
    V1 \=@= V0,
    !.
changed_at(First, _Data, Key, 0) :-  % number are @< atoms
    d(V0,_,_,_) = First.get(Key, _),
    nonvar(V0),
    !.
changed_at(_, _, _, infinite(arg)).  % compounds are @> atoms

%!  exogenous(?Class)
%
%   True when Class identifies an exogenous object.

exogenous(exogenous_free).      % Random
exogenous(exogenous_steady).
exogenous(exogenous_increasing).
exogenous(exogenous_decreasing).
exogenous(exogenous_sinus).
exogenous(exogenous_pos_parabola).
exogenous(exogenous_neg_parabola).

%!  asymptotes(+Series, -Asymptotes, +Options) is det.
%
%   True when Asymptotes  is  a  list   of  Key-Der  pairs  denoting the
%   asymptotic traces.

asymptotes(Series, Asymptotes, Options) :-
    Series = [First|_],
    dict_keys(First, Keys0),
    delete(Keys0, t, Keys),
    maplist(series_key_derivative(Series), Keys, Ders),
    asymptotes(Keys, Ders, Series, Asymptotes, Options).

asymptotes([], _, _, [], _).
asymptotes([K|KT], [D|DT], Series, Asymptotes, Options) :-
    maplist(state_trace_value(K-D, _Empty), Series, Values),
    (   zero_asymptote(Values, [type(How)|Options])
    ->  Asymptotes = [asymptote(K,D,How)|TA],
        D1 is D-1
    ;   Asymptotes = TA,
        D1 = -1                 % if this derivative is not an asymptote,
    ),                          % lower derivatives are surely not one
    (   D1 >= 0
    ->  asymptotes([K|KT], [D1|DT], Series, TA, Options)
    ;   asymptotes(KT, DT, Series, TA, Options)
    ).

%!  stable_from(+Series, -Asymptotes, -Time, +Options) is semidet.
%
%   True when some traces in Series are asymptotes towards zero.
%
%   @arg Asymptotes is a list of asymptote(Key,Derivative,How) terms.

stable_from(Series, Asymptotes, Time, Options) :-
    asymptotes(Series, Asymptotes, Options),
    maplist(zero_from(Series, Options), Asymptotes, Nths),
    max_list(Nths, Nth),
    nth0(Nth, Series, State),
    d(Time,_,_,_) = State.t.

zero_from(Series, Options, asymptote(K,D,How), Nth) :-
    maplist(state_trace_value(K-D, _Empty), Series, Values),
    min_list_normal(Values, Min),
    max_list_normal(Values, Max),
    option(fraction(Frac), Options, 20),
    skip_leadin(Values, Skipped, Consider, Options),
    zero_from(How, Min, Max, Consider, Nth0, Frac),
    length(Skipped, NSkipped),
    Nth is NSkipped+Nth0.

zero_from(asc, Min, _, Consider, Nth0, Frac) =>
    MinV is Min/Frac,
    nth0(Nth0, Consider, Value),
    Value > MinV.
zero_from(desc, _, Max, Consider, Nth0, Frac) =>
    MaxV is Max/Frac,
    nth0(Nth0, Consider, Value),
    Value < MaxV.
zero_from(osc, Min, Max, Consider, Nth0, Frac) =>
    MinV is Min/Frac,
    MaxV is Max/Frac,
    reverse(Consider, Values),
    nth0(Nth00, Values, Value),
    (   Value < MinV
    ;   Value > MaxV
    ),
    !,
    length(Consider, Len),
    Nth0 is Len-Nth00.

%!  zero_asymptote(+Values, +Options) is semidet.
%
%   Find whether Values approaches zero. Note that we will start finding
%   asymptotes from the highest considered   derivative,  so looking for
%   zero is enough.  Scenarios:
%
%     - Value is monotonic and slope decreases monotonic.
%     - Value oscillates with decreasing amplitude
%       - Create sequence of local min/max and prove both to be
%         asymtotic.
%
%    Options:
%
%      - fraction(+Number)
%        Demand the end values to be Number times smaller than the max.
%        Default is 20.
%      - min_cycles(+Integer)
%        Demand that decreasing oscillation passes at least Integer
%        cycles.  Default is 10.
%      - skip(+Percentage)
%        Skip the first Percentage Values.  Default 10%.
%      - type(-Type)
%        One of `desc` (descending to zero), `asc` (ascending to zero)
%        or `osc` (oscillating with ever smaller amplitude to zero).

zero_asymptote(Values, Options) :-
    skip_leadin(Values, _Skipped, Consider, Options),
    option(type(How), Options, _),
    zero_asymptote(Consider, 1, How, Options).

skip_leadin(Values, Head, Values1, Options) :-
    option(skip(Perc), Options, 30),
    (   Perc > 0
    ->  length(Values, Len),
        HeadLen is round((Len*Perc)/100),
        length(Head, HeadLen),
        append(Head, Values1, Values)
    ;   Values1 = Values,
        Head = []
    ).

zero_asymptote(Values, Level, How, Options) :-
    option(fraction(Frac), Options, 20),
    min_list_normal(Values, Min),
    max_list_normal(Values, Max),
    last(Values, Last),
    normal_number(Last),
    (   Max > 0,
        Min >= 0,
        Last < Max/Frac
    ->  monotonic_decreasing(Values),
        How = desc
    ;   Min < 0,
        Max =< 0,
        Last > Min/Frac
    ->  maplist(neg, Values, Values1),
        monotonic_decreasing(Values1),
        How = asc
    ;   abs(Last) < max(abs(Max),abs(Min))
    ->  option(min_cycles(Min), Options, 10),
        local_extremes(Values, Highs, Lows),
        length(Highs, LH), LH >= Min,
        length(Lows, LL), LL >= Min,
        Level1 is Level + 1,
        Level =< 5,
        zero_asymptote(Highs, Level1, _, Options),
        zero_asymptote(Lows, Level1, _, Options),
        How = osc
    ).

neg(Y, Yneg) :- Yneg is -Y.

%!  monotonic_decreasing(+Values) is semidet.
%
%   True when Values is monotonic  decreasing   towards  zero. We assume
%   this to be true if
%
%     - The values are derivative are monotonically decreasing
%     - The above stops, but all remaining values are less then
%       0.001 of the initial.  The latter covers cases where
%       rounding errors come into play.  Alternatively, we could
%       average over some value.

monotonic_decreasing([H1,H2|T]) :-
    normal_number(H1),
    normal_number(H2),
    D is H1-H2,
    D >= 0,
    monotonic_decreasing(T, H2, D, H1).

monotonic_decreasing([], _, _, _).
monotonic_decreasing([H|T], V, D, Scale) :-
    normal_number(H),
    D2 is V-H,
    D2 >= 0,
    D2 =< D,
    !,
    monotonic_decreasing(T, H, D2, Scale).
monotonic_decreasing(Remainder, _, _, Scale) :-
    Max is Scale/1000,
    forall(member(N, Remainder),
           (   normal_number(N),
               Max > N)).

%!  local_extremes(+Values, -Highs, -Lows) is det.
%
%   Find the local minima and maxima of a series.

local_extremes([V1,V2,V3|T0], Highs, Lows) :-
    normal_number(V1),
    normal_number(V2),
    normal_number(V3),
    V2 > V1,
    $,
    (   V3 < V2
    ->  Highs = [V2|T],
        local_extremes([V3|T0], T, Lows)
    ;   V3 =:= V2
    ->  (   append(_, [V4|T1], T0),
            (   V4 < V2
            ->  !,
                Highs = [V2|T]
            ;   V4 > V2
            ->  !,
                Highs = T
            )
        ->  local_extremes([V4|T1], T, Lows)
        ;   Highs = [],
            Lows = []
        )
    ;   local_extremes([V2,V3|T0], Highs, Lows)
    ).
local_extremes([V1,V2,V3|T0], Highs, Lows) :-
    normal_number(V1),
    normal_number(V2),
    normal_number(V3),
    V2 < V1,
    $,
    (   V3 > V2
    ->  Lows = [V2|T],
        local_extremes([V3|T0], Highs, T)
    ;   V3 =:= V2
    ->  (   append(_, [V4|T1], T0),
            (   V4 > V2
            ->  !,
                Lows = [V2|T]
            ;   V4 < V2
            ->  !,
                Lows = T
            )
        ->  local_extremes([V4|T1], Highs, T)
        ;   Lows = []
        )
    ;   local_extremes([V2,V3|T0], Highs, Lows)
    ).
local_extremes(_, [], []).


		 /*******************************
		 *       NUM -> QUALITATIVE	*
		 *******************************/

%!  series_qualitative(+Series, -Qualitative, +Options) is det.
%
%   Where Series is a list of  dicts   holding  values, Qualitative is a
%   list of qualitative states.

series_qualitative(Series, Qualitative, Options) :-
    stable_from(Series, Asymptotes, Time, Options),
    !,
    option(qspaces(QSpaces), Options),
    stable_min_derivatives(Asymptotes, Asymptotes1),
    last(Series, LastState),
    state_qualitative_final(Asymptotes1, QSpaces, LastState, LastQState),
    maplist(state_qualitative_a(Asymptotes1,Time,QSpaces,LastQState), Series, Qualitative).
series_qualitative(Series, Qualitative, Options) :-
    option(qspaces(QSpaces), Options),
    maplist(state_qualitative(QSpaces), Series, Qualitative).

stable_min_derivatives(Asymptotes0, Asymptotes) :-
    sort(Asymptotes0, Asymptotes1),
    stable_min_derivatives_(Asymptotes1, Asymptotes).

stable_min_derivatives_([], []).
stable_min_derivatives_([H1,H2|T0], T) :-
    arg(1, H1, Key),
    arg(1, H2, Key),
    !,
    stable_min_derivatives_([H1|T0], T).
stable_min_derivatives_([H|T0], [H|T]) :-
    stable_min_derivatives_(T0, T).

%!  state_qualitative_a(+Asymptotes, +Time, +QSpaces,
%!                      +LastQState, +NDict, -QDict) is det.
%
%   Turn the numeric  state  NDict  into   a  qualitative  state  QDict,
%   considering that all quantities in  Asymptotes are considered `zero`
%   after  Time.  If  the  first  derivative  is  considered  zero,  the
%   qualitative value is taken from LastQState.

:- det(state_qualitative_a/6).
state_qualitative_a(Asymptotes, Time, QSpaces, LastQState, Dict, QDict) :-
    d(DictTime,_,_,_) = Dict.t,
    (   DictTime >= Time
    ->  mapdict(to_qualitative_z(true,Asymptotes,QSpaces,LastQState), Dict, QDict)
    ;   mapdict(to_qualitative(true, QSpaces), Dict, QDict)
    ).

state_qualitative_final(Asymptotes, QSpaces, Dict, QDict) :-
    mapdict(to_qualitative_z(false, Asymptotes, QSpaces, #{}), Dict, QDict).

state_qualitative(QSpaces, Dict, QDict) :-
    mapdict(to_qualitative(true, QSpaces), Dict, QDict).

to_qualitative_z(Exact, Asymptotes, QSpaces, LastQState, Q, V0, R) :-
    to_qualitative(Exact, QSpaces, Q, V0, R0),
    (   memberchk(asymptote(Q,D,_), Asymptotes)
    ->  QSpace = QSpaces.get(Q, [point(zero)]),
        (   d(QVal,_,_,_) = LastQState.get(Q)
        ->  true
        ;   arg(1, R0, QVal)
        ),
        to_zero(D, QSpace, QVal, R0, R)
    ;   R = R0
    ).

%!  to_zero(+N, +QSpace, +QVal, +VIn, -VOut) is det.
%
%   Keep the first N  of  value  or   d(V,D1,...)  as  is,  mapping  all
%   subsequent to zero. For the value, zero means point(zero), while for
%   derivatives it means the plain atom `zero`.
%
%   @arg QVal is the asymptotical qualitative value at the end of the
%   series.

:- det(to_zero/5).
to_zero(0, QSpace, _, _, R) =>
    R = d(Zero, zero, zero, zero),
    qspace_zero(QSpace, Zero).
to_zero(1, _QSpace, QVal, d(_,_,_,_), R) =>
    R = d(QVal, zero, zero, zero).
to_zero(2, _QSpace, _, d(V,D1,_,_), R) =>
    R = d(V, D1, zero, zero).
to_zero(3, _QSpace, _, d(V,D1,D2,_), R) =>
    R = d(V, D1, D2, zero).

qspace_zero(QSpace, Zero) :-
    member(point(Name=Val), QSpace),
    Val =:= 0,
    !,
    Zero = point(Name).
qspace_zero(_, point(zero)).

%!  to_qualitative(+Exact, +QSpaces, +Quantity, +NumIn, -QOut) is det.
%
%   Map a numeric result NumIn for Quantity into a qualitative result
%   QOut.
%
%   @arg NumIn is either a plain number, a term d(V,D1), term d(V,D1,D2)
%   or term d(V,D1,D3).  Each Dn must be mapped to plus/min/zero.  Each
%   V must be mapped to the quantity space.

to_qualitative(_, _, t, T, R) => R = T.
to_qualitative(Exact, QSpaces, Q, d(V,D1,D2,D3), R) =>
    R = d(QV,QD1,QD2,QD3),
    to_qualitative_v(Exact, QSpaces, Q, V, QV),
    to_qualitative_d(D1, QD1),
    to_qualitative_d(D2, QD2),
    to_qualitative_d(D3, QD3).

to_qualitative_d(V, _), \+ normal_number(V) => true.
to_qualitative_d(V, D), V > 0   => D = plus.
to_qualitative_d(V, D), V < 0   => D = min.
to_qualitative_d(V, D), V =:= 0 => D = zero.

to_qualitative_v(_, _QSpaces, _, V, _), \+ normal_number(V) => true.
to_qualitative_v(Exact, QSpaces, Q, V, VQ), QSpace = QSpaces.get(Q) =>
    n_to_qualitative(QSpace, Exact, Q, V, VQ).
to_qualitative_v(_, _, _, V, VQ) =>
    to_qualitative_d(V, VQ).

%!  n_to_qualitative(+QSpace, +Exact, +Quantity, +Num, -QValue) is det.
%
%   Map Num into a qualitative name from QSpace.
%
%   @arg QSpace is the value list defining the quantity space.
%   Its values are atoms (internal names), point(Name) or
%   point(Name=Value).
%   @arg Exact is a bool.  If `false`, matching values is done using
%   eq_approx/2, which allows for a 0.001 (1%%) flexibility.
%   @arg QValue is one of
%        - point(PointName)
%        - IntervalName.
%        - error(Error)
%          The numeric value is outside the quantity space. Error is one
%          of `underflow` or `overflow`.

n_to_qualitative([Name], _, _Q, _V, VQ), atom(Name) =>
    VQ = Name.
n_to_qualitative(Values, false, Q, V, VQ),
    member(point(NP), Values),
    qspace_point_value(Q, NP, P, PV),
    eq_approx(PV, V) =>
    VQ = point(P).
n_to_qualitative(Values, _, Q, V, VQ) =>
    n_to_qualitative(Values, Q, V, VQ).

n_to_qualitative([Name,point(NP)|_], Q, V, VQ),
    qspace_point_value(Q, NP, P, PV),
    V =< PV =>
    (   PV =:= V
    ->  VQ = point(P)
    ;   VQ = Name
    ).
n_to_qualitative([point(NP)|_], Q, V, VQ),
    qspace_point_value(Q, NP, P, PV),
    V =< PV =>
    (   PV =:= V
    ->  VQ = point(P)
    ;   VQ = error(underflow)
    ).
n_to_qualitative(List, Q, V, VQ),
    append(_, [point(NP), Name], List),
    qspace_point_value(Q, NP, P, PV),
    V >= PV =>
    (   PV =:= V
    ->  VQ = point(P)
    ;   VQ = Name
    ).
n_to_qualitative(List, Q, V, VQ),
    append(_, [point(NP)], List),
    qspace_point_value(Q, NP, P, PV),
    V >= PV =>
    (   PV =:= V
    ->  VQ = point(P)
    ;   VQ = error(overflow)
    ).
n_to_qualitative(List, Q, V, VQ) =>
    append(_, [point(NP1),Name,point(NP2)|_], List),
    qspace_point_value(Q, NP1, P1, N1),
    qspace_point_value(Q, NP2, P2, N2),
    (   N1 =:= V
    ->  VQ = point(P1)
    ;   N2 =:= V
    ->  VQ = point(P2)
    ;   V > N1, V < N2
    ->  VQ = Name
    ),
    !.

%!  eq_approx(+Reference, +Value) is semidet.
%
%   Approximate equality testing. This is   used for asymptotic matching
%   and we allow for small mismatches.  It   is  not  clean how small is
%   "small". Should we use  information  from   the  value  range of the
%   entire series?

eq_approx(Reference, Value), Reference =:= 0 => abs(Value) < 1e-3.
eq_approx(Reference, Value) => abs(1-abs(Value/Reference)) < 1e-3.

%!  qspace_point_value(+Quantity, +Point, -Name, -Number) is det.

qspace_point_value(_Quantity, N=V, Name, Value) =>
    Name = N,
    Value = V.
qspace_point_value(_Quantity, N, Name, Value),
    qspace_point_value(N, Value0) =>
    Name = N,
    Value = Value0.

%!  qspace_point_value(+Name, -Number) is semidet.
%
%   Well known quantity space point values.

qspace_point_value(zero, V) => V = 0.0.
qspace_point_value(N, V), number(N) => V = N.
qspace_point_value(A, V), atom_number(A,N) => V = N.
qspace_point_value(_, _) => fail.

%!  opt_link_garp_states(+QSeries0, -QSeries, +Options) is det.

opt_link_garp_states(QSeries0, QSeries, Options) :-
    option(link_garp_states(true), Options),
    !,
    link_garp_states(QSeries0, QSeries, Options).
opt_link_garp_states(Series, Series, _).

%!  link_garp_states(+QSeries0, -QSeries, +Options) is det.
%
%   Link our derived qualitative states to the states generated by Garp.
%   In QSeries, each state has a property   `garp_states` that is a list
%   of Garp states that match  the   qualitative  state that we derived.
%
%   First, we perform a simple match.  Next,   we  look  at sequences of
%   unmatched states and their  corresponding   Garp  states.  This step
%   takes care of the fact that things that   happen in Garp in a single
%   state  may  be  spread  over  multiple    states  in  the  numerical
%   simulation, e.g., quantity space and derivative zero-transitions may
%   not happend exactly at the same time in the numerical simulation.
%
%   Options:
%
%     - model(+Model)
%       The Garp model.  Used to find the Garp states.
%     - garp_states(-States)
%       Unify States with the retrieved Garp states.
%     - qspaces(+Dict)
%       Dict maps quantities to a list of values, which is an
%       alternating sequence of _interval_ and `point(Name)`.
%     - equations(+Equations:list)
%       The numeric model as a list of `X := Expr` terms. Used to
%       extract 'Δt' to determine whether states are "close in time".

link_garp_states(QSeries0, QSeries, Options) :-
    option(model(Model), Options, engine),
    findall(Id-State, qstate(Model, Id, State, Options), GarpStates),
    option(garp_states(GarpStates), Options, _),
    maplist(add_state(GarpStates), QSeries0, QSeries1),
    merge_states(QSeries1, GarpStates, QSeries, Options).

%!  add_state(+GarpStates, +State0, -State) is det.
%
%   Add a property `garp_states` holding a  list of (numeric) Garp state
%   Ids for Garp states that match State0.

add_state(GarpStates, State0, State) :-
    include(matching_state(State0), GarpStates, Matching),
    remove_less_instantiated_maps(Matching, Matching1),
    pairs_keys(Matching1, StateIds),
    State = State0.put(garp_states, StateIds).

matching_state(State, _Id-GarpState) :-
    \+ \+ State >:< GarpState.

remove_less_instantiated_maps(Pairs0, Pairs) :-
    select(_Q1-S1, Pairs0, Pairs1),
    select(Q2-S2, Pairs1, Pairs2),
    subsumes_term(S1, S2),
    !,
    remove_less_instantiated_maps([Q2-S2|Pairs2], Pairs).
remove_less_instantiated_maps(Pairs, Pairs).



%!  merge_states(+QSeries0, +GarpStates, -QSeries, Options) is det.
%
%   Try to merge sequences of non-matching qualitative states into a
%   sequence that matches the garp states.

merge_states(QSeries0, GarpStates, QSeries, Options) :-
    phrase(merge_states(QSeries, GarpStates, Options), QSeries0).

merge_states(QSeries, GarpStates, Options) -->
    seq(BSeq),
    linked_state(Before, ATo),
    not_linked_states(Unmatched),
    peek_linked_state(After, ZTo),
    { (   Unmatched \== []
      ->  true
      ;   \+ garp_valid_transition(ATo, ZTo, Options),
          \+ (member(X, ATo), memberchk(X, ZTo))        % not a real transition
      ),
      member(A, ATo),
      member(Z, ZTo),
      garp_gap(A, Z, GarpStateNums, Options),
      sync_states(GarpStateNums, GarpStates, Before, Unmatched, After, Synced, Options),
      !,
      append([BSeq, [Before], Synced], Replaced),
      append(Replaced, Rest, QSeries)
    },
    merge_states(Rest, GarpStates, Options).
merge_states(QSeries, _GarpStates, _Options) -->
    remainder(QSeries).

seq([]) --> [].
seq([H|T]) --> [H], seq(T).

%!  garp_valid_transition(+FromSet, +ToSet, +Options)
%
%   True if there is a state transition   from  any member of FromSet to
%   any member of ToSet in the Garp state graph.

garp_valid_transition(FromSet, ToSet, Options) :-
    option(model(Model), Options, engine),
    member(From, FromSet),
    member(To, ToSet),
    qstate_from(Model, To, From),
    !.

%!  garp_gap(+From, +To, -Seq, +Options) is nondet.
%
%   True when Seq is a list of Garp state (numbers) that link up From to
%   To, _exluding_ From and To itself.

garp_gap(From, To, Seq, Options) :-
    option(model(Model), Options, engine),
    qstate_from(Model, First, From),
    qstate_from(Model, To, Last),
    garp_sequence(First, Last, Seq, Model).

garp_sequence(State, State, [State], _).
garp_sequence(First, Last, [First|Seq], Model) :-
    qstate_from(Model, State, First),
    garp_sequence(State, Last, Seq, Model).

qstate_from(Model, To, From) :-
    m_qstate_from(Model, To, FTo),
    member(From, FTo).

%!  sync_states(+GarpStateNums, +GarpStates,
%!              +Before, +Unmatched, +After,
%!              -Synced, +Options) is semidet.
%
%   See whether we can reduce a sequence of unmatched qualitative states
%   to a smaller sequence of Garp  states.   This  means  that for every
%   value
%
%     - If the Garp state is an interval, the interval matches for the
%       entire collapsed sequence.
%     - If the Garp state is a point (or 0 for derivative) we check that
%       the sequence passes this point. Note that the passing may happen
%       in combination with the state before or after the sequence.
%       - If the point is at the end of the quantity space, we check
%         that it leaves or arrives at the point.

sync_states([H], GarpStates, Before, Unmatched, After, [Synced], Options) :-
    option(qspaces(Qspaces), Options, #{}),
    append([[Before],Unmatched,[After]], Seq),
    memberchk(H-GState, GarpStates),
    mapdict(match_quantity(Seq, Qspaces), GState),
    vtime_stamp(Before, Unmatched, After, T),
    Synced = GState.put(#{t:T, garp_states:[H]}).

match_quantity(_, _, t, _) =>                          % Limit the time?
    true.
match_quantity(_, _, garp_states, _) =>
    true.
match_quantity(Seq, QSpaces, Q, d(V,D1,D2,D3)),
    maplist(get_dict(Q), Seq, DLs) =>
    maplist(arg(1), DLs, Values),
    match_values(V, Values, QSpaces.get(Q,[])),
    maplist(arg(2), DLs, D1s),
    match_derivatives(D1, D1s),
    maplist(arg(3), DLs, D2s),
    match_derivatives(D2, D2s),
    maplist(arg(4), DLs, D3s),
    match_derivatives(D3, D3s).
match_quantity(_Seq, _QSpaces, _Q, d(_V,_D1,_D2,_D3)) =>
    true.                                              % not simulated

match_values(V, _, _), var(V) => true.
match_values(Pt, Values, QSpace),
    phrase(i_transition(Below,Pt,Above), QSpace) =>
    phrase(match_traverse_value(Below, Pt, Above), Values).
match_values(Pt, Values, QSpace),
    phrase(i_adjacent(Pt,Interval), QSpace) =>
    (   phrase((opt_seqof(Pt),opt_seqof(Interval)), Values)
    ->  true
    ;   phrase((opt_seqof(Interval),opt_seqof(Pt)), Values)
    ->  true
    ).
match_values(Interval, Values, QSpace),
    phrase(v_between(Below, Interval, Above), QSpace) =>
    member(End, [Below, Above]),
    (   phrase((opt_seqof(End),seqof(Interval)), Values)
    ->  true
    ;   phrase((seqof(Interval),opt_seqof(End)), Values)
    ->  true
    ).
match_values(Interval, Values, QSpace),
    phrase(v_adjacent(Interval, End), QSpace) =>
    (   phrase((opt_seqof(End),seqof(Interval)), Values)
    ->  true
    ;   phrase((seqof(Interval),opt_seqof(End)), Values)
    ->  true
    ).
match_values(Interval, Values, _QSpace) =>
    phrase(seqof(Interval), Values),
    !.

%!  i_transition(-Below,+Pt,-Above)//
%
%   True if Pt is a point between two intervals

i_transition(Below,Pt,Above) -->
    ..., [Below], point(Pt), [Above], ... .

%!  i_adjacent(+Pt, -Interval)//
%
%   True if Pt is at the start or  end of a quantity space with Interval
%   adjacent.

i_adjacent(Pt, Interval) -->
    point(Pt), [Interval], ... .
i_adjacent(Pt, Interval) -->
    ..., [Interval], point(Pt).

%!  v_adjacent(+Interval, -Pt)//
%
%   True if Pt is above or below Interval

v_adjacent(Interval, Pt) -->
    { atom(Interval) },
    ..., [Interval], point(Pt).
v_adjacent(Interval, Pt) -->
    { atom(Interval) },
    point(Pt), [Interval], ... .

v_between(Below, Interval,  Above) -->
    { atom(Interval) },
    ..., point(Below), [Interval], point(Above), ... .

... --> [] ; [_], ... .


point(point(P1)) -->
    [point(P2)],
    { eq_point(P1, P2) }.

eq_point(Pt, Pt) => true.
eq_point(Name=_, Name) => true.
eq_point(Name, Name=_) => true.
eq_point(_, _) => fail.

match_derivatives(D, _DL), var(D) => true.
match_derivatives(zero,  DL) =>
    phrase(match_traverse_derivative(min, zero, plus), DL).
match_derivatives(PM,  DL) =>
    phrase(match_derivative(PM), DL),
    !.

match_traverse_value(Below, point(Pt), Above) -->
    opt_seqof(Below),
    seqof(point(Pt)),
    opt_seqof(Above),
    !.
match_traverse_value(Below, point(Pt), Above) -->
    opt_seqof(Above),
    seqof(point(Pt)),
    opt_seqof(Below),
    !.
match_traverse_value(Below, _, Above) -->
    seqof(Above),
    seqof(Below),
    !.
match_traverse_value(Below, _, Above) -->
    seqof(Below),
    seqof(Above),
    !.

match_traverse_derivative(Below, zero, Above) -->
    opt_seqof(Below),
    seqof(zero),
    opt_seqof(Above),
    !.
match_traverse_derivative(Below, zero, Above) -->
    opt_seqof(Above),
    seqof(zero),
    opt_seqof(Below),
    !.
match_traverse_derivative(Below, _, Above) -->
    seqof(Above),
    seqof(Below),
    !.
match_traverse_derivative(Below, _, Above) -->
    seqof(Below),
    seqof(Above),
    !.

match_derivative(PM) -->
    opt_seqof(zero), seqof(PM),
    !.
match_derivative(PM) -->
    seqof(PM), opt_seqof(zero),
    !.

%!  seqof(+Value)//
%
%   Match a non-empty sequence of Value.

seqof(V) --> [V0], { match(V, V0) }, opt_seqof(V).

%!  opt_seqof(+Value)//
%
%   Match a possibly empty sequence of Value.

opt_seqof(V) --> [V0], { match(V, V0) }, opt_seqof(V).
opt_seqof(_) --> [].

match(V1, V2) :- \+ V1 \= V2, !.
match(point(P1), point(P2)) :- eq_point(P1, P2).

vtime_stamp(_Before, [U1|_], _After, T) =>
    T = U1.t.
vtime_stamp(Before, _, After, DT) =>
    d(T1,D11,_,_) = Before.t,
    d(T2,D12,_,_) = After.t,
    normal_mid(T1, T2, Ti),
    normal_mid(D11, D12, Di),
    DT = d(Ti,Di,_,_).

%!  linked_state(?State, ?To)// is semidet.
%
%   True when State is linked to the Garp states in To.

linked_state(State, To) -->
    [State],
    { To = State.get(garp_states),
      To = [_|_]
    }.

peek_linked_state(State, To) -->
    peek(State),
    { To = State.get(garp_states),
      To = [_|_]
    }.

peek(X), [X] --> [X].

%!  not_linked_states(-States:list)// is multi.
%
%   True when States is a list  of   qualitative  states  that cannot be
%   matched to Garp.

not_linked_states([]) -->
    [].
not_linked_states([H|T]) -->
    not_linked_state(H),
    not_linked_states(T).

not_linked_state(State) -->
    [State],
    { \+ State.get(garp_states) = [_|_] }.

%!  q_series(+Source, -QSeries, +Options) is det.
%
%   True when QSeries is a list of   qualitative states that result from
%   running the model described in Source.
%
%   Options are passed to  nq_series/3.   Additionally,  this  predicate
%   processes
%
%     - model(Model)
%       (Saved) Garp model to compare against.

q_series(Source, QSeries, Options) :-
    option(model(Model), Options, engine),
    id_mapping(Model, IdMapping),
    simulate(Source, Series0,
             [ track(all),
               id_mapping(IdMapping)
             | Options
             ]),
    init_derivatives(Series0, Series, IdMapping),
    nq_series(Series, QSeries, Options).

%!  nq_series(+Series, -QSeries, +Options) is det.
%
%   Map the numeric Series into a qualitative QSeries.  Options:
%
%     - match(Derivatives)
%       Derivatives is a dict `Quantity -> List`, where
%       `List` holds a subset of [0,1,2].
%     - link_garp_states(+Bool)
%       Find related Garp states.
%     - qspaces(Dict)
%       Dict mapping quantities to their qualitative value space

nq_series(Series, QSeries, Options) :-
    deleted_unmatched(Series, Series1, Options),
    add_derivatives(Series1, Series2, Options),
    series_qualitative(Series2, QSeries0, Options),
    simplify_qseries(QSeries0, QSeries2, Options),
    opt_link_garp_states(QSeries2, QSeries, Options).

%!  deleted_unmatched(+AllSeries, -Series, +Options)
%
%   Delete the quantities set to `[]` in the match(Match) option.

deleted_unmatched(Series0, Series, Options) :-
    option(match(Match), Options, #{}),
    findall(K-_, get_dict(K, Match, []), Del),
    Del \== [],
    !,
    dict_pairs(DelDict, _, Del),
    maplist(delete_keys(DelDict), Series0, Series).
deleted_unmatched(Series, Series, _).

delete_keys(Del, Dict0, Dict) :-
    dict_same_keys(Del, Free),
    select_dict(Free, Dict0, Dict).

%!  add_derivatives(+Series, -Series1, +Options) is det.

add_derivatives(Series, Series1, Options) :-
    max_derivative_used(Options, MaxD),
    foreach(between(0, MaxD, D),
            add_derivative(D, Series)),
    (   option(match(Match), Options),
        Match._ \== []
    ->  maplist(strip_derivatives(Match), Series, Series1)
    ;   Series1 = Series
    ).

max_derivative_used(Options, D) :-
    option(match(Match), Options, #{}),
    dict_pairs(Match, _, Pairs),
    pairs_values(Pairs, Values),
    append(Values, AllValues),
    max_list(AllValues, D).

%!  strip_derivatives(+Match, +State0, -State) is det.
%
%   Map unneeded derivatives to a variable.

strip_derivatives(Match, State0, State) :-
    mapdict(strip_derivatives_(Match), State0, State).

strip_derivatives_(Match, K, V0, V) :-
    Keep = Match.get(K),
    !,
    keep_derivatives(Keep, V0, V).
strip_derivatives_(_, _, V, V).

keep_derivatives([0], d(V,_,_,_), R) =>
    R = d(V,_,_,_).
keep_derivatives([0,1], d(V,D1,_,_), R) =>
    R = d(V,D1,_,_).
keep_derivatives([1], d(_,D1,_,_), R) =>
    R = d(_,D1,_,_).
keep_derivatives([0,1,2], d(V,D1,D2,_), R) =>
    R = d(V,D1,D2,_).
keep_derivatives([0,1,2,3], D, R) =>
    R = D.
keep_derivatives(List, D, R) =>
    R = d(_,_,_,_),
    maplist(copy_arg(List), D, R).

copy_arg([], _, _).
copy_arg(A, I, O) :-
    arg(A, I, V),
    arg(A, O, V).

%!  simplify_qseries(+QSeries0, -QSeries, +Options) is det.
%
%   Tasks:
%
%     - Remove subsequent qualitative states that are equal.
%     - Add intermediates if a continuous quantity moves in
%       the quantity space.
%
%   @tbd: swap insert_points/2.   For that we need to keep
%   first and last of an equal series to keep the start and
%   end time.

simplify_qseries(Series0, Series, Options) :-
    (   option(insert_point_traversal(true), Options, true)
    ->  option(qspaces(QSpaces), Options),
        insert_points(Series0, Series1, QSpaces)
    ;   Series1 = Series0
    ),
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
    A =@= B.

%!  insert_points(+SeriesIn, -SeriesOut, +QSpaces) is det.
%
%   Insert intermediate time points if  a   qualitative  value  passes a
%   "point(Value)" between two time points in SeriesIn.

insert_points([], [], _).
insert_points([S1,S2|T0], [S1,Si|T], QSpaces) :-
    insert_point(S1, S2, Si, QSpaces),
    !,
    insert_points([S2|T0], T, QSpaces).
insert_points([S1|T0], [S1|T], QSpaces) :-
    insert_points(T0, T, QSpaces).

%!  insert_point(+S1, +S2, -Si, +QSpaces) is semidet.
%
%   True when Si needs to be inserted between   S1 and S2 because one or
%   more variables crossed a point in the  quantity space. S1 and S2 are
%   qualitative states.

insert_point(S1, S2, Si, QSpaces) :-
    mapdict(insert_value(S2, QSpaces, Done), S1, Si),
    Done == true.

%!  insert_value(+State2, +QSpaces, -Done, +Q, +V1, -Vi) is det.

:- det(insert_value/6).
insert_value(S2, QSpaces, Done, Q, V1, Vi) :-
    get_dict(Q, S2, V2),
    insert_value_(Q, QSpaces, V1, V2, Vi, Done).

insert_value_(t, _, d(T1,D1,_,_), d(T2,D2,_,_), R, _) =>
    R = d(Ti,Di,_,_),
    normal_mid(T1, T2, Ti),
    normal_mid(D1, D2, Di).   % time D is recomputed and may vary a little.
insert_value_(Q, QSpaces, d(V1,D11,D12,D13), d(V2,D21,D22,D23), R, Done) =>
    R = d(Vi,D1i,D2i,D3i),
    insert_value_v(Q, QSpaces, V1, V2, Vi, Done),
    insert_value_d(D11, D21, D1i, Done),
    insert_value_d(D12, D22, D2i, Done),
    insert_value_d(D13, D23, D3i, Done).

insert_value_d(V, V, Vi, _Done) => Vi = V.
insert_value_d(min, plus, Vi, Done) => Vi = zero, Done = true.
insert_value_d(plus, min, Vi, Done) => Vi = zero, Done = true.
insert_value_d(zero, V, Vi, _Done) => Vi = V.
insert_value_d(V, zero, Vi, _Done) => Vi = V.
insert_value_d(Var, _, _, _Done), var(Var) => true.
insert_value_d(_, Var, _, _Done), var(Var) => true.

insert_value_v(_Q, _QSpaces, V, V, Vi, _Done) => Vi = V.
insert_value_v(_Q, _QSpaces, V, point(_), Vi, _Done) => Vi = V.
insert_value_v(_Q, _QSpaces, point(_), V, Vi, _Done) => Vi = V.
insert_value_v(_Q, _QSpaces, Var, _, _, _Done), var(Var) => true.
insert_value_v(_Q, _QSpaces, _, Var, _, _Done), var(Var) => true.
insert_value_v(Q, QSpaces, V1, V2, Vi, Done) =>
    (   Values = QSpaces.get(Q),
        append(_, [V1,point(P),V2|_], Values)
    ->  point_name(P, PN),
        Vi = point(PN),
        Done = true
    ;   Vi = V1
    ).

point_name(N=_V, Name) => Name = N.
point_name(N, Name), atom(N) => Name = N.


		 /*******************************
		 *              CSV		*
		 *******************************/

%!  q_series_table(+Model, +Qseries, -Table, +IdMapping, +Options)
%
%   Translate a qualitative series into CSV format.

q_series_table(Model, QSeries, [Title|Rows], IdMapping, Options) :-
    (   QSeries = [_,Sample|_]
    ->  true
    ;   QSeries = [Sample|_]
    ),
    dict_keys(Sample, Keys0),
    order_keys(Model, IdMapping, Keys0, Keys),
    option(match(Match), Options, #{}),
    phrase(q_title_row(Keys, Sample, IdMapping, Match), TitleCells),
    Title =.. [row|TitleCells],
    maplist(q_sample_row(Keys, Match), QSeries, Rows).

q_sample_row(Keys, Match, QSample, Row) :-
    phrase(q_sample_cols(Keys, QSample, Match), Cells),
    Row =.. [row|Cells].

q_sample_cols([], _, _) -->
    !.
q_sample_cols([H|T], QSample, Match) -->
    { DColumns = Match.get(H,[0,1]) },
    q_sample_cell(QSample.get(H,d(_,_,_,_)), DColumns),
    q_sample_cols(T, QSample, Match).

q_sample_cell(V, DColumns) -->
    { compound(V),
      compound_name_arguments(V, d, Args)
    },
    !,
    q_dcells(Args, 0, DColumns).
q_sample_cell(H, _) -->
    { is_list(H),
      !,
      atomics_to_string(H, ",", V)
    },
    [V].
q_sample_cell(H, _) -->
    { float(H),
      !,
      round_float(4, H, V)
    },
    [V].
q_sample_cell(H, _) -->
    [H].

q_title_row([], _, _, _) --> [].
q_title_row([H|T], First, IdMapping, Match) -->
    { DColumns = Match.get(H,[0,1]) },
    q_title_cell(H, First, IdMapping, DColumns),
    q_title_row(T, First, IdMapping, Match).

q_title_cell(Key, Sample, IdMapping, DColumns) -->
    { compound(Sample.Key),
      compound_name_arguments(Sample.Key, d, Args),
      !,
      key_label(IdMapping, Key, Lbl0)
    },
    derivative_titles(Args, 0, Lbl0, DColumns).
q_title_cell(Key, _Sample, IdMapping, _DColumns) -->
    { key_label(IdMapping, Key, Label)
    },
    [Label].

q_dcells([], _, _) -->
    [].
q_dcells([H|T], N, DColumns) -->
    (   {memberchk(N, DColumns)}
    ->  one(H)
    ;   []
    ),
    {N2 is N+1},
    q_dcells(T, N2, DColumns).

one(C) --> {var(C)}, !, [*].
one(C) --> [C].

derivative_titles([], _, _, _) -->
    [].
derivative_titles([_|T], N, Lbl0, DColumns) -->
    (   { memberchk(N, DColumns) }
    ->  derivative_title(N,Lbl0)
    ;   []
    ),
    {N2 is N+1},
    derivative_titles(T, N2, Lbl0, DColumns).

derivative_title(0, Lbl0) ==>
    [ Lbl0 ].
derivative_title(N, Lbl0) ==>
    { d_label(N, DL),
      format(string(Label), '~w~w', [DL, Lbl0])
    },
    [ Label ].

d_label(1, 'Δ').
d_label(2, 'Δ²').
d_label(3, 'Δ³').

                /*******************************
                *       CORRESPONDENCES        *
                *******************************/

%!  validate_correspondences(+QSeriesIn, -QSeries, +Options) is det.
%
%   Validate  the  correspondence  relations.  If  a  correspondence  is
%   violated add/extend the `errors` property of   the state(s) in which
%   correspondence(s) are violated.

:- det(validate_correspondences/3).
validate_correspondences(QSeriesIn, QSeries, Options) :-
    option(model(ModelID), Options),
    correspondences(ModelID, Correspondences),
    maplist(validate_correspondences_(Correspondences), QSeriesIn, QSeries).

validate_correspondences_(Correspondences, QStateIn, QState) :-
    include(violated_correspondense(QStateIn), Correspondences, Violated),
    (   Violated == []
    ->  QState = QStateIn
    ;   q_add_error(violated_correspondences(Violated), QStateIn, QState)
    ).

violated_correspondense(QState, Corr) :-
    \+ satisfied_correspondence(QState, Corr).

satisfied_correspondence(QState, v_correspondence(Q1,V1,Q2,V2)) =>
    (   QState.get(Q1) == V1
    ->  QState.get(Q2) == V2
    ;   QState.get(Q2) == V2
    ->  QState.get(Q1) == V1
    ;   true
    ).
satisfied_correspondence(_QState, Corr) =>
    print_message(warning, satisfied_correspondence(Corr)).

q_add_error(Error, State0, State), Errors = State0.get(errors) =>
    State = State0.put(errors([Error|Errors])).
q_add_error(Error, State0, State) =>
    State = State0.put(errors([Error])).

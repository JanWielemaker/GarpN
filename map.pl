:- module(map,
          [ id_mapping/1,               % -Mapping:dict
            id_mapping/2,               % +Model, -Mapping
            q_series/3,                 % +Model, -QSeries, +Options
            link_garp_states/3,         % +QSeries0, -QSeries, +Options
            q_series_table/3,           % +QSeries, -Table, +IdMapping
            nq_series/3,                % +Series, -QSeries, +Options
            qstate/4,                   % +Model, ?Id, -Values, +Options
            zero_asymptote/2,           % +Values, +Options
            q_rel/2,                    % +Model, -Rel
            q_input_state/2,            % +Model, -Dict
            q_exogenous/3,              % +Model, ?Quantity, ?Exegenous
            exogenous/1,                % ?Class
            m_qspace/4,                 % +ModelId, ?QspaceId, ?Qspace, ?Values
            qspace_point_value/2        % +Name, -Number
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

:- use_module(gsim).
:- use_module(csv_util).

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
m_qspace(ModelId, QspaceId, Qspace, Values) :-
    dynalearn_model(ModelId, ModelData),
    member(qspace(QspaceId, Qspace, Values, fail), ModelData.qspaces).
:- else.
m_qspace(engine, QspaceId, Qspace, Values) :-
    !,
    engine:qspace(QspaceId, Qspace, Values, fail).
m_qspace(Model, QspaceId, Qspace, Values) :-
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

%!  qstate(+Model, ?Id, -Values, +Options)
%
%   Extract qualitative states from a saved Garp simulation If no model
%   is saved, we use the current model.

:- if(current_prolog_flag(dynalearn, true)).
qstate(ModelId, State, Values, Options) :-
    dynalearn_model(ModelId, ModelData),
    member(qstate(State, Values0), ModelData.results.qstates),
    select_derivatives(Values0, Values, Options).
:- else.
qstate(engine, State, Values, Options) :-
    !,
    qstate(State, Values, Options).
qstate(Model, State, Values, Options) :-
    ensure_loaded_model(Model, qstate/2),
    Model:qstate(State, Values0),
    select_derivatives(Values0, Values, Options).
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

%!  q_input_state(+Model, -Dict)
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
    maplist(key_state_derivative_r(First), Keys, Ders),
    asymptotes(Keys, Ders, Series, Asymptotes, Options).

key_state_derivative_r(State, Key, Der) :-
    key_state_derivative(Key, State, Der).

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
    Time = State.t.

zero_from(Series, Options, asymptote(K,D,How), Nth) :-
    maplist(state_trace_value(K-D, _Empty), Series, Values),
    min_list(Values, Min),
    max_list(Values, Max),
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
%
%   @tbd Deal with quatity spaces. Currently assumes all quantity spaces
%   are {negative, zero, positive},  represented   as  `min`, `zero` and
%   `plus`.

series_qualitative(Series, Qualitative, Options) :-
    stable_from(Series, Asymptotes, Time, Options),
    !,
    option(qspaces(QSpaces), Options),
    stable_min_derivatives(Asymptotes, Asymptotes1),
    maplist(state_qualitative_a(Asymptotes1,Time,QSpaces), Series, Qualitative).
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

%!  state_qualitative_a(+Asymptotes, +Time, +QSpaces, +NDict, -QDict) is
%!                      det.
%
%   Turn the numeric  state  NDict  into   a  qualitative  state  QDict,
%   considering that all quantities in  Asymptotes are considered `zero`
%   after Time.

state_qualitative_a(Asymptotes, Time, QSpaces, Dict, QDict) :-
    (   Dict.t >= Time
    ->  mapdict(to_qualitative_z(Asymptotes, QSpaces), Dict, QDict)
    ;   mapdict(to_qualitative(QSpaces), Dict, QDict)
    ).

state_qualitative(QSpaces, Dict, QDict) :-
    mapdict(to_qualitative(QSpaces), Dict, QDict).

to_qualitative_z(Asymptotes, QSpaces, Q, V0, R) :-
    to_qualitative(QSpaces, Q, V0, R0),
    (   memberchk(asymptote(Q,D,_), Asymptotes)
    ->  to_zero(D, R0, R)
    ;   R = R0
    ).

:- det(to_zero/3).
to_zero(N, V0, R), V0 =.. [d|Args] =>
    length(Keep, N),
    append(Keep, ToZero, Args),
    maplist(to_zero, ToZero, Zero),
    append(Keep, Zero, Args1),
    R =.. [d|Args1].
to_zero(0, _, R) => R = zero.

to_zero(_, zero).

%!  to_qualitative(+QSpaces, +Quantity, +NumIn, -QOut) is det.
%
%   Map a numeric result NumIn for Quantity into a qualitative result
%   QOut.
%
%   @arg NumIn is either a plain number, a term d(V,D1), term d(V,D1,D2)
%   or term d(V,D1,D3).  Each Dn must be mapped to plus/min/zero.  Each
%   V must be mapped to the quantity space.

to_qualitative(_, t, T, R) => R = T.
to_qualitative(QSpaces, Q, d(V,D1), R) =>
    R = d(QV,QD1),
    to_qualitative_v(QSpaces, Q, V, QV),
    to_qualitative_d(D1, QD1).
to_qualitative(QSpaces, Q, d(V,D1,D2), R) =>
    R = d(QV,QD1,QD2),
    to_qualitative_v(QSpaces, Q, V, QV),
    to_qualitative_d(D1, QD1),
    to_qualitative_d(D2, QD2).
to_qualitative(QSpaces, Q, d(V,D1,D2,D3), R) =>
    R = d(QV,QD1,QD2,QD3),
    to_qualitative_v(QSpaces, Q, V, QV),
    to_qualitative_d(D1, QD1),
    to_qualitative_d(D2, QD2),
    to_qualitative_d(D3, QD3).
to_qualitative(QSpaces, Q, V, QV) =>
    to_qualitative_v(QSpaces, Q, V, QV).

to_qualitative_d(V, _), \+ normal_number(V) => true.
to_qualitative_d(V, D), V > 0   => D = plus.
to_qualitative_d(V, D), V < 0   => D = min.
to_qualitative_d(V, D), V =:= 0 => D = zero.

to_qualitative_v(_QSpaces, _, V, _), \+ normal_number(V) => true.
to_qualitative_v(QSpaces, Q, V, VQ), QSpace = QSpaces.get(Q) =>
    n_to_qualitative(QSpace, Q, V, VQ).
to_qualitative_v(_, _, V, VQ) =>
    to_qualitative_d(V, VQ).

%!  n_to_qualitative(+QSpace, +Quantity, +Num, -QValue) is det.
%
%   Map Num into a qualitative name from QSpace.
%
%   @arg QValue is one  of  point(Name),  Name   or  a  variable  if the
%   quantity space is undefined (`[interval]`).

n_to_qualitative([interval], _Q, _V, _VQ) => true.
n_to_qualitative([Name,point(P)|_], Q, V, VQ),
    qspace_point_value(Q, P, PV),
    V =< PV =>
    (   V < PV
    ->  VQ = Name
    ;   VQ = point(P)
    ).
n_to_qualitative([point(P)|_], Q, V, VQ),
    qspace_point_value(Q, P, PV),
    V =< PV =>
    (   V < PV
    ->  VQ = error
    ;   VQ = point(P)
    ).
n_to_qualitative(List, Q, V, VQ),
    append(_, [point(P), Name], List),
    qspace_point_value(Q, P, PV),
    V >= PV =>
    (   V > PV
    ->  VQ = Name
    ;   VQ = point(P)
    ).
n_to_qualitative(List, Q, V, VQ),
    append(_, [point(P)], List),
    qspace_point_value(Q, P, PV),
    V >= PV =>
    (   V > PV
    ->  VQ = error
    ;   VQ = point(P)
    ).
n_to_qualitative(List, Q, V, VQ) =>
    append(_, [point(P1),Name,point(P2)|_], List),
    qspace_point_value(Q, P1, N1),
    qspace_point_value(Q, P2, N2),
    (   V =:= N1
    ->  VQ = point(P1)
    ;   V =:= N2
    ->  VQ = point(P2)
    ;   V > N1, V < N2
    ->  VQ = Name
    ),
    !.

%!  qspace_point_value(+Quantity, +Name, -Number) is det.

% TBD: Get from configuration
qspace_point_value(_Quantity, Name, Value),
    qspace_point_value(Name, Value0) =>
    Value = Value0.

%!  qspace_point_value(+Name, -Number) is semidet.
%
%   Well known quantity space point values.

qspace_point_value(zero, V) => V = 0.0.
qspace_point_value(N, V), number(N) => V = N.
qspace_point_value(A, V), atom_number(A,N) => V = N.

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
%     - match(Derivatives)
%       Derivatives is a dict `Quantity -> List`, where
%       `List` holds a subset of [0,1,2].
%     - link_garp_states(+Bool)
%       Find related Garp states.
%     - qspaces(Dict)
%       Dict mapping quantities to their qualitative value space

nq_series(Series, QSeries, Options) :-
    deleted_unmatched(Series, Series1, Options),
    add_derivatives(Series1, SeriesD, Options),
    series_qualitative(SeriesD, QSeries0, Options),
    simplify_qseries(QSeries0, QSeries1, Options),
    opt_link_garp_states(QSeries1, QSeries, Options).

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

%!  add_derivatives(+Series0, -Series, +Options) is det.

add_derivatives(Series0, Series, Options) :-
    max_derivative_used(Options, MaxD),
    add_derivatives_(MaxD, Series0, Series1),
    (   option(match(Match), Options),
        Match._ \== []
    ->  maplist(strip_derivatives(Match), Series1, Series)
    ;   Series = Series1
    ).

max_derivative_used(Options, D) :-
    option(match(Match), Options, #{}),
    dict_pairs(Match, _, Pairs),
    pairs_values(Pairs, Values),
    append(Values, AllValues),
    max_list(AllValues, D).

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

keep_derivatives([0], D, V) =>
    arg(1, D, V).
keep_derivatives([0,1], D, R) =>
    R = d(V,D1),
    arg(1, D, V),
    arg(2, D, D1).
keep_derivatives([1], D, R) =>
    R = d(_,D1),
    arg(2, D, D1).
keep_derivatives([0,1,2], D, R) =>
    R = d(V,D1,D2),
    arg(1, D, V),
    arg(2, D, D1),
    arg(3, D, D2).
keep_derivatives([0,1,2,3], D, R) =>
    R = d(V,D1,D2,D3),
    arg(1, D, V),
    arg(2, D, D1),
    arg(3, D, D2),
    arg(4, D, D3).
keep_derivatives(List, D, R) =>
    max_list(List, Max),
    Arity is Max+1,
    functor(R, d, Arity),
    copy_derivatives(List, D, R).

copy_derivatives([], _, _).
copy_derivatives([H|T], D, R) :-
    A is H+1,
    arg(A, D, V),
    arg(A, R, V),
    copy_derivatives(T, D, R).

%!  simplify_qseries(+QSeries0, -QSeries, +Options) is det.
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

simplify_qseries(Series0, Series, Options) :-
    option(qspaces(QSpaces), Options),
    insert_points(Series0, Series1, QSpaces),
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

%!  insert_value(+State2, QSpaces, -Done, +Q, +V1, -Vi) is det.

:- det(insert_value/6).
insert_value(S2, _, _, t, V1, Vi) :-
    !,
    get_dict(t, S2, V2),
    Vi is (V1+V2)/2.
insert_value(S2, QSpaces, Done, Q, V1, Vi) :-
    get_dict(Q, S2, V2),
    insert_value_(Q, QSpaces, V1, V2, Vi, Done).

insert_value_(Q, QSpaces, d(V1,D11,D12,D13), d(V2,D21,D22,D23), R, Done) =>
    R = d(Vi,D1i,D2i,D3i),
    insert_value_v(Q, QSpaces, V1, V2, Vi, Done),
    insert_value_d(D11, D21, D1i, Done),
    insert_value_d(D12, D22, D2i, Done),
    insert_value_d(D13, D23, D3i, Done).
insert_value_(Q, QSpaces, d(V1,D11,D12), d(V2,D21,D22), R, Done) =>
    R = d(Vi, D1i, D2i),
    insert_value_v(Q, QSpaces, V1, V2, Vi, Done),
    insert_value_d(D11, D21, D1i, Done),
    insert_value_d(D12, D22, D2i, Done).
insert_value_(Q, QSpaces, d(V1,D11), d(V2,D21), R, Done) =>
    R = d(Vi, D1i),
    insert_value_v(Q, QSpaces, V1, V2, Vi, Done),
    insert_value_d(D11, D21, D1i, Done).
insert_value_(Q, QSpaces, V1, V2, Vi, Done) =>
    insert_value_v(Q, QSpaces, V1, V2, Vi, Done).

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
    ->  Vi = point(P),
        Done = true
    ;   Vi = V1
    ).

		 /*******************************
		 *              CSV		*
		 *******************************/

%!  q_series_table(+Qseries, -Table, +IdMapping)
%
%   Translate a qualitative series into CSV format.

q_series_table(QSeries, [Title|Rows], IdMapping) :-
    QSeries = [First|_],
    dict_keys(First, Keys0),
    order_keys(IdMapping, Keys0, Keys),
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

one(C) --> {var(C)}, !, [*].
one(C) --> [C].

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

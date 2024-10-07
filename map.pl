:- module(map,
          [ id_mapping/1                % -Mapping:dict
          ]).
:- use_module(gsim).

/** <module> Map qualitative and quantitative (simulation) model
*/

%!  id_mapping(-Mapping:dict) is det.
%
%   Compute the mapping of  Garp  quantity   names  to  number_of(Q)  or
%   growth(Q) terms.

id_mapping(Mapping) :-
    findall(Id-Term, id_map(Id, Term), Pairs),
    dict_pairs(Mapping, _, Pairs).

id_map(Id, Term) :-
    engine:qspace(Id, Term4, _Values, fail),
    Term4 =.. [DV,Q|_],
    Term =.. [DV,Q].

state(Id, Values) :-
    engine:state(Id, SMD),
    smd_data(values, SMD, ParValues),
    maplist(dict_val, ParValues, Pairs),
    dict_pairs(Values, _, Pairs).

dict_val(value(Id,V0,D10,D20), Id-d(V,D1,D2)) :-
    unknown_var(V0, V),
    unknown_var(D10, D1),
    unknown_var(D20, D2).

unknown_var(unk, _) => true.
unknown_var(plus, Val) => Val = plus.
unknown_var(zero, Val) => Val = zero.
unknown_var(min,  Val) => Val = min.

smd_data(params, smd(_Name, _SE, parameters(P), _V, _R, _SS, _IS), Result) => Result = P.
smd_data(values, smd(_Name, _SE, _P, par_values(V), _R, _SS, _IS), Result) => Result = V.
smd_data(store,  smd(_Name, _SE, _P, _V, _R, _SS, store(IS)),      Result) => Result = store(IS).

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
to_qualitative(t, T, R) => R = T.
to_qualitative(_, V, _), var(V)  => true.
to_qualitative(_, V, D), V > 0   => D = plus.
to_qualitative(_, V, D), V < 0   => D = min.
to_qualitative(_, V, D), V =:= 0 => D = zero.

q_series(QSeries, Options) :-
    simulate(Series, Options),
    add_derivative(Series, SeriesD1),
    add_derivative(SeriesD1, SeriesD2),
    series_qualitative(SeriesD2, QSeries0),
    simplify_qseries(QSeries0, QSeries).

%!  simplify_qseries(+QSeries0, -QSeries) is det.
%
%   Tasks:
%
%     - Remove subsequent qualitative states that are equal.
%     - Add intermediates if a continuous quantity moves in
%       the quantity space.

simplify_qseries(Series0, Series) :-
    removes_equal_sequences(Series0, Series1),
    insert_points(Series1, Series).

removes_equal_sequences([], T) => T = [].
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
insert_value_(Var, _, _, _Done), var(Var) => true.
insert_value_(_, Var, _, _Done), var(Var) => true.

simulate(Series, Options) :-
    id_mapping(Mapping),
    simulate(file('predator.pl'), Series,
             [ track(all),
               id_mapping(Mapping)
             | Options
             ]).


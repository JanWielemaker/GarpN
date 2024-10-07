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
    series_qualitative(SeriesD2, QSeries).

simulate(Series, Options) :-
    id_mapping(Mapping),
    simulate(file('predator.pl'), Series,
             [ track(all),
               id_mapping(Mapping)
             | Options
             ]).


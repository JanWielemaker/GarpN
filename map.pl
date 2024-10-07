:- module(map,
          [ id_mapping/1                % -Mapping:dict
          ]).
:- use_module(gsim).

/** <module> Map qualitative and quantitative (simulation) model
*/

id_map(Id, Term) :-
    engine:qspace(Id, Term4, _Values, fail),
    Term4 =.. [DV,Q|_],
    Term =.. [DV,Q].

%!  id_mapping(-Mapping:dict) is det.
%
%   Compute the mapping of  Garp  quantity   names  to  number_of(Q)  or
%   growth(Q) terms.

id_mapping(Mapping) :-
    findall(Id-Term, id_map(Id, Term), Pairs),
    dict_pairs(Mapping, _, Pairs).


simulate(Series, Options) :-
    id_mapping(Mapping),
    simulate(file('predator.pl'), Series,
             [ track(all),
               id_mapping(Mapping)
             | Options
             ]).

:- module(dynalearn,
          []).
:- use_module(library(uri)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).
:- use_module(library(apply)).
:- use_module(library(dicts)).

dynalearn_url('https://api.dynalearn.nl/garpN/').

%!  dynalearn_models(-Models)

dynalearn_models(Models) :-
    dynalearn_url(Base),
    uri_edit([path(models)], Base, URL),
    http_get(URL, Models,
             [ value_string_as(atom),
               json_object(dict)
             ]).

dynalearn_model(Id, Simulation) :-
    get_model(Id, Model0),
    import_simulation(Model0, Simulation).

:- table
    get_model/2.

get_model(Id, Model) :-
    dynalearn_url(Base),
    format(string(Path), 'model/~a', [Id]),
    uri_edit([path(Path)], Base, URL),
    http_get(URL, Model,
             [ value_string_as(atom),
               json_object(dict)
             ]).

import_simulation(DL, #{qstates: Qstates, qstate_graph:Edges}) :-
    dict_pairs(DL.simulation.states, _, Pairs),
    pairs_values(Pairs, DLStates),
    maplist(import_qstate(DL.mapping), DLStates, Qstates, Edges).

import_qstate(Mapping, DLState, qstate(Num, QValues), qstate_from(Num, From)) :-
    atom_number(DLState.num, Num),
    import_qvalues(Mapping, DLState.values,     Values0),
    import_qvalues(Mapping, DLState.dvalues,  D1Values0),
    import_qvalues(Mapping, DLState.d2values, D2Values0),
    import_qvalues(Mapping, DLState.d3values, D3Values0),
    dicts_to_same_keys([Values0, D1Values0, D2Values0, D3Values0],
                       unbound,
                       [Values,  D1Values,  D2Values,  D3Values]),
    mapdict(join_ds(D1Values,  D2Values,  D3Values), Values, QValues),
    maplist(atom_number, DLState.from, From).

unbound(_, _, _).

import_qvalues(Mapping, DLValues, Values) :-
    dict_pairs(DLValues, _, DLPairs),
    maplist(map_kv(Mapping), DLPairs, Pairs),
    dict_pairs(Values, _, Pairs).

map_kv(Mapping, DLK-DLV, K-V) :-
    K = Mapping.get(DLK,DLK),
    V = Mapping.get(DLV,DLV).

join_ds(D1Values, D2Values, D3Values, Key, V, d(V,D1,D2,D3)) :-
    D1 = D1Values.Key,
    D2 = D2Values.Key,
    D3 = D3Values.Key.

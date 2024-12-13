:- module(dynalearn,
          [ dynalearn_models/1,         % -Models
            dynalearn_model/2           % ++Id, -Model
          ]).
:- use_module(library(uri)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json), []). % plugin
:- use_module(library(apply)).
:- use_module(library(dicts)).
:- use_module(library(pairs)).
:- use_module(library(terms)).
:- use_module(library(lists)).
:- use_module(library(occurs)).

:- use_module(map).

dynalearn_url('https://api.dynalearn.nl/garpN/').

%!  dynalearn_models(-Models)

dynalearn_models(Models) :-
    dynalearn_url(Base),
    uri_edit([path(models)], Base, URL),
    http_get(URL, Models,
             [ value_string_as(atom),
               json_object(dict)
             ]).

%!  dynalearn_model(++Id, -Model) is det.

dynalearn_model(Id, #{ results: Simulation,
                       prolog:Terms,
                       id_mapping:IdMapping,
                       qspaces:QSpaces,
                       input_state:InputState,
                       exogenous:Exogenous,
                       qrels:QRels
                     }) :-
    get_model(Id, Model0),
    prolog_model(Model0, Terms, IdMapping),
    import_qrels(Terms, QRels),
    import_qspaces(Terms, QSpaces),
    import_input_state(Terms, InputState),
    import_exogenous(Terms, Exogenous),
    import_simulation(Model0, Simulation).

%!  get_model(++Id, -Model) is det.
%
%   Fetch the model with Id from DynaLearn

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

%!  prolog_model(+DLModel, -Terms) is det

prolog_model(DLModel, Terms, IdMapping) :-
    read_string_to_terms(DLModel.prolog, Terms0),
    foldsubterms(unmap(DLModel.mapping), Terms0, Terms, [], QPairs0),
    sort(QPairs0, QPairs),
    dict_pairs(IdMapping, #, QPairs).

unmap(Mapping, parameters(Parms0), Result, Q0, Q) =>
    Result = parameters(Parms),
    foldl(unmap_parameter(Mapping), Parms0, Parms, Q0, Q).
unmap(Mapping, par_values(Values0), Result, Q0, Q) =>
    Result = par_values(Values),
    Q = Q0,
    maplist(unmap_value(Mapping), Values0, Values).
unmap(_Mapping, par_relations(Rels), Result, Q0, Q) =>
    Result = par_relations(Rels),
    Q = Q0.
unmap(Mapping, Id, UnMapped, Q0, Q),
    atom(Id),
    UnMapped = Mapping.get(Id) =>
    Q = Q0.
unmap(_, _, _, _, _) =>
    fail.

unmap_parameter(Mapping, Term0, Term, Q0, [Id-Q|Q0]) :-
    Term0 =.. [Id, Ent, Attr, U, QSpace], atom(Attr),
    AttrName = Mapping.get(Attr,Attr),
    EntName = Mapping.get(Ent,Ent),
    Term  =.. [AttrName, EntName, Id, U, QSpace],
    Q =.. [AttrName, EntName].

unmap_value(Mapping, value(Id, A, V0, B), value(Id, A, V, B)) :-
    V = Mapping.get(V0, V0).

read_string_to_terms(String, Terms) :-
    setup_call_cleanup(
        open_string(String, In),
        read_stream_to_terms(In, Terms, []),
        close(In)).

read_stream_to_terms(In, Terms, Options) :-
    read_term(In, T0, [variable_names(Bindings)|Options]),
    maplist(bind_nvar, Bindings),
    read_stream_to_terms_(T0, In, Terms, Options).

read_stream_to_terms_(end_of_file, _, [], _) :-
    !.
read_stream_to_terms_(T0, In, [T0|Terms], Options) :-
    read_term(In, T1, [variable_names(Bindings)|Options]),
    maplist(bind_nvar, Bindings),
    read_stream_to_terms_(T1, In, Terms, Options).

bind_nvar(Name=Var) :-
    string_concat('N', Num, Name),
    atom_number(Num, _),
    !,
    atom_concat(n, Num, Var).
bind_nvar(_).

%!  import_qrels(+Terms, -QRels) is det.
%
%   Import the qualitative relations

:- det(import_qrels/2).
import_qrels(Terms, QRels) :-
    findall(QRelSet,
            (   sub_term(Sub, Terms),
                par_relations(Sub, QRelSet)
            ),
            QRelSets),
    append(QRelSets, QRels0),
    list_to_set(QRels0, QRels).

par_relations(par_relations(List), QRels), is_list(List) =>
    QRels = List.
par_relations(_, _) =>
    fail.

%!  import_qspaces(+Terms, -QSpaces) is det.
%
%   Create list of terms
%
%       qspace(ParamId, Attr(Ent,ParamId,?,?), Spaces, fail)
%
%   @tbd It seems we lost  the  Garp   3th  and  second  argument, which
%   provide properties (continuous) and the name of the quantity space.

:- det(import_qspaces/2).
import_qspaces(Terms, QSpaces) :-
    smd(Terms, SMD),
    arg(3, SMD, parameters(Parms)),
    include(is_qspace, Terms, QSpaceIn),
    foldl(import_qspace(QSpaceIn), Parms, QSpaces, 1, _).

smd(Terms, SMD) :-
    SMD = smd(_In,_SE,_Parms,_ParVals,_ParRels,_SysStructs),
    memberchk(SMD, Terms).

is_qspace(quantity_space(_Id, _A, _QSpaces)) => true.
is_qspace(_) => fail.

import_qspace(QSpaceIn, Param, Result, N, N1) :-
    Result = qspace(Id, Term, QSpaces, fail),
    N1 is N+1,
    arg(4, Param, QId),
    functor(Param, Attr, 4),
    arg(1, Param, Ent),
    arg(2, Param, Id),
    memberchk(quantity_space(QId, _A, QSpaces), QSpaceIn),
    Term =.. [Attr,Ent,_,_].

%!  import_input_state(+Terms, -InputState) is det
%
%

:- det(import_input_state/2).
import_input_state(Terms, InputState) :-
    smd(Terms, SMD),
    arg(4, SMD, par_values(Values)),
    maplist(value_pair, Values, Pairs),
    dict_pairs(InputState, _, Pairs).

value_pair(value(Q, _, Value, _), Q-Value).

%!  import_exogenous(+Terms, -Exogenous) is det.
%
%   @arg Exogenous is a list of exogenous(Qid, Class)

import_exogenous(Terms, Exogenous) :-
    findall(exogenous(Qid, Class),
            find_exogenous(Terms, Qid, Class), Exogenous).

find_exogenous(Terms, Qid, Class) :-
    member(system_structures(_,_,conditions(Conds),givens(Givens)), Terms),
    member(system_elements(SEList), Conds),
    exogeneous(SEList, Ent, Class),
    memberchk(parameters(Params), Givens),
    member(Param, Params),
    functor(Param, Qid, 4),
    arg(1, Param, Ent).

exogeneous(SEList, Ent, Class) :-
    numbervars(SEList, 0, _),
    member(has_attribute(Ent, has_assumption, X), SEList),
    is_instance(X, SEList, Class, [X]),
    exogenous(Class).

is_instance(X, _, X, _).
is_instance(X, SEList, Class, Done) :-
    member(instance(X, Super), SEList),
    \+ memberchk(Super, Done),
    is_instance(Super, SEList, Class, [Super|Done]).


%!  import_simulation(+DLModel, -Results)
%
%   Import the qualitative values and  state   graph  from the DynaLearn
%   output.

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

map_kv(Mapping, DLK-DLV, DLK-V) :-
    V = Mapping.get(DLV,DLV).

join_ds(D1Values, D2Values, D3Values, Key, V, d(V,D1,D2,D3)) :-
    D1 = D1Values.Key,
    D2 = D2Values.Key,
    D3 = D3Values.Key.

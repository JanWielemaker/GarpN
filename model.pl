:- module(model,
          [ init_model/2                % +Model, -Equations
          ]).
:- use_module(library(terms)).
:- use_module(library(lists)).

:- use_module(map).
:- use_module(gsim).

/** <module> Propose a (partial) numeric model from Garp
 *
*/

%!  init_model(+Model, -Equations)

init_model(Model, Equations) :-
    findall(QRel, q_rel(Model, QRel), QRels),
    qrel2nrel(QRels, NRels),
    default_nrels(DefNRels),
    append(NRels, DefNRels, Eql0),
    id_mapping(Model, Mapping),
    foldsubterms(id_to_term(Mapping), Eql0, Eql1, [], ConstEql),
    append(Eql1, ConstEql, Equations1),
    add_model_init(Equations1, Equations).

qrel2nrel(QRels, NRels) :-
    findall(t(NRels,Left,NLeft),
            qrel2nrel(QRels, Left, NRels, NLeft),
            Tuples),
    sort(3, =<, Tuples, [t(NRels,Left,NLeft)|_]),
    maplist(rel_unknown, Left).

%!  qrel2nrel(+QRels, -Left, -NRels, -NLeft) is multi.
%
%   True when NRels are the  numeric   relations  for QRels. Enumeration
%   stops if we find an element that   leaves  nothing unmapped. Is this
%   ok? I.e., can we have multiple meaningful mappings?
%
%   @arg Left are unmapped QRels elements.
%   @arg NLeft is the length of Left.

qrel2nrel(QRels, Left, NRels, NLeft) :-
    qrel2nrel(QRels, Left, NRels),
    length(Left, NLeft),
    (   NLeft =:= 0
    ->  !
    ;   true
    ).

qrel2nrel(QRels, Left, NRels) :-
    qrel_nrel(Q, N),
    select_graph(Q, QRels, QRels1),
    append(N, NRels1, NRels),
    qrel2nrel(QRels1, Left, NRels1).
qrel2nrel(Left, Left, []).

%!  qrel_nrel(+QRel:list, -NRel:list) is multi.
%
%   Map  a  set  of  qualitative  relations  to  a  set  of  qualitative
%   relations. The rules are ordered to map larger submodels first.
%
%   @tbd include quantity spaces into the picture

qrel_nrel([ equal(min(InflA,InflB), Dep),
            prop_pos(Dep,InflA),
            prop_neg(Dep,InflB)
          ],
          [ Dep := InflA-InflB
          ]).
qrel_nrel([ equal(min(InflA,InflB), Dep),
            prop_pos(Dep,InflB),
            prop_neg(Dep,InflA)
          ],
          [ Dep := InflB-InflA
          ]).
qrel_nrel([ prop_pos(Dep,Infl),
            dir_q_correspondence(Dep,Infl)
          ],
          [ Dep := Infl/c
          ]).
qrel_nrel([inf_pos_by(I,D)], [I := I + D*'Δt']).
qrel_nrel([prop_pos(Dep,Infl)], [Dep := c*Infl]).

default_nrels([ t := t + 'Δt',
                t := 0,
                'Δt' := placeholder(constant,0.1)
              ]).

id_to_term(_Mapping, c, Term, S0, S) =>
    length(S0, N0),
    N is N0+1,
    atom_concat(c,N,Term),
    S = [(Term := placeholder(constant,_))|S0].
id_to_term(Mapping, Id, Term, S0, S), atom(Id) =>
    S = S0,
    Term = Mapping.get(Id,Id).
id_to_term(_Mapping, _Id, _Term, _S0, _S) =>
    fail.

% add_model_init(Eq, Eq) :- !.
add_model_init(Eq0, Eq) :-
    catch(read_model(terms(Eq0), _Formulas, _Constants, _State, []),
          error(existence_error(initial_values, Unresolved)),
          true),
    (   var(Unresolved)
    ->  Eq = Eq0
    ;   maplist(init_quantity, Unresolved, Init),
        append(Eq0, Init, Eq)
    ).

init_quantity(Q, (Q := placeholder(init,_))).

select_graph([], Set, Set).
select_graph([H|T], Set0, Set) :-
    select(H, Set0, Set1),
    select_graph(T, Set1, Set).

rel_unknown(Rel) :-
    print_message(warning, unknown_relation(Rel)),
    fail.

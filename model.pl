:- module(model,
          [ init_model/2                % +Model, -Equations
          ]).
:- use_module(map).
:- use_module(library(terms)).
:- use_module(library(lists)).

/** <module> Propose a (partial) numeric model from Garp
*/

%!  init_model(+Model, -Equations)

init_model(Model, Equations) :-
    findall(Eq, propose_equation(Model, Eq), Eql0),
    id_mapping(Model, Mapping),
    foldsubterms(id_to_term(Mapping), Eql0, Eql1, [], ConstEql),
    append(Eql1, ConstEql, Equations).

propose_equation(Model, Equation) :-
    q_rel(Model, Rel),
    rel_equation(Rel, Equation).
propose_equation(_, t := t + 'Δt').
propose_equation(_, t := 0).
propose_equation(_, 'Δt' := placeholder(dt,0.1)).

rel_equation(equal(min(A,B), C), Eq) =>
    Eq = (C := A-B).                    % Constants?
rel_equation(prop_pos(A,B), Eq) =>
    Eq = (B := c*A).
rel_equation(prop_neg(A,B), Eq) =>
    Eq = (B := -c*A).
rel_equation(inf_pos_by(I,D), Eq) =>
    Eq = (I := I + D*'Δt').
rel_equation(Rel, _Eq) =>
    rel_unknown(Rel).

id_to_term(_Mapping, c, Term, S0, S) =>
    length(S0, N0),
    N is N0+1,
    atom_concat(c,N,Term),
    S = [(Term := placeholder(Term,_))|S0].
id_to_term(Mapping, Id, Term, S0, S), atom(Id) =>
    S = S0,
    Term = Mapping.get(Id,Id).
id_to_term(_Mapping, _Id, _Term, _S0, _S) =>
    fail.

rel_unknown(Rel) :-
    print_message(warning, unknown_relation(Rel)),
    fail.



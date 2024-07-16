:- module(gsim,
          [ read_model/3,
            run/3
          ]).

/** <module> Numerical simulation

@tbd Formula representation?
*/

run(File, Series, Options) :-
    option(steps(Count), Options, 100),
    read_model(File, Formulas, State),
    steps(Count, Formulas, State, Series).

read_model(From, Formulas, State) :-
    read_file_to_terms(From, Terms, [module(gsim)]),
    foldl(model_expression, Terms, m(f{}, s{}), m(Formulas, State)).

model_expression(Term, m(FormulasIn, StateIn),  m(Formulas, State)) :-
    model_expression(Term, FormulasIn, Formulas, StateIn, State).

model_expression(Left := Right, FormulasIn, Formulas, StateIn, State),
    number(Right) =>
    Formulas = FormulasIn,
    to_id(Left, Id),
    State = StateIn.put(Id, Right).
model_expression(Formula, FormulasIn, Formulas, StateIn, State) =>
    State = StateIn,
    mapsubterms(to_id, Formula, Interned),
    Interned = (Left := Right),
    Formulas = FormulasIn.put(Left, Right).

to_id(Term, Id), atom(Term) => Id = Term.
to_id(number_of(X), Id), atom(X) => atom_concat(number_of_, X, Id).
to_id(growth(X), Id), atom(X) => atom_concat(growth_, X, Id).
to_id(_, _) => fail.

steps(0, _, State, [State]) :-
    !.
steps(N, Formulas, State, [State|Series]) :-
    must_be(positive_integer, N),
    step(Formulas, State, State1),
    N1 is N - 1,
    steps(N1, Formulas, State1, Series).

step(F, S0, S) :-
    dict_pairs(F, _, Pairs),
    foldl(eval, Pairs, S0, S).

eval(Left-Right, S0, S) :-
    eval_formula(Right, S0, Value),
    S = S0.put(Left, Value).

eval_formula(Right, S0, Value) :-
    mapsubterms(value_from(S0), Right, Expr),
    Value is Expr.

value_from(S, Id, Value) :-
    atom(Id),
    Value = S.Id.


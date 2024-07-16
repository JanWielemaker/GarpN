:- module(gsim,
          [ read_model/3,
            run/3
          ]).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(option)).
:- use_module(library(readutil)).
:- use_module(library(terms)).

/** <module> Numerical simulation

@tbd Formula representation?
*/

%!  run(+Model, -Series, +Options) is det.
%
%   Compile and run Model, producing Series as a list of states.
%
%   Options:
%
%     - iterations(+Count)
%       Number of iterations.  Default is 100.
%     - sample(+Size)
%       Add the state after each Size iterations to Series. Default is
%       1 (all).

run(From, Series, Options) :-
    option(iterations(Count), Options, 100),
    option(sample(Sample), Options, 1),
    read_model(From, Formulas, State),
    steps(0, Count, Sample, Formulas, State, Series).

read_model(From, Formulas, State) :-
    read_to_terms(From, Terms),
    foldl(model_expression, Terms, m(f{}, s{}), m(Formulas, State)).

read_to_terms(file(File), Terms) :-
    read_file_to_terms(File, Terms, [module(gsim)]).
read_to_terms(string(String), Terms) :-
    setup_call_cleanup(
        open_string(String, In),
        read_stream_to_terms(In, Terms, [module(gsim)]),
        close(In)).

read_stream_to_terms(In, Terms, Options) :-
    read_term(In, T0, Options),
    read_stream_to_terms_(T0, In, Terms, Options).

read_stream_to_terms_(end_of_file, _, [], _) :-
    !.
read_stream_to_terms_(T0, In, [T0|Terms], Options) :-
    read_term(In, T1, Options),
    read_stream_to_terms_(T1, In, Terms, Options).

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

steps(I, N, Sample, Formulas, State, Series) :-
    I < N,
    !,
    (   (   I mod Sample =:= 0
        ;   I =:= N-1
        )
    ->  Series = [State|SeriesT]
    ;   SeriesT = Series
    ),
    must_be(positive_integer, N),
    step(Formulas, State, State1),
    I1 is I+1,
    steps(I1, N, Sample, Formulas, State1, SeriesT).
steps(_, _, _, _, _, []).

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


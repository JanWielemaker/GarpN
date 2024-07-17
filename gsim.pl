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
%     - method(+Method)
%       Approximation method.  One of `rk4` or 'euler` (default)

run(From, Series, Options) :-
    option(iterations(Count), Options, 100),
    option(sample(Sample), Options, 1),
    option(method(Method), Options, euler),
    must_be(positive_integer, Count),
    must_be(positive_integer, Sample),
    read_model(From, Formulas, State),
    steps(0, Count, Method, Sample, Formulas, State, Series).

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

steps(I, N, Method, Sample, Formulas, State, Series) :-
    I < N,
    !,
    (   (   I mod Sample =:= 0
        ;   I =:= N-1
        )
    ->  Series = [State|SeriesT]
    ;   SeriesT = Series
    ),
    step(Method, Formulas, State, State1),
    I1 is I+1,
    steps(I1, N, Method, Sample, Formulas, State1, SeriesT).
steps(_, _, _, _, _, _, []).

step(Method, F, S0, S) :-
    dict_pairs(F, _, Pairs),
    foldl(eval(Method), Pairs, S0, S).

%!  eval(+Method, +Pair, +S0, -S) is det.
%
%   @see https://en.wikipedia.org/wiki/Runge%E2%80%93Kutta_methods

eval(euler, Left-Right, S0, S) =>
    eval_formula(Right, S0, Value),
    S = S0.put(Left, Value).
eval(rk4, Left-Right, S0, S) =>
    get_dict(Left, S0, Y),
    get_dict(δt, S0, H),
    get_dict(t, S0, T),
    eval_formula(Right, S0, K1),
    T2 is T+H/2,
    Y2 is Y+H*K1/2,
    eval_formula_(Right, S0, [t-T2, Left-Y2], K2),
    Y3 is Y+H*K2/2,
    eval_formula_(Right, S0, [t-T2, Left-Y3], K3),
    T4 = T+H,
    Y4 is Y+H*K3,
    eval_formula_(Right, S0, [t-T4, Left-Y4], K4),
    Value is Y+(H/6)*(K1 + 2*K2 + 2*K3 + K4),
    S = S0.put(Left, Value).

eval_formula(Right, S0, Value) :-
    mapsubterms(value_from(S0), Right, Expr),
    Value is Expr.

eval_formula_(Right, S0, Mod, Value) :-
    foldl(mod, Mod, S0, S1),
    eval_formula(Right, S1, Value).

mod(Key-Value, S0, S) :-
    put_dict(Key, S0, Value, S).

value_from(S, Id, Value) :-
    atom(Id),
    Value = S.Id.


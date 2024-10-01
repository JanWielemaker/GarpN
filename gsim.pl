:- module(gsim,
          [ read_model/3,
            run/3
          ]).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(option)).
:- use_module(library(readutil)).
:- use_module(library(terms)).
:- use_module(library(lists)).

/** <module> Numerical simulation

Formulas are represented as

    Quantity := Expression.

Where Quantity is a ground Prolog term.

Time is represented as

    t := t + δt.
    t := 0.
    δt := 0.1.

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
%
%   @arg Model is either term file(FileName) or term string(Text), where
%   the content must be a valid set of Prolog terms.
%   @arg Series is a list of dicts, each dict mapping quatities to their
%   current value. The first dict _only_ contains the keys that belong
%   to quantities that have an initial value.

run(From, Series, Options) :-
    option(iterations(Count), Options, 100),
    option(sample(Sample), Options, 1),
    option(method(Method), Options, euler),
    must_be(positive_integer, Count),
    must_be(positive_integer, Sample),
    read_model(From, Formulas, State),
    steps(0, Count, Method, Sample, Formulas, State, Series).

read_model(From, Formulas, State) :-
    read_to_terms(From, Terms0),
    maplist(quantity, Terms0, Quantities0),
    sort(Quantities0, Sorted),
    maplist(q_term, Sorted, Quantities1),
    maplist(intern_model_term(Quantities1), Terms0, Terms1),
    maplist(is_valid_model_term, Terms1),
    foldl(model_expression, Terms1, m(f{}, s{}), m(Formulas, State)).

%!  read_to_terms(++Input, -Terms) is det.
%
%   Read the input into a list of Prolog terms.

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

%!  quantity(++ModelTerm, -Quantity) is det.

quantity(Q := _Expr, Out), ground(Q) =>
    Out = Q.
quantity(Invalid, _) =>
    type_error(model_term, Invalid).

q_term(Q, q(Q,Id,_)) :-
    to_id(Q, Id).

to_id(Term, Id) :-
    format(atom(Id), '~q', Term).

%!  intern_model_term(+Quantities, +TermIn, -TermOutBindings)
%
%   @arg Quantities is a list q(Q,Id,Var)
%   @arg TermIn is a model term in grounded notation
%   @arg TermOutBindings is a term `TermOut-Dict`, Where
%        TermOut is the original formula with all
%        quantities replaced by variables and Dict maps
%        quantity-ids to the variable.

intern_model_term(Quantities, Term0, Term-Bindings) :-
    foldsubterms(intern(Quantities), Term0, Term, [], Pairs0),
    sort(1, @<, Pairs0, Pairs),
    dict_pairs(Bindings, #, Pairs).

intern(Quantities, Q, V, B0, B), ground(Q), memberchk(q(Q,Id,Var),Quantities) =>
    V = Var,
    B = [Id-V|B0].
intern(_, _, _, _, _) =>
    fail.

%!  is_valid_model_term(+TermAndBindings) is det.
%
%   Validate the model term, raising an exception on errors.

is_valid_model_term((Left:=Right)-_Bindings), var(Left) =>
    is_valid_model_term_(Right).

is_valid_model_term_(Term), compound(Term), current_arithmetic_function(Term) =>
    compound_name_arguments(Term, _, Args),
    maplist(is_valid_model_term_, Args).
is_valid_model_term_(Term), atom(Term), current_arithmetic_function(Term) =>
    true.                                         % i.e., `pi`, `e`
is_valid_model_term_(Term), number(Term) =>
    true.
is_valid_model_term_(Term), var(Term) =>
    true.
is_valid_model_term_(_Term) =>
    fail.

%!  model_expression(+TermAndBindings, +Model0, -Model1)

model_expression(Term, m(FormulasIn, StateIn),  m(Formulas, State)) :-
    model_expression(Term, FormulasIn, Formulas, StateIn, State).

model_expression((Left := Right)-Bindings, FormulasIn, Formulas, StateIn, State),
    dict_pairs(Bindings, #, [Id-Left]) =>
    Formulas = FormulasIn,
    Value is Right,
    State = StateIn.put(Id, Value).
model_expression((Left := Right)-Bindings, FormulasIn, Formulas, StateIn, State) =>
    State = StateIn,
    dict_pairs(Bindings, #, Pairs),
    member(Id-Var, Pairs),
    Var == Left,
    !,
    Formulas = FormulasIn.put(Id, formula(Right, Bindings)).

%!  steps(+I, +N, +Method, +Sample, +Formulas, +State, -Series) is det.

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


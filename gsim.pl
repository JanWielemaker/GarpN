:- module(gsim,
          [ read_model/4,                         % +Source, -Formulas, -Constants, -State0
            run/3,                                % +Model, -Series, +Options
            add_derivative/2                      % +Series, -DSeries
          ]).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(option)).
:- use_module(library(readutil)).
:- use_module(library(terms)).
:- use_module(library(lists)).
:- use_module(library(debug)).

/** <module> Numerical simulation

Formulas are represented as

    Quantity := Expression.

Where Quantity is  a  ground  Prolog   term.  The  system  rewrites  all
`Expression`  instances  by  replacing  quantities    found   by  Prolog
variables.  The  resulting  Expression  is   verified  to  only  contain
functions known to Prolog.

Time is represented as below. Note that `δt`  is just an atom to Prolog,
so `dt` also works.  The variable name `t` however is reserved.

    t := t + δt.
    t := 0.
    δt := 0.1.
*/

%!  run(+Model, -Series, +Options) is det.
%
%   Compile and run Model, producing Series as a list of states.
%
%   Options:
%
%     - iterations(+Count)
%       Number of iterations.  Default is 1000.
%     - sample(+Size)
%       Add the state after each Size iterations to Series. Default is
%       1 (all).
%     - method(+Method)
%       Approximation method.  One of `rk4` or 'euler` (default)
%     - constants(-Constants)
%       Unify Constants with a dict `Id` -> `Value`.  Note that the
%       discovered initial state is the first element of Series.
%
%   @arg Model is either term file(FileName) or term string(Text), where
%   the content must be a valid set of Prolog terms.
%   @arg Series is a list of dicts, each dict mapping quatities to their
%   current value. The first dict _only_ contains the keys that belong
%   to quantities that have an initial value.

run(From, Series, Options) :-
    option(iterations(Count), Options, 1000),
    option(sample(Sample), Options, 1),
    option(method(Method), Options, euler),
    option(constants(Constants), Options, _),
    must_be(positive_integer, Count),
    must_be(positive_integer, Sample),
    read_model(From, Formulas, Constants, State),
    intern_constants(Constants, Formulas, Formulas1),
    steps(0, Count, Method, Sample, Formulas1, State, Series).

%!  read_model(+Source, -Formulas, -Constants, -State0) is det.
%
%   @arg Formulas is a dict `QuantityId` -> formula(Expression,
%   Bindings), where Bindings is a dict `QuantityId` -> `Var`.
%   @arg Constants is a dict `ConstantId` -> `Value`.
%   @arg State0 is a dict `QuantityId` -> `Value`.

read_model(From, Formulas, Constants, State) :-
    read_to_terms(From, Terms0),
    maplist(quantity, Terms0, Quantities0),
    sort(Quantities0, Sorted),
    maplist(q_term, Sorted, Quantities1),
    maplist(intern_model_term(Quantities1), Terms0, Terms1),
    maplist(is_valid_model_term, Terms1),
    foldl(model_expression, Terms1, m(f{}, i{}), m(Formulas, Init)),
    split_init(Init, Formulas, Constants, State).

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
    dict_pairs(Bindings, _, Pairs).

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

model_expression(Term, m(FormulasIn, InitIn),  m(Formulas, Init)) :-
    model_expression(Term, FormulasIn, Formulas, InitIn, Init).

model_expression((Left := Right)-Bindings,
                 FormulasIn, Formulas, InitIn, Init),
    dict_pairs(Bindings, _, [Id-Left]) =>
    Formulas = FormulasIn,
    Value is Right,
    Init = InitIn.put(Id, Value).
model_expression((Left := Right)-Bindings,
                 FormulasIn, Formulas, InitIn, Init) =>
    InitIn = Init,
    dict_pairs(Bindings, _, Pairs),
    select(Id-Var, Pairs, Pairs1),
    Var == Left,
    !,
    (   same_variables(Right, Pairs)
    ->  RightBindings = Bindings
    ;   assertion(same_variables(Right, Pairs1)),
        dict_pairs(RightBindings, _, Pairs1)
    ),
    Formulas = FormulasIn.put(Id, formula(Right, RightBindings)).

same_variables(T1, T2) :-
    term_variables(T1, V1), sort(V1, Vs1),
    term_variables(T2, V2), sort(V2, Vs2),
    Vs1 == Vs2.

%!  split_init(+Init, +Formulas, -Constants, -State) is det.
%
%   Split the quatities for which we found  an initial value into a dict
%   with constants and the initial  state  dict.   Both  map  an id to a
%   concrete number.

split_init(Init, Formulas, Constants, State) :-
    dict_pairs(Init, _, Pairs),
    split_init_(Pairs, Formulas, ConstantPairs, StatePairs),
    dict_pairs(Constants, c, ConstantPairs),
    dict_pairs(State, s, StatePairs).

split_init_([], _, [], []).
split_init_([Id-Value|T], Formulas, ConstantPairs, [Id-Value|StatePairs]) :-
    get_dict(Id, Formulas, _),
    !,
    split_init_(T, Formulas, ConstantPairs, StatePairs).
split_init_([Id-Value|T], Formulas, [Id-Value|ConstantPairs], StatePairs) :-
    split_init_(T, Formulas, ConstantPairs, StatePairs).

%!  intern_constants(+Constants, +FormualsIn, -Formuals) is det.
%
%   Simplify FormualsIn by binding the constants  and removing them from
%   the bindings term of the formulas.

intern_constants(Constants, Formulas0, Formulas1) :-
    dict_pairs(Formulas0, f, Pairs0),
    maplist(intern_constants_(Constants), Pairs0, Pairs1),
    dict_pairs(Formulas1, f, Pairs1).

intern_constants_(Constants,
                  Id-formula(Expr, Bindings),
                  Id-formula(Expr, Bindings1)) :-
    Bindings >:< Constants,
    dict_pairs(Bindings, _, Pairs),
    exclude(instantiated_binding, Pairs, Pairs1),
    dict_pairs(Bindings1, _, Pairs1).

instantiated_binding(_-I) => nonvar(I).

%!  steps(+I, +N, +Method, +Sample, +Formulas, +State, -Series) is det.
%
%   Run the actual simulation.
%
%   @arg I numbers the simulation steps (0..)
%   @arg N ends the simulation
%   @arg Method is one of `euler` or `rk4`
%   @arg Sample indicates that we create the output Series
%   by sampling every N iterations.
%   @arg Formulas is a dict `Id` -> formula(Expr,Bindings)
%   @arg State is a dict `Id` -> `Value`
%   @arg Series is a list of dicts with the same state as
%        `State` but different values.

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

step(Method, Formulas, S0, S) :-
    dict_pairs(Formulas, _, Pairs),
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

eval_formula(Formula, S0, Value) :-
    copy_term(Formula, formula(Expr, Bindings)),
    Bindings :< S0,
    Value is Expr.

eval_formula_(Right, S0, Mod, Value) :-
    foldl(mod, Mod, S0, S1),
    eval_formula(Right, S1, Value).

mod(Key-Value, S0, S) :-
    put_dict(Key, S0, Value, S).

%!  add_derivative(+Series, -DSeries) is det.
%
%   Add the derivatives to a Series by replacing the value with a term
%   d(V,D1,...).

add_derivative([], []).
add_derivative(L, D) :-
    L = [H|_],
    nth_derivative(H, N),
    derivative(L, N, D).

derivative([H1,H2|T0], N, [DH|T]) :-
    !,
    derivative_1(H1, H2, N, DH),
    derivative([H2|T0], N, T).
derivative([_], _, []).


nth_derivative(S, N), get_dict(_, S, T) =>
    nth_derivative_(T, N).

nth_derivative_(d(_,_), D)  => D = 1.
nth_derivative_(d(_,_,_), D) => D = 2.
nth_derivative_(V, D), number(V) => D = 0.

derivative_1(D1, D2, N, D) :-
    dict_pairs(D1, T, P1),
    maplist(derivative_v(N, D2), P1, P),
    dict_pairs(D, T, P).

derivative_v(2, Dict, K-d(V,D1), K-R) =>
    R = K-d(V,D1,D2),
    get_dict(K, Dict, d(_,D1b)),
    D2 is D1b-D1.
derivative_v(1, Dict, K-d(V,D1,D2), R) =>
    R = K-d(V,D1,D2,D3),
    get_dict(K, Dict, d(_,_,D2b)),
    D3 is D2b-D2.
derivative_v(0, Dict, K-V, R) =>
    R = K-d(V,D1),
    get_dict(K, Dict, Vb),
    D1 is Vb-V.

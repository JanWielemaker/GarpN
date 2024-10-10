:- module(gsim,
          [ read_model/5,         % +Source, -Formulas, -Constants, -State0, +Opts
            simulate/3,           % +ModelSrc, -Series, +Options
            add_derivative/2      % +Series, -DSeries
          ]).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(option)).
:- use_module(library(readutil)).
:- use_module(library(terms)).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(dicts)).

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

%!  simulate(+Model, -Series, +Options) is det.
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
%     - track(+What)
%       Determines what is tracked in the Series.  Default are all
%       quantities that have an initial and a formula.  Alternative
%       is currently `all` to track all quantities that have a formula.
%     - id_mapping(+Dict)
%       Maps integer ids to terms number_of(Q) or growth(Q).
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

simulate(From, Series, Options) :-
    option(iterations(Count), Options, 1000),
    option(sample(Sample), Options, 1),
    option(method(Method), Options, euler),
    option(constants(Constants), Options, _),
    must_be(positive_integer, Count),
    must_be(positive_integer, Sample),
    read_model(From, Formulas, Constants, State0, Options),
    add_tracking(Formulas, State0, State, Options),
    intern_constants(Constants, DTExpr, Formulas, Formulas1),
    method_params(Method, DTExpr, Constants, Formulas1, Formulas2, MethodP),
    steps(0, Count, MethodP, Sample, Formulas2, State, Series).

%!  read_model(+Source, -Formulas, -Constants, -State0, +Options) is det.
%
%   @arg Formulas is a dict `QuantityId` -> formula(Expression,
%   Bindings), where Bindings is a dict `QuantityId` -> `Var`.
%   @arg Constants is a dict `ConstantId` -> `Value`.
%   @arg State0 is a dict `QuantityId` -> `Value`.

read_model(From, Formulas, Constants, State, Options) :-
    read_to_terms(From, Terms0),
    maplist(quantity, Terms0, Quantities0),
    sort(Quantities0, Sorted),
    maplist(q_term(Options), Sorted, Quantities1),
    maplist(intern_model_term(Quantities1), Terms0, Terms1),
    maplist(is_valid_model_term, Terms1),
    foldl(model_expression, Terms1, m(f{}, i{}), m(Formulas, Init)),
    split_init(Init, Formulas, Constants, State).

%!  read_to_terms(++Input, -Terms) is det.
%
%   Read the input into a list of Prolog terms.

read_to_terms(file(File), Terms) =>
    read_file_to_terms(File, Terms, [module(gsim)]).
read_to_terms(string(String), Terms) =>
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

q_term(Options, Q, q(Q,Id,_)) :-
    option(id_mapping(Mapping), Options),
    get_dict(Id, Mapping, Q),
    !.
q_term(_Options, Q, q(Q,Id,_)) :-
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
%   Split the quantities for which we found an initial value into a dict
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

%!  intern_constants(+Constants, -DTExpr, +FormualsIn, -Formuals) is det.
%
%   Simplify FormualsIn by binding the constants  and removing them from
%   the bindings term of the formulas.

intern_constants(Constants0, DTExpr, Formulas0, Formulas1) :-
    dt_expression(Formulas0, DTExpr),
    dict_keys(DTExpr, [DTName]),
    del_dict(DTName, Constants0, _, Constants),
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

%!  dt_expression(+Formulas, -DTExpr:dict) is det.
%
%   Find the ``t := t +  δt``  formula.   This  gives  us  the time step
%   variable name and allows binding  it  if   it  is  a constant as for
%   _Euler_.
%
%   @arg DTExpr is a dict _{DTName: DTVar}.

:- det(dt_expression/2).
dt_expression(Formulas, DTExpr) :-
    formula(T+DTVar, Dict) = Formulas.t,
    assertion(T == Dict.t),
    DTVar == Dict.DTName,
    !,
    dict_pairs(DTExpr, _, [DTName-DTVar]).

%!  differential_formulas(+DTName, +Formulas, -DFormulas) is det.
%
%   Translate formulas of  the  for  ``X  :=   X  +  Y*δt``  into  their
%   differential form, i.e., simply ``Y``. Also  removes the formula for
%   the time step. This is a more   suitable form for advanced numerical
%   approximation methods like RK4.
%
%   @tbd Deal with ``X := X + Y*δt - Z*δt````

:- det(differential_formulas/3).
differential_formulas(DTName, Formulas, DFormulas) :-
    dict_pairs(Formulas, _, Pairs),
    selectchk(t-_, Pairs, Pairs1),
    maplist(differential_formula(DTName), Pairs1, DPairs),
    dict_pairs(DFormulas, d, DPairs).

differential_formula(DTName, Id-formula(Expr, Bindings),
                             Id-d_formula(DExpr,DBindings)) :-
    del_dict(Id, Bindings, IdV, Bindings1),
    del_dict(DTName, Bindings1, DTV, DBindings),
    $,
    diff_formula(IdV, DTV, Expr, DExpr).
differential_formula(_, Pair, Pair).

diff_formula(IdV, DTV, IdV + D*DTV, DExpr) => DExpr = D.
diff_formula(IdV, DTV, IdV + DTV*D, DExpr) => DExpr = D.
diff_formula(IdV, DTV, D*DTV + IdV, DExpr) => DExpr = D.
diff_formula(IdV, DTV, DTV*D + IdV, DExpr) => DExpr = D.
diff_formula(IdV, DTV, IdV - D*DTV, DExpr) => DExpr = -D.
diff_formula(IdV, DTV, IdV - DTV*D, DExpr) => DExpr = -D.
diff_formula(IdV, DTV, D*DTV - IdV, DExpr) => DExpr = -D.
diff_formula(IdV, DTV, DTV*D - IdV, DExpr) => DExpr = -D.

%!  method_params(+Method, +DTExpr, +Constants,
%!                +FormulasIn, -FormulasOut, -MethodOut)

method_params(euler, DTExpr, Constants, Formulas, Formulas, euler) :-
    DTExpr :< Constants.
method_params(rk4, DTExpr, Constants, Formulas, DFormulas,
              rk4(DTName, DT)) :-
    $dict_keys(DTExpr, [DTName]),
%   differential_formulas(DTName, Formulas, DFormulas),
    del_dict(t, Formulas, _, DFormulas),
    dict_pairs(DTExpr, _, [DTName-_DTVar]),
    DT = Constants.DTName.

%!  add_tracking(+Formulas:dict, +State0, -State, +Options) is det.
%
%   When track(all) is given, track the values for all formulas

add_tracking(Formulas, State0, State, Options) :-
    option(track(all), Options),
    !,
    dict_keys(Formulas, FKeys),
    maplist(unknown, FKeys, Pairs),
    dict_pairs(FState, _, Pairs),
    State = FState.put(State0).
add_tracking(_, State, State, _).

unknown(Key, Key-_).


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

step(rk4(DT,H), Formulas, Ys, Y) =>
    del_dict(t, Ys, T0, Y0),
    H2 is H/2,
    T2 is T0+H/2,
    Te is T0+H,
    eval_d(Formulas, T0, DT, H, Y0, K1),
    slope(Y0, K1, H2, Y1),                         % Y1 is Y0+K1*H/2
    eval_d(Formulas, T2, DT, H, Y1, K2),
    slope(Y0, K2, H2, Y2),
    eval_d(Formulas, T2, DT, H, Y2, K3),
    slope(Y0, K3, H, Y3),
    eval_d(Formulas, Te, DT, H, Y3, K4),
    sum_dict_list([K1,2*K2,2*K3,K4], K),
    H6 is H/6,
    slope(Y0, K, H6, Yt),
    Y = Yt.put(t, Te).
step(euler, Formulas, S0, S) =>
    euler_step(Formulas, S0, S).

euler_step(Formulas, S0, S) :-
    dict_pairs(Formulas, _, Pairs),
    foldl(eval, Pairs, S0, S).

%!  eval(+Pair, +S0, -S) is det.
%
%   @see https://en.wikipedia.org/wiki/Runge%E2%80%93Kutta_methods

eval(Left-Right, S0, S) =>
    eval_formula(Right, S0, Value),
    S = S0.put(Left, Value).

eval_formula(Formula, S0, Value) :-
    copy_term(Formula, formula(Expr, Bindings)),
    Bindings >:< S0,
    Value is Expr.

%!  eval_d(+Formulas, +T, +DT, +H, +Y0, -K) is det.
%
%   K is the derivative of Formulas at T and Y0.
%
%   @arg DT is the name of the variable holding the _delta T_
%   @arg H is the interval we use.  This is irrelevant as we
%   divide by it again.

:- det(eval_d/6).
eval_d(Formulas, T, DT, H, Y0, K) :-
    dict_pairs(Extra, _, [DT-H,t-T]),
    Y1 = Y0.put(Extra),
    euler_step(Formulas, Y1, Y2),
    derivative_(Y0,Y2,H,K0),
    select_dict(Extra, K0, K).

derivative_(Y0, Y1, H, K) :-
    dict_pairs(Y0, _, P0),
    dict_pairs(Y1, _, P1),
    maplist(derivative__(H), P0, P1, KP),
    dict_pairs(K, _, KP).

derivative__(H, P-Y0, P-Y1, P-K) :-
    K is (Y1-Y0)/H.

%!  slope(+Y0, +K, +H, -Y) is det.

:- det(slope/4).
slope(Y0, K, H, Y1) :-
    dict_pairs(Y0, Tag, Pairs),
    maplist(slope_(K, H), Pairs, Pairs1),
    dict_pairs(Y1, Tag, Pairs1).

slope_(K, H, P-V0, P-V) :-
    V is V0+K*H.

%!  sum_dict_list(+Dicts, -Dict) is det.

:- det(sum_dict_list/2).
sum_dict_list([D1,D2|T], Dict) =>
    sum_dict(D1, D2, S1),
    sum_dict_list([S1|T], Dict).
sum_dict_list([D], Dict) =>
    Dict = D.

sum_dict(D1, N2*D2, D) =>
    dict_pairs(D1, _, P1),
    dict_pairs(D2, _, P2),
    maplist(sum_(N2), P1, P2, P),
    dict_pairs(D, _, P).
sum_dict(D1, D2, D) =>
    dict_pairs(D1, _, P1),
    dict_pairs(D2, _, P2),
    maplist(sum_, P1, P2, P),
    dict_pairs(D, _, P).

sum_(P-V1, P-V2, P-V) =>
    V is V1+V2.
sum_(N, P-V1, P-V2, P-V) =>
    V is V1+N*V2.


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


nth_derivative(S, N), get_dict(K, S, T), K \== t =>
    nth_derivative_(T, N).

nth_derivative_(d(_,_), D)  => D = 1.
nth_derivative_(d(_,_,_), D) => D = 2.
nth_derivative_(V, D), number(V) => D = 0.

derivative_1(D1, D2, N, D) :-
    dict_pairs(D1, T, P1),
    maplist(derivative_v(N, D2), P1, P),
    dict_pairs(D, T, P).

derivative_v(_, _, t-T, R) =>
    R = t-T.
derivative_v(1, Dict, K-d(V,D1), R) =>
    R = K-d(V,D1,D2),
    get_dict(K, Dict, d(_,D1b)),
    v_minus(D1b, D1, D2).
derivative_v(2, Dict, K-d(V,D1,D2), R) =>
    R = K-d(V,D1,D2,D3),
    get_dict(K, Dict, d(_,_,D2b)),
    v_minus(D2b, D2, D3).
derivative_v(0, Dict, K-V, R) =>
    R = K-d(V,D1),
    get_dict(K, Dict, Vb),
    v_minus(Vb, V, D1).

v_minus(V1, V2, D), nonvar(V1), nonvar(V2) => D is V1-V2.
v_minus(_, _, _) => true.

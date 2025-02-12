:- module(gsim,
          [ read_model/5,          % +Source, -Formulas, -Constants, -State0,
                                   % +Opts
            order_formulas/2,      % +Formulas, -Ordering
            simulate/3,            % +ModelSrc, -Series, +Options
            init_derivatives/3,    % +Series, -DSeries, +IdMapping
            add_derivative/2,      % +Nth, +Series
            read_model_to_terms/2, % ++Input, -Terms
            normal_number/1,       % @Term
            min_list_normal/2,     % +List, -Min
            max_list_normal/2,     % +List, -Max
            normal_mid/3           % +N1, +N2, -Ni
          ]).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(option)).
:- use_module(library(readutil)).
:- use_module(library(terms)).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(dicts)).
:- use_module(library(apply_macros), []).
:- use_module(library(listing), [portray_clause/2]).
:- use_module(library(prolog_code), [comma_list/2]).
:- use_module(library(dcg/high_order), [sequence/4]).
:- use_module(library(ordsets), [ord_intersection/3]).
:- use_module(library(pairs), [map_list_to_pairs/3]).
:- use_module(library(ugraphs),
              [ugraph_layers/2, vertices_edges_to_ugraph/3, del_edges/3]).

:- use_module(model).
:- use_module(identifiers).

:- set_prolog_flag(optimise, true).

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
    add_tracking(Formulas, Constants, State0, State, [method(Method)|Options]),
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
    read_model_to_terms(From, Terms0),
    partition(is_qspace, Terms0, QSpaces, Terms1),
    option(source_qspaces(QSpaces), Options, _),
    add_derivative_equations(Terms1, Terms2),
    equation_quantities(Terms2, Quantities0),
    maplist(q_term(Options), Quantities0, Quantities1), % q(Term,Id,Var)
    maplist(intern_model_term(Quantities1), Terms2, Terms3),
    validate_model(Terms3, Options),
    foldl(model_expression, Terms3, m(f{}, i{}), m(Formulas0, Init)),
    split_init(Init, Formulas0, Constants0, State0),
    derived_constants(Formulas0, Constants0, Formulas, Constants),
    derived_initial_state(Formulas, Constants, State0, State, Options).

is_qspace(qspace(_Q,_Values)) => true.
is_qspace(_) => fail.

%!  read_model_to_terms(++Input, -Terms) is det.
%
%   Read the input into a list of Prolog terms.

read_model_to_terms(terms(Terms0), Terms) =>
    Terms = Terms0.
read_model_to_terms(file(File), Terms) =>
    read_file_to_terms(File, Terms, [module(gsim)]).
read_model_to_terms(string(String), Terms) =>
    setup_call_cleanup(
        open_string(String, In),
        read_stream_to_terms(In, Terms, [module(gsim)]),
        close(In)).
read_model_to_terms(String, Terms), string(String) =>
    read_model_to_terms(string(String), Terms).

read_stream_to_terms(In, Terms, Options) :-
    read_term(In, T0, Options),
    read_stream_to_terms_(T0, In, Terms, Options).

read_stream_to_terms_(end_of_file, _, [], _) :-
    !.
read_stream_to_terms_(T0, In, [T0|Terms], Options) :-
    read_term(In, T1, Options),
    read_stream_to_terms_(T1, In, Terms, Options).

%!  add_derivative_equations(Equations0, Equations) is det.

add_derivative_equations(Equations0, Equations) :-
    derivative_equations(Equations0, DEquations),
    append(Equations0, DEquations, Equations).

%!  derivative_equations(+Equations, -DEquations) is det.
%
%   If there are formulas that use the   derivative  of some quantity as
%   inputs while only the quantity itself exists, add an equation
%
%       'ΔQ' := δ(Q).

derivative_equations(Equations, DEquations) :-
    equation_quantities(Equations, Quantities),
    findall(DQ, dependent_derivative(Equations, DQ), DQs0),
    sort(DQs0, DQs),
    ord_subtract(DQs, Quantities, Required),
    maplist(d_equation, Required, DEquations).

dependent_derivative(Equations, DQ) :-
    member(_Q := Expr, Equations),
    sub_term(DQ, Expr),
    is_derivative_term(DQ).

d_equation(DQ, DQ := δ(Q)) :-
    term_derivative(Q, DQ).

%!  equation_quantities(+Equations, -Quantities) is det.

equation_quantities(Terms1, Quantities) :-
    maplist(quantity, Terms1, Quantities0),     % Quantities are the
    sort(Quantities0, Quantities).		% left side of equations

%!  quantity(++ModelTerm, -Quantity) is det.

quantity(Q := _Expr, Out), ground(Q) =>
    Out = Q.
quantity(Invalid, _) =>
    type_error(model_term, Invalid).

%!  q_term(+Options, ?QTerm, ?QData) is det.

q_term(Options, QTerm, q(QTerm,QId,_)) :-
    option(id_mapping(IdMapping), Options, #{}),
    term_key(QTerm, QId, IdMapping).

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

%!  validate_model(+Equations, +Options) is det.
%
%   Validate that all equations have an evaluable right side.
%
%   @throws model_error(invalid(Invalid)) if there are invalid
%   parts in the equations. Invalid is a list of invalid terms.

validate_model(Terms, Options) :-
    maplist(invalid_model_term, Terms, InvalidL),
    append(InvalidL, Invalid0),
    strip_placeholders(Invalid0, Invalid, Options),
    (   Invalid == []
    ->  true
    ;   throw(model_error(invalid(Invalid)))
    ).

strip_placeholders(Invalid0, Invalid, Options),
    option(allow_placeholders(true), Options) =>
    exclude(is_placeholder, Invalid0, Invalid).
strip_placeholders(Invalid0, Invalid, _Options) =>
    Invalid = Invalid0.

%!  invalid_model_term(+TermAndBindings, -Invalid:list) is det.
%
%   True when Invalid are the  invalid  parts   of  the  model. That is,
%   Prolog terms that are not evaluable.

invalid_model_term((Left:=Right)-Bindings, Missing), var(Left) =>
    phrase(invalid_model_term_(Bindings, Right), Missing).

invalid_model_term_(Bindings, Term),
    compound(Term), supported_function(Term) ==>
    { compound_name_arguments(Term, _, Args) },
    sequence(invalid_model_term_(Bindings), Args).
invalid_model_term_(_Bindings, Term),
    atom(Term), supported_function(Term) ==>
    [].                                         % i.e., `pi`, `e`
invalid_model_term_(_Bindings, Term),
    number(Term) ==>
    [].
invalid_model_term_(Bindings, Term),
    var(Term),
    get_dict(_Q, Bindings, Var),
    Term == Var ==>                               % interned
    [].
invalid_model_term_(Bindings, placeholder(_Name, Value)),
    phrase(invalid_model_term_(Bindings, Value), []) ==>
    [].
invalid_model_term_(_Bindings, Term) ==>
    [Term].

%!  supported_function(@Term) is semidet.
%
%   True if Term is a function supported by NGarp.

supported_function(Term), current_arithmetic_function(Term) => true.
supported_function(δ(_)) => true.
supported_function(_) => fail.

is_delta_function(δ(_)) => true.
is_delta_function(_) => fail.

%!  model_expression(+TermAndBindings, +Model0, -Model1) is det.
%
%   Split the input into formulas and constants.

model_expression(Term, m(FormulasIn, InitIn),  m(Formulas, Init)) :-
    model_expression(Term, FormulasIn, Formulas, InitIn, Init).

model_expression((Left := Right)-Bindings,
                 FormulasIn, Formulas, InitIn, Init),
    dict_pairs(Bindings, _, [Id-Left]) =>         % Left only binding
    Formulas = FormulasIn,
    (   is_placeholder(Right)
    ->  Value = Right
    ;   ground(Right)
    ->  Value is Right
    ;   Value = Right                             % only for incomplete formulas
    ),
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
    dict_pairs(Constants, _, ConstantPairs),
    dict_pairs(State, _, StatePairs).

split_init_([], _, [], []).
split_init_([Id-Value|T], Formulas, ConstantPairs, [Id-Value|StatePairs]) :-
    get_dict(Id, Formulas, _),
    !,
    split_init_(T, Formulas, ConstantPairs, StatePairs).
split_init_([Id-Value|T], Formulas, [Id-Value|ConstantPairs], StatePairs) :-
    split_init_(T, Formulas, ConstantPairs, StatePairs).

%!  derived_constants(+Formulas0, +Constants0, -Formulas, -Constants) is
%!                    det.
%
%   If a formula just has constants at the  right, remove it and add the
%   value to Constants.

derived_constants(Formulas0, Constants0, Formulas, Constants) :-
    dict_pairs(Formulas0, FTag, FPairs0),
    derived_constants_(FPairs0, Constants0, FPairs, Constants),
    dict_pairs(Formulas, FTag, FPairs).

derived_constants_(FPairs0, Constants0, FPairs, Constants) :-
    derived_constants__(FPairs0, Constants0, FPairs1, Constants1),
    (   Constants0 == Constants1
    ->  FPairs = FPairs0,
        Constants = Constants0
    ;   derived_constants_(FPairs1, Constants1, FPairs, Constants)
    ).

derived_constants__([], Constants, [], Constants).
derived_constants__([Left-formula(Right, Bindings)|FT], Constants0,
                    FPairs, Constants) :-
    Bindings >:< Constants0,
    ground(Right), !,
    Value is Right,
    Constants1 = Constants0.put(Left, Value),
    derived_constants__(FT, Constants1, FPairs, Constants).
derived_constants__([FH|FT], Constants0, [FH|FPairs], Constants) :-
    derived_constants__(FT, Constants0, FPairs, Constants).

%!  derived_initial_state(+Formulas, +Constants, +State0, -State,
%!                        +Options) is det.
%
%   Extend the initial state. We do so  by evaluating formulas for which
%   the right side is known and add these  to the state. This process is
%   repeated until no formula can be filled.  _dt_ is set to 0.
%
%   @throw model_error(no_initial_values(Terms)) for the variables that
%   require an initial value that is not given.

derived_initial_state(Formulas, Constants, State0, State, Options) :-
    select_option(allow_placeholders(true), Options, Options1),
    !,
    mapsubterms(bind_placeholder,
                t(Formulas,Constants,State0),
                t(Formulas1,Constants1,State1)),
    derived_initial_state(Formulas1, Constants1, State1, State, Options1).
derived_initial_state(Formulas, Constants, State0, State, Options) :-
    findall(Key, missing_init(Formulas, Constants, State0, Key), Missing0),
    sort(Missing0, Missing),
    dt_expression(Formulas, DTExpr0),
    copy_term(DTExpr0, DTExpr),
    0 = DTExpr.DTKey,
    (   del_dict(DTKey, Constants, _, Constants1)
    ->  true
    ;   Constants1 = Constants
    ),
    derived_initials(Missing, Unresolved, Formulas, DTExpr, Constants1,
                     State0, State),
    (   Unresolved == []
    ->  true
    ;   formulas_needs_init(Formulas, NeedsInit),
        ord_intersection(Unresolved, NeedsInit, Init),
        (   Init == []
        ->  true
        ;   maplist(q_term_id(Options), InitTerms, Init),
            throw(model_error(no_initial_values(InitTerms)))
        )
    ).

bind_placeholder(placeholder(_Id, PValue), Value) :-
    (   nonvar(PValue)
    ->  Value = PValue
    ;   Value = 1
    ).

%!  derived_initials(+Missing, -Unresolved:ordset, +Formulas, +DTExpr,
%!                   +Constants, +State0, -State) is det.
%
%   Remove missing values that can be   computed  from other fully known
%   values.

derived_initials([], [], _, _, _, State, State) :-
    !.
derived_initials(Missing, Unres, Formulas, DTExpr, Constants, State0, State) :-
    select(Key, Missing, Missing1),
    copy_term(Formulas.get(Key), formula(Right,Bindings)),
    Constants >:< Bindings,
    State0 >:< Bindings,
    DTExpr >:< Bindings,
    ground(Right),
    \+ is_placeholder(Right),
    \+ is_delta_function(Right),
    !,
    Value is Right,
    State1 = State0.put(Key,Value),
    derived_initials(Missing1, Unres, Formulas, DTExpr, Constants,
                     State1, State).
derived_initials(Missing, Unresolved, _, _, _, State, State) :-
    sort(Missing, Unresolved).

%!  missing_init(+Formulas, +Constants, +State, -Key) is nondet.
%
%   True when Key appears at the right side of an equation and refers to
%   a variable for which there is no   constant and that does not appear
%   in State.

missing_init(Formulas, Constants, State, Key) :-
    get_dict(_Left, Formulas, formula(Right, Bindings)),
    \+ is_delta_function(Right),
    Bindings >:< Constants,
    Bindings >:< State,
    term_variables(Right, Vars),
    Vars \== [],
    get_dict(Key, Bindings, Var),
    member(Var2, Vars),
    Var2 == Var.

q_term_id(Options, Term, Id) :-
    q_term(Options, Term, q(Term,Id,_)).

%!  formulas_needs_init(+Formulas, -NeedsInit:ordset) is det.
%
%   Find the variables that require to be initialized.  That is
%    - Any variable that depends on itself (i.e., integrations)
%    - Any _source_ of the dependency graph.

formulas_needs_init(Formulas, NeedsInit) :-
    formulas_partial_odering(Formulas, Layers, SelfLoops),
    Layers = [First|_],
    append(First, SelfLoops, NeedsInit0),
    sort(NeedsInit0, NeedsInit).

%!  formulas_partial_odering(+Formulas, -Layers, -SelfLoops) is det.
%
%   Create  a  partial  ordering  of  the    formulas   based  on  their
%   dependencies.
%
%   @arg Formulas is a dict Var:formula(Prolog,Bindings)
%   @arg Layers is a list of lists of variables representing a partial
%   ordering in the dependencies.
%   @arg DeletedVertices are the vertices that needed to be deleted
%   from the graph to make it acyclic.

formulas_partial_odering(Formulas, Layers, SelfLoops) :-
    formulas_ugraph(Formulas, UGRaph),
    ugraph_remove_cycles(UGRaph, UGRaph1, SelfLoops),
    ugraph_layers(UGRaph1, Layers).

ugraph_remove_cycles(UGRaph0, UGRaph, SelfLoops) :-
    ugraph_remove_self_cycles(UGRaph0, UGRaph1, SelfLoops),
    ugraph_remove_other_cycles(UGRaph1, UGRaph).

ugraph_remove_self_cycles([], [], []).
ugraph_remove_self_cycles([V-E0|T0], [V-E|T], [V|LT]) :-
    selectchk(V, E0, E),
    !,
    ugraph_remove_self_cycles(T0, T, LT).
ugraph_remove_self_cycles([VE|T0], [VE|T], SL) :-
    ugraph_remove_self_cycles(T0, T, SL).

ugraph_remove_other_cycles(UGRaph0, UGRaph) :-
    ugraph_layers(UGRaph0, _), !,
    UGRaph = UGRaph0.
ugraph_remove_other_cycles(UGRaph0, UGRaph) :-
    findall(Cycle, ugraph_cycle(UGRaph0, Cycle), AllCycles),
    sort(AllCycles, Cycles),
    shortest_cycle(Cycles, ShortestCycle),
    once(cycle_edge(ShortestCycle, Edge)),        % TODO: Smart select
    del_edges(UGRaph0, [Edge], UGRaph1),
    ugraph_remove_other_cycles(UGRaph1, UGRaph).

cycle_edge(List, F-T) :-
    List = [H|_],
    append(List, [H], List1),
    append(_, [F,T|_], List1).

ugraph_cycle(UGRaph, Cycle) :-
    ugraph_cycle_(UGRaph, Cycle0),
    canonical_cycle(Cycle0, Cycle).

canonical_cycle(Cycle0, Cycle) :-
    min_member(Min, Cycle0),
    nth0(N, Cycle0, Min),
    length(Pre, N),
    append(Pre, Post, Cycle0),
    append(Post, Pre, Cycle).

ugraph_cycle_(UGRaph, [V1|Path]) :-
    member(V0-To, UGRaph),
    member(V1, To),
    cycle_from(V1, V0, UGRaph, [V1], Path).

cycle_from(T, T, _, _, []).
cycle_from(H, T, UGRaph, Seen, [H1|Path]) :-
    memberchk(H-To, UGRaph),
    member(H1, To),
    \+ memberchk(H1, Seen),
    cycle_from(H1, T, UGRaph, [H1|Seen], Path).

shortest_cycle(Cycles, Cycle) :-
    map_list_to_pairs(length, Cycles, Keyed),
    keysort(Keyed, [_-Cycle|_]).

%!  formulas_ugraph(+Formulas, -UGRaph) is det.
%
%   Create a ugraph holding all InVar-OutVar edges.

formulas_ugraph(Formulas, UGRaph) :-
    dict_pairs(Formulas, _, FPairs),
    maplist(formula_edges, FPairs, EdgesL),
    append(EdgesL, Edges),
    vertices_edges_to_ugraph([], Edges, UGRaph).

formula_edges(Left-formula(_Right,Bindings), Edges) :-
    dict_keys(Bindings, RVars),
    maplist(mk_edge(Left), RVars, Edges).

mk_edge(LVar, RVal, RVal-LVar).


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
%   @throws model_error(no_time_formulas)

:- det(dt_expression/2).
dt_expression(Formulas, DTExpr) :-
    formula(T+DTVar, Dict) = Formulas.get(t),
    T == Dict.t,
    DTVar == Dict.DTName,
    !,
    dict_pairs(DTExpr, _, [DTName-DTVar]).
dt_expression(_, _) :-
    throw(model_error(no_time_formulas)).

%!  method_params(+Method, +DTExpr, +Constants,
%!                +FormulasIn, -FormulasOut, -MethodOut)

method_params(euler, DTExpr, Constants, Formulas, Formulas, euler) :-
    DTExpr :< Constants.
method_params(rk4, DTExpr, Constants, Formulas, DFormulas,
              rk4(DTName, DT)) :-
    $dict_keys(DTExpr, [DTName]),
    del_dict(t, Formulas, _, DFormulas),
    dict_pairs(DTExpr, _, [DTName-_DTVar]),
    DT = Constants.DTName.

%!  add_tracking(+Formulas:dict, +Constants, +State0, -State, +Options)
%!               is det.
%
%   When track(all) is given, track the values for all formulas

add_tracking(Formulas, Constants, State0, State, Options) :-
    (   option(track(all), Options)
    ;   option(method(rk4), Options)
    ),
    !,
    dict_keys(Formulas, FKeys),
    maplist(initial_value(Formulas, Constants, State0), FKeys, Pairs),
    dict_pairs(FState, _, Pairs),
    State = FState.put(State0).
add_tracking(_, _, State, State, _).

initial_value(Formulas, Constants, State0, Key, Key-Value) :-
    copy_term(Formulas.get(Key), formula(Expr,Bindings)),
    Bindings >:< State0,
    Bindings >:< Constants,
    catch(Value is Expr, error(_,_), fail),
    !.
initial_value(_Formulas, _Constants, _State0, Key, Key-_).

%!  order_formulas(+Formulas:dict, -Ordering:list(list(var))) is det.
%
%   Generate a partial ordering of  Formulas  as   a  list  of layers of
%   variables (keys of the dict).

order_formulas(Formulas, Layers) :-
    formulas_partial_odering(Formulas, Layers, _SelfLoops).


                /*******************************
                *          SIMULATOR           *
                *******************************/

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
    setup_call_cleanup(
        compile_formulas(Formulas, Ref),
        steps_(I, N, Method, Sample, State, Series),
        clean_formulas(Ref)).

steps_(I, N, Method, Sample, State, Series) :-
    I < N,
    !,
    (   (   I mod Sample =:= 0
        ;   I =:= N-1
        )
    ->  Series = [State|SeriesT]
    ;   SeriesT = Series
    ),
    step(Method, State, State1),
    I1 is I+1,
    steps_(I1, N, Method, Sample, State1, SeriesT).
steps_(_, _, _, _, _, []).

%!  step(+Method, +S0, -S) is det.
%
%   @see https://en.wikipedia.org/wiki/Runge%E2%80%93Kutta_methods

step(rk4(DT,H), Ys, Y) =>
    del_dict(t, Ys, T0, Y0),
    H2 is H/2,
    T2 is T0+H/2,
    Te is T0+H,
    eval_d(T0, DT, H, Y0, K1),
    slope(Y0, K1, H2, Y1),                         % Y1 is Y0+K1*H/2
    eval_d(T2, DT, H, Y1, K2),
    slope(Y0, K2, H2, Y2),
    eval_d(T2, DT, H, Y2, K3),
    slope(Y0, K3, H, Y3),
    eval_d(Te, DT, H, Y3, K4),
    sum_dict_list([K1,2*K2,2*K3,K4], K),
    H6 is H/6,
    slope(Y0, K, H6, Yt),
    Y = Yt.put(t, Te).
step(euler, S0, S) =>
    euler_step(S0, S).

:- thread_local euler_step/2.

compile_formulas(Formulas, Ref) :-
    dict_pairs(Formulas, _, Pairs),
    dict_same_keys(Formulas, S0),
    dict_same_keys(Formulas, S),
    partition(is_delta_formula, Pairs, Deltas, Normal),
    maplist(eval(S0, S), Normal, EvalNormal),
    maplist(eval_delta(S0, S), Deltas, EvalDelta),
    append(EvalNormal, EvalDelta, Eval),
    comma_list(Body, Eval),
    assertz((euler_step(S0, S) :- Body), Ref),
    (   debugging(euler_step_clause)
    ->  portray_clause(user_error, (euler_step(S0, S) :- Body))
    ;   true
    ),
    ieee_floats.

eval(S0, S, Key-formula(Expr, Bindings),
     ( S0 >:< Bindings,
       Value is Expr)) :-
    get_dict(Key, S, Value).

eval_delta(S0, S, Key-formula(δ(Of), Bindings), Eval) =>
    get_dict(OfKey, Bindings, OfB),
    assertion(Of == OfB),
    get_dict(Key, S, D1),
    get_dict(OfKey, S, V1),
    get_dict(OfKey, S0, V0),
    Eval = ( D1 is V1-V0 ).

clean_formulas(Ref) :-
    iso_floats,
    erase(Ref).

ieee_floats :-
    set_prolog_flag(float_overflow, infinity),
    set_prolog_flag(float_zero_div, infinity),
    set_prolog_flag(float_undefined, nan).

iso_floats :-
    set_prolog_flag(float_overflow, error),
    set_prolog_flag(float_zero_div, error),
    set_prolog_flag(float_undefined, error).

is_delta_formula(_-formula(δ(_),_)) => true.
is_delta_formula(_) => fail.



%!  eval_d(+Formulas, +T, +DT, +H, +Y0, -K) is det.
%
%   K is the derivative of Formulas at T and Y0.
%
%   @arg DT is the name of the variable holding the _delta T_
%   @arg H is the interval we use.  This is irrelevant as we
%   divide by it again.

:- det(eval_d/5).
eval_d(T, DT, H, Y0, K) :-
    dict_pairs(Extra, _, [DT-H,t-T]),
    Y1 = Y0.put(Extra),
    euler_step(Y1, Y2),
    derivative_(Y0,Y2,H,K).

derivative_(Y0, Y1, H, K) :-
    mapdict(derivative__(H), Y0, Y1, K).

derivative__(H, _P, Y0, Y1, K) :-
    K is (Y1-Y0)/H.

%!  slope(+Y0, +K, +H, -Y) is det.

:- det(slope/4).
slope(Y0, K, H, Y1) :-
    mapdict(slope_(K, H), Y0, Y1).

slope_(K, H, P, V0, V) :-
    get_dict(P, K, S),
    V is V0 + S * H.

%!  sum_dict_list(+Dicts, -Dict) is det.

:- det(sum_dict_list/2).
sum_dict_list([D1,D2|T], Dict) =>
    sum_dict(D1, D2, S1),
    sum_dict_list([S1|T], Dict).
sum_dict_list([D], Dict) =>
    Dict = D.

sum_dict(D1, N2*D2, D) =>
    mapdict(sum_(N2), D1, D2, D).
sum_dict(D1, D2, D) =>
    mapdict(sum_, D1, D2, D).

sum_(_P, V1, V2, V) :-
    V is V1+V2.
sum_(N, _P, V1, V2, V) :-
    V is V1+N*V2.

%!  init_derivatives(+Series, -DSeries, +IdMapping) is det.
%
%   Change the values for Series into a  term d(V,D1,D2,D3). If the keys
%   are quantities, the simulation value is used to fill `V`. If it is a
%   delta-term, we find the corresponding quantity and fill `D1`.
%
%   Note that we may have both the value and derivative. In that case we
%   need to combine both into one.

init_derivatives(Series, DSeries, IdMapping) :-
    Series = [H|_],
    make_derivative_map(H, IdMapping, In, Out),
    maplist(derivative_map(In, Out), Series, DSeries).

make_derivative_map(Dict, IdMapping, In, Out) :-
    dict_pairs(Dict, _, Pairs),
    maplist(make_derivative_map_(IdMapping), Pairs, InPairs, OutPairs),
    dict_pairs(In, _, InPairs),
    keysort(OutPairs, OutPairs1),                  % we may have value and
    join_pairs(OutPairs1, OutPairs2),              % derivative
    dict_pairs(Out, _, OutPairs2).

make_derivative_map_(IdMapping, K-_, K-D0, Id-d(_,D0,_,_)) :-
    key_derivative(Id, K, IdMapping),
    !.
make_derivative_map_(_IdMapping, K-_, K-V0, K-d(V0,_,_,_)).

join_pairs([], []).
join_pairs([K-V,K-V|T0], Pairs) :-
    !,
    join_pairs([K-V|T0], Pairs).
join_pairs([H|T0], [H|T]) :-
    join_pairs(T0, T).

derivative_map(In, Out, DictIn, DictOut) :-
    copy_term(In+Out, DictIn+DictOut).

%!  add_derivative(+Nth, +Series) is det.
%
%   Materialize the Nth derivative in Series. Series   is  a dict Key ->
%   d(V,D1,D2,D3).  Nth1  is   0..3.   The    logic   both   allows  for
%   differentiation and integration. Initially, a combination of the 0th
%   (value) and 1st derivative may be filled.

add_derivative(_, []) :-
    !.
add_derivative(_, [_]) :-
    !.
add_derivative(Nth0, [H1|T1]) :-
    T1 = [H2|_],
    Nth is Nth0+1,
    add_derivative_1(Nth, H1, H2),
    add_derivative(Nth0, T1).

add_derivative_1(Nth, H1, H2) :-
    mapdict(add_derivative_k(Nth), H1, H2).

add_derivative_k(Nth, _K, _A1, A2) :-
    arg(Nth, A2, V),
    nonvar(V),
    !.
add_derivative_k(Nth, _K, A1, A2) :-
    P is Nth-1,
    P > 0,
    arg(P, A1, V1),
    arg(P, A2, V2),
    normal_number(V1), normal_number(V2),
    !,
    D is V2-V1,
    arg(Nth, A2, D).
add_derivative_k(Nth, _K, A1, A2) :-
    N is Nth+1,
    arg(Nth, A1, V1), normal_number(V1),
    arg(N, A2, V2),   normal_number(V2),
    !,
    I is V1+V2,
    arg(Nth, A2, I).
add_derivative_k(_Nth, _K, _A1, _A2).

%!  normal_number(@Term) is semidet.
%
%   True when Term is a _normal_ number, i.e., a rational or normal
%   float.

normal_number(N), rational(N) => true.
normal_number(N), float(N) => float_class(N, C), ok_float_class(C).
normal_number(N), var(N) => fail.

ok_float_class(zero).
ok_float_class(subnormal).
ok_float_class(normal).

%!  min_list_normal(+Nums, -Min) is det.
%!  max_list_normal(+Nums, -Max) is det.
%
%   Get the min/max number of a list, ignoring non-normal values.

min_list_normal(Nums, Min) :-
    include(normal_number, Nums, Normal),
    (   min_list(Normal, Min0)
    ->  Min = Min0
    ;   Min = 0.0
    ).

max_list_normal(Nums, Max) :-
    include(normal_number, Nums, Normal),
    (   max_list(Normal, Max0)
    ->  Max = Max0
    ;   Max = 0.0
    ).

%!  normal_mid(+N1, +N2, -Ni) is det.
%
%   Compute the mid point between two numbers that may be abnormal.

normal_mid(N1, N2, Ni) :-
    normal_number(N1),
    normal_number(N2),
    !,
    Ni is (N1+N2)/2.
normal_mid(_, _, _).

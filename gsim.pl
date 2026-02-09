:- module(gsim,
          [ read_model/5,          % +Source, -Formulas, -Constants, -State0,
                                   % +Opts
            order_formulas/3,      % +Formulas, -Ordering, +Options
            simulate/3,            % +ModelSrc, -Series, +Options
            init_derivatives/3,    % +Series, -DSeries, +IdMapping
            add_derivative/2,      % +Nth, +Series
            read_model_to_terms/2, % ++Input, -Terms
            normal_number/1,       % @Term
            min_list_normal/2,     % +List, -Min
            max_list_normal/2,     % +List, -Max
            normal_mid/3,          % +N1, +N2, -Ni
            ground_formula/2       % +Formula, -Grounded
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
:- use_module(library(prolog_code), [comma_list/2]).
:- use_module(library(dcg/high_order), [sequence/4]).
:- use_module(library(ordsets), [ord_intersection/3]).
:- use_module(library(pairs), [map_list_to_pairs/3]).
:- use_module(library(ugraphs),
              [ugraph_layers/2, vertices_edges_to_ugraph/3, del_edges/3]).

:- use_module(model).
:- use_module(identifiers).
:- use_module(debug).
:- use_module(map).

% :- set_prolog_flag(optimise, true).

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
    read_model(From, Formulas, Constants, State0,
               [insert_causal_steps(true)|Options]),
    add_tracking(Formulas, Constants, State0, State, [method(Method)|Options]),
    intern_constants(Constants, DTExpr, Formulas, Formulas1),
    method_params(Method, DTExpr, Constants, Formulas1, Formulas2, MethodP),
    steps(Count, MethodP, Sample, Formulas2, State, Series, Options).

%!  read_model(+Source, -Formulas:pairs, -Constants:pairs, -State0:dict,
%!             +Options) is det.
%
%   @arg Formulas is list of pairs `QuantityId` -
%   formula(Expression, Bindings), where Bindings is a dict `QuantityId`
%   -> `Var`.
%   @arg Constants is a list of pairs `ConstantId` - `Value`.
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
    split_formulas(Terms3, Formulas0, Init),
    insert_causal_steps(Formulas0, Formulas1, Options),
    initialise_model(Formulas1, Init, Formulas, Constants, State, Options).

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

%!  add_derivative_equations(+EquationsIn, -Equations) is det.
%
%   If there are formulas that use the   derivative  of some quantity as
%   inputs while only the quantity itself exists, add an equation
%
%       'ΔQ' := δ(Q).
%
%   after the equation for Q.

add_derivative_equations(Equations0, Equations) :-
    equation_quantities(Equations0, Quantities),
    findall(DQ, dependent_derivative(Equations0, DQ), DQs0),
    sort(DQs0, DQs),
    ord_subtract(DQs, Quantities, Required),
    insert_derivative_equations(Equations0, Required, Equations).

dependent_derivative(Equations, DQ) :-
    member(_Q := Expr, Equations),
    sub_term(DQ, Expr),
    is_derivative_term(DQ).

insert_derivative_equations([], _, []).
insert_derivative_equations([H|T0], Required, [H,DQ|T]) :-
    quantity(H, Q),
    memberchk(Q, Required),
    !,
    d_equation(Q, DQ),
    insert_derivative_equations(T0, Required, T).
insert_derivative_equations([H|T0], Required, [H|T]) :-
    insert_derivative_equations(T0, Required, T).

d_equation(DQ, DQ := δ(Q)) :-
    term_derivative(Q, DQ).

%!  insert_causal_steps(+Formulas0, -Formulas, +Options) is det.
%
%   For each formula `X := X+C*Δt` and  `C   :=  ...`, add `ΔX` := C and
%   change to `X := ΔX+C*Δt`.

insert_causal_steps(Formulas0, Formulas, Options) :-
    option(insert_causal_steps(true), Options),
    !,
    option(id_mapping(IdMapping), Options, #{}),
    insert_causal_steps_(Formulas0, IdMapping, Formulas).
insert_causal_steps(Formulas, Formulas, _).

insert_causal_steps_(Formulas0, IdMapping, Formulas) :-
    select(Q-formula(Expr0,Bindings), Formulas0, Formulas1),
    is_integration(Q, Expr0, Bindings, IdMapping, F1, F2),
    !,
    insert_causal_steps_([F1,F2|Formulas1], IdMapping, Formulas).
insert_causal_steps_(Formulas, _, Formulas).

%!  is_integration(+Q, +Expr0, +Bindings, +IdMapping,
%!                 -F1, -F2) is semidet.
%
%   True when Q-formula(Expr0,Bindings) expresses and integration and F1
%   is the modified version of this formula using   ΔQ  and F2 is a link
%   formula that computes ΔQ. The  idea  is   that  F2  must go into the
%   _layer_ before F1.

is_integration(Q, Expr0, Bindings, IdMapping,
               Q-formula(Expr,QBindings),
               DQ-formula(C,CBindings)) :-
    integration(Expr0, Expr, X, C, VDQ, Dt),
    var_name(Bindings,  X, Q),
    term_key('Δt', DtKey, IdMapping),
    var_name(Bindings, Dt, DtKey),
    key_derivative(Q, DQ, IdMapping),
    \+ var_name(Bindings, C, DQ),
    !,
    canonical_formula_bindings(Expr, Bindings.put(DQ, VDQ), QBindings),
    canonical_formula_bindings(C, Bindings, CBindings).

integration(X+C*Dt, Expr, X1, C1, VDQ, Dt1) =>
    Expr = X+VDQ*Dt, X1 = X, C1 = C, Dt1 = Dt.
integration(X-C*Dt, Expr, X1, C1, VDQ, Dt1) =>
    Expr = X-VDQ*Dt, X1 = X, C1 = C, Dt1 = Dt.
integration(_, _, _, _, _, _) =>
    fail.

var_name(Bindings, Var, Name) :-
    var(Var),
    get_dict(Name, Bindings, V),
    V == Var,
    !.

%!  canonical_formula_bindings(+Expression, +BindingDict,
%!                             -MinimalBindingDict) is det.

canonical_formula_bindings(Expr, Bindings0, Bindings) :-
    term_variables(Expr, Vars),
    dict_pairs(Bindings0, Tag, Pairs0),
    include(var_in_expression(Vars), Pairs0, Pairs),
    dict_pairs(Bindings, Tag, Pairs).

var_in_expression(Vars, _Key-Var) :-
    member(V2, Vars),
    V2 == Var,
    !.

%!  ground_formula(+Formula, -Grounded) is det.
%
%   Given Var-formula(Expr, Bindings), bind each variable in Expr to its
%   name, producing `Var := Expr`. Note   that  the ariables in Grounded
%   are _keys_ (as opposed to _terms_.

ground_formula(Var-formula(Expr, Bindings), Result) =>
    mapsubterms_var(var_name(Bindings), Expr, RExpr),
    Result = (Var := RExpr).

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
    strip_placeholders(Invalid0, Invalid1, Options),
    (   Invalid1 == []
    ->  true
    ;   sort(Invalid1, Invalid),
        throw(model_error(invalid(Invalid)))
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

%!  split_formulas(+Terms:pairs, -Formulas, -Init) is det.
%
%   Split the input into formulas and constants.

split_formulas([], Formulas, Init) =>
    Formulas = [],
    Init = [].
split_formulas([(Left := Right)-Bindings|Terms], Formulas, Init),
    dict_pairs(Bindings, _, [Id-Left]) =>         % Left only binding
    (   is_placeholder(Right)
    ->  Value = Right
    ;   ground(Right)
    ->  Value is Right
    ;   Value = Right                             % only for incomplete formulas
    ),
    Init = [Id-Value|Init1],
    split_formulas(Terms, Formulas, Init1).
split_formulas([(Left := Right)-Bindings|Terms], Formulas, Init) =>
    dict_pairs(Bindings, _, Pairs),
    select(Id-Var, Pairs, Pairs1),
    Var == Left,
    !,
    (   same_variables(Right, Pairs)
    ->  RightBindings = Bindings
    ;   assertion(same_variables(Right, Pairs1)),
        dict_pairs(RightBindings, _, Pairs1)
    ),
    Formulas = [Id-formula(Right, RightBindings)|Formulas1],
    split_formulas(Terms, Formulas1, Init).

same_variables(T1, T2) :-
    term_variables(T1, V1), sort(V1, Vs1),
    term_variables(T2, V2), sort(V2, Vs2),
    Vs1 == Vs2.

%!  initialise_model(+FormulasIn:pairs, +Init:pairs, -Formulas:pairs,
%!                   -Constants:pairs, -State:dict, +Options) is det.
%
%   Determine the final set  of  formulas,   constants  and  the initial
%   state. Mathematically, we can  set  Δt   to  zero  and propagate all
%   constants and equations.
%
%   @arg FormulasIn is a list of Id-formula(Right,RightBindings)
%   @arg Init is a list of Id-Value, where Value can be an expression
%   if not all constants are known.

initialise_model(Formulas0, Init, Formulas, Constants, State, Options) :-
    split_init(Init, Formulas0, Constants0, State0),
    copy_term(Formulas0, Formulas1),
    derived_constants(Formulas0, Constants0, Formulas, Constants),
    dict_pairs(ConstantsDict, #, Constants),
    dict_pairs(StateDict0, #, State0),
    derived_initial_state(Formulas1, ConstantsDict,
                          StateDict0, State, Options).

%!  split_init(+Init:pairs, +Formulas:pairs, -Constants:pairs,
%!             -State:dict) is det.
%
%   Split the quantities for  which  we   found  an  initial  value into
%   constants and the initial state dict. Both   map an id to a concrete
%   number.

split_init(Init, Formulas, Constants, StatePairs) :-
    split_init_(Init, Formulas, Constants, StatePairs).

split_init_([], _, [], []).
split_init_([Id-Value|T], Formulas, ConstantPairs, [Id-Value|StatePairs]) :-
    memberchk(Id-_, Formulas),
    !,
    split_init_(T, Formulas, ConstantPairs, StatePairs).
split_init_([Id-Value|T], Formulas, [Id-Value|ConstantPairs], StatePairs) :-
    split_init_(T, Formulas, ConstantPairs, StatePairs).

%!  derived_constants(+Formulas0:pairs, +Constants0:pairs,
%!                    -Formulas:pairs, -Constants:pairs) is det.
%
%   If a formula just has constants at the  right, remove it and add the
%   value to Constants.

derived_constants(Formulas0, Constants0, Formulas, Constants) :-
    derived_constants_(Formulas0, Constants0, Formulas, Constants).

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
    map_filled_placeholders(Right, Right1),
    Value is Right1,
    Constants1 = [Left-Value|Constants0],
    derived_constants__(FT, Constants1, FPairs, Constants).
derived_constants__([FH|FT], Constants0, [FH|FPairs], Constants) :-
    derived_constants__(FT, Constants0, FPairs, Constants).

%!  derived_initial_state(+Formulas:pairs, +Constants:dict,
%!			  +State0:dict, -State:dict, +Options) is det.
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

%!  derived_initials(+Missing, -Unresolved:ordset, +Formulas:pairs,
%!                   +DTExpr, +Constants, +State0, -State) is det.
%
%   Remove missing values that can be   computed  from other fully known
%   values.

derived_initials([], [], _, _, _, State, State) :-
    !.
derived_initials(Missing, Unres, Formulas, DTExpr, Constants, State0, State) :-
    select(Key, Missing, Missing1),
    member(Key-formula(Right,Bindings), Formulas),
    Constants >:< Bindings,
    State0 >:< Bindings,
    DTExpr >:< Bindings,
    ground(Right),
    \+ is_placeholder(Right),
    \+ is_delta_function(Right),
    !,
    map_filled_placeholders(Right, Right1),
    Value is Right1,
    State1 = State0.put(Key,Value),
    derived_initials(Missing1, Unres, Formulas, DTExpr, Constants,
                     State1, State).
derived_initials(Missing, Unresolved, _, _, _, State, State) :-
    sort(Missing, Unresolved).

%!  map_filled_placeholders(+TermIn, -Term) is det.

map_filled_placeholders(Expr0, Expr) :-
    mapsubterms(map_filled_placeholder, Expr0, Expr).

map_filled_placeholder(placeholder(_Type, Value), Result), number(Value) =>
    Result = Value.
map_filled_placeholder(_, _) => fail.


%!  missing_init(+Formulas:pairs, +Constants:dict, +State:dict, -Key) is
%!               nondet.
%
%   True when Key appears at the right side of an equation and refers to
%   a variable for which there is no   constant and that does not appear
%   in State.

missing_init(Formulas, Constants, State, Key) :-
    member(_-formula(Right, Bindings), Formulas),
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

%!  formulas_needs_init(+Formulas:pairs, -NeedsInit:ordset) is det.
%
%   Find the variables that require to be initialized.  That is
%    - Any variable that depends on itself (i.e., integrations)
%    - Any _source_ of the dependency graph.

formulas_needs_init(Formulas, NeedsInit) :-
    formulas_partial_ordering(Formulas, Layers, SelfLoops, []),
    Layers = [First|_],
    append(First, SelfLoops, NeedsInit0),
    sort(NeedsInit0, NeedsInit).

%!  extend_state(+FormulaLayers:list(pairs), +State0, -State) is det.
%
%   Add possible new left hand side of formulas to State0.

extend_state(FormulaLayers, State0, State) :-
    append(FormulaLayers, Formulas),
    pairs_keys(Formulas, Keys),
    foldl(add_key_to_state, Keys, State0, State).

add_key_to_state(Key, State0, State) :-
    get_dict(Key, State0, _),
    !,
    State = State0.
add_key_to_state(Key, State0, State) :-
    put_dict(Key, State0, _, State).

%!  formulas_partial_ordering(+Formulas:pairs, -Layers, -SelfLoops,
%!                            +Options) is det.
%
%   Create  a  partial  ordering  of  the    formulas   based  on  their
%   dependencies. Note that the dependencies   are  typically cyclic and
%   thus we have to break the cycles.  This consists of two steps. First
%   we remove the _self cycles_ as  these   are  evident. Next we remove
%   some arbitrary cycle.
%
%   @arg Formulas is a dict `Var:formula(Prolog,Bindings)`.  Note that
%   `Var` here means the (atom) name of a quantity or constant.
%   @arg Layers is a list of lists of variables (atoms) representing a
%   partial ordering in the dependencies.  The first layer contains all
%   constants.
%   @arg DeletedVertices are the vertices that needed to be deleted
%   from the graph to make it acyclic.

:- det(formulas_partial_ordering/4).
/*
formulas_partial_ordering(Formulas, Layers, SelfLoops, Options) :-
    option(model(ModelId), Options),
    q_partial_ordering(ModelId, QLayers, [constants(remove)|Options]),
    formulas_partial_ordering_(Formulas, Layers, SelfLoops, nondet),
    consistent_ordering(QLayers, Layers),
    !.
*/
formulas_partial_ordering(Formulas, Layers, SelfLoops, _Options) :-
    formulas_partial_ordering_(Formulas, Layers, SelfLoops, det).

formulas_partial_ordering_(Formulas, Layers, SelfLoops, Det) :-
    formulas_ugraph(Formulas, UGRaph),
    ugraph_remove_cycles(UGRaph, UGRaph1, SelfLoops, Det),
    ugraph_layers(UGRaph1, Layers).

ugraph_remove_cycles(UGRaph0, UGRaph, SelfLoops, Det) :-
    ugraph_remove_self_cycles(UGRaph0, UGRaph1, SelfLoops),
    ugraph_remove_other_cycles(UGRaph1, UGRaph, Det).

ugraph_remove_self_cycles([], [], []).
ugraph_remove_self_cycles([V-E0|T0], [V-E|T], [V|LT]) :-
    selectchk(V, E0, E),
    !,
    ugraph_remove_self_cycles(T0, T, LT).
ugraph_remove_self_cycles([VE|T0], [VE|T], SL) :-
    ugraph_remove_self_cycles(T0, T, SL).

ugraph_remove_other_cycles(UGRaph0, UGRaph, _) :-
    ugraph_layers(UGRaph0, _), !,
    UGRaph = UGRaph0.
ugraph_remove_other_cycles(UGRaph0, UGRaph, Det) :-
    findall(Cycle, ugraph_cycle(UGRaph0, Cycle), AllCycles),
    sort(AllCycles, Cycles),
    shortest_cycle(Cycles, ShortestCycle),
    (   Det == det
    ->  once(cycle_edge(ShortestCycle, Edge))
    ;   cycle_edge(ShortestCycle, Edge)
    ),
    del_edges(UGRaph0, [Edge], UGRaph1),
    ugraph_remove_other_cycles(UGRaph1, UGRaph, Det).

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

%!  formulas_ugraph(+Formulas:pairs, -UGRaph) is det.
%
%   Create a ugraph holding all InVar-OutVar edges.

formulas_ugraph(Formulas, UGRaph) :-
    maplist(formula_edges, Formulas, EdgesL),
    append(EdgesL, Edges),
    vertices_edges_to_ugraph([], Edges, UGRaph).

formula_edges(Left-formula(_Right,Bindings), Edges) :-
    dict_keys(Bindings, RVars),
    maplist(mk_edge(Left), RVars, Edges).

mk_edge(LVar, RVal, RVal-LVar).

%!  consistent_ordering(+QLayers, +Layers) is semidet.
%
%   True when the simulation derived quantity ordering QLayers (computed
%   by q_partial_ordering/3) is consistent with   the dependency derived
%   order. As we can break cycles  in multiple ways, multiple dependency
%   based orders are possible. We define consistency  as "if Q2 is after
%   Q1 in the Garp model and both appear  in the numeric model, the same
%   relation should apply".
%
%   @arg QLayers is a list of lists of quantities extracted and ordered
%   from the Garp simulation.
%   @arg Layers has the same format, but contains all quantities,
%   including constants from the numerical simulation and is ordered
%   based on dependencies derived from the formulas.

consistent_ordering(QLayers, Layers) :-
    forall(( q_layer(Lq1, QLayers, Q1),
             q_layer(Lq2, QLayers, Q2),
             Lq2 > Lq1
           ),
           (   q_layer(Ln1, Layers, Q1),
               q_layer(Ln2, Layers, Q2)
           ->  Ln2 > Ln1
           ;   true
           )).

q_layer(Layer, Layers, Q) :-
    nth0(Layer, Layers, QL),
    member(Q, QL).

%!  layer_formulas(+Formulas:list, -Layers:list(list), +Options) is
%!                 semidet.
%
%   Organise Formulas into a list of   sub-sets of Formulas according to
%   the causal ordering derived by  Garp.  If   a  Garp  Quantity is the
%   result of integration and starts at  point(zero) with ΔQ starting at
%   zero, we normally see two Garp  states,   first  ΔQ moves and in the
%   next  state  Q  moves.  This   requires  introducing  an  additional
%   formaula, such that we can move the ΔQ in one layer and the Q in the
%   next.  For example:
%
%   ```
%   a := ...
%   v := v + a*Δt
%   ```
%
%   Should become
%
%   ```
%   a := ...
%   Δv := a,
%   v := v + Δv*Δt.
%   ```
%
%   We must do this transformation for any
%
%   @arg Formulas is a list of Q-formula(Expression,Bindings).

layer_formulas(Formulas, Layers, Options) :-
    option(model(ModelId), Options),
    q_partial_ordering(ModelId, QLayers,
                       [ %derivatives(true),
                         constants(remove)
                       | Options
                       ]),
    $,
    select(t-formula(X+Dt,Bindings), Formulas, Formulas1),
    layer_formulas_(QLayers, Formulas1, Layers0, Options),
    flatten_layers(Layers0, Layers1),
    delete(Layers1, [], Layers2), % delete empty layers
    length(Layers2, NLayers),
    Dt1 is Dt/NLayers,
    maplist(append([t-formula(X+Dt1,Bindings)]), Layers2, Layers).

layer_formulas_([], [], [], _) :-
    !.
layer_formulas_([], Formulas, [Formulas], _Options).
layer_formulas_([H|T], Formulas0, [d(DerLayer,Layer)|LayerT], Options) :-
    partition(formula_targets(H), Formulas0, Layer, Formulas1),
    partition(derivatives_first(Layer, Options),
              Formulas1, DerLayer, Formulas2),
    layer_formulas_(T, Formulas2, LayerT, Options).

formula_targets(QLayer, Var-_Expression) :-
    memberchk(Var, QLayer).

derivatives_first(QLayer, Options, DVar-_DExpression) :-
    member(QVar-_QExpression, QLayer),
    key_is_derivative_of(DVar, QVar, Options).

flatten_layers([d([],QL0)|T], Layers) :-
    !,
    flatten_layers_([d([],QL0)|T], Layers).
flatten_layers([d(DL,QL0)|T], [DL|Layers]) :-
    flatten_layers_([d([],QL0)|T], Layers).

flatten_layers_([], []).
flatten_layers_([d(_,QL)], [QL]) :-
    !.
flatten_layers_([d(_DL0,QL0),d(DL1,QL1)|T], [QL|Layers]) :-
    append(QL0, DL1, QL),
    flatten_layers_([d(DL1,QL1)|T], Layers).

%!  key_is_derivative_of(+DKey, +QKey, +Options) is semidet.
%
%   True if DKey is a key for the derivative of QKey.

key_is_derivative_of(D, Q, Options) :-
    option(id_mapping(IdMapping), Options, #{}),
    term_key(TD, D, IdMapping),
    term_key(TQ, Q, IdMapping),
    term_derivative(TQ, TDVar),
    TDVar == TD.

%!  insert_propagation_formulas(+LayersIn, -Layers, +Options) is det.
%
%   If a layer has a formula with left-hand side ΔQ, search for formulas
%   with a right hand side Q and   establish  ist derived form. Add this
%   formula to the current layer.

insert_propagation_formulas(Layers0, Layers, Options) :-
    append(Layers0, Formulas),
    maplist(insert_propagation_formulas_(Options, Formulas),
            Layers0, Layers).

insert_propagation_formulas_(Options, Formulas, Layer0, Layer) :-
    append(Prefix, [DFormula|Postfix0], Layer0),
    is_dformula(DFormula, Q, Options),
    $,
    propagates_to(Q, Formulas, Props, Options),
    append(Props, Postfix0, Postfix1),
    insert_propagation_formulas_(Options, Formulas, Postfix1, Postfix),
    append([Prefix,[DFormula],Postfix], Layer).
insert_propagation_formulas_(_, _, Layer, Layer).

is_dformula(DQ-_, Q, Options) =>
    option(id_mapping(IdMapping), Options),
    term_key(DTerm, DQ, IdMapping),
    term_derivative(Term, DTerm),
    $,
    term_key(Term, Q, IdMapping).
is_dformula(_-_, _, _) =>
    fail.

propagates_to(Q, Formulas, [Prop|T], Options) :-
    propagates_to_(Q, Formulas, Prop, Options), !,
    Prop = Q2-_,
    propagates_to(Q2, Formulas, T, Options).
propagates_to(_, _, [], _).

propagates_to_(Q, Formulas, Prop, Options) :-
    Formula = formula(_,Bindings),
    member(Q2-Formula, Formulas),
    dict_keys(Bindings, Vars),
    memberchk(Q, Vars),
    derive_formula(Q, Q2-Formula, Prop, Options),
    !.

derive_formula(V, Q-formula(Expr,Bindings), Prop, Options),
    get_dict(V, Bindings, VVar),
    derivative(VVar, Expr, DVar, DExpr) =>
    option(id_mapping(IdMapping), Options, #{}),
    Prop = DQ-formula(DExpr, DBindings),
    key_derivative(Q, DQ, IdMapping),
    key_derivative(V, DV, IdMapping),
    canonical_formula_bindings(DExpr, Bindings.put(DV, DVar), DBindings).
derive_formula(_, _, _, _) =>
    fail.

derivative(V,   C*V, DV, Der) => Der =  C*DV.
derivative(V, _+C*V, DV, Der) => Der =  C*DV.
derivative(V, _-C*V, DV, Der) => Der = -C*DV.
derivative(V, _+V,   DV, Der) => Der =  DV.
derivative(V, _-V,   DV, Der) => Der = -DV.
derivative(_, _, _, _)        => fail.

%!  intern_constants(+Constants:pairs, -DTExpr, +FormualsIn:pairs,
%!                   -Formuals:pairs) is det.
%
%   Simplify FormualsIn by binding the constants  and removing them from
%   the bindings term of the formulas.

intern_constants(Constants0, DTExpr, Formulas0, Formulas) :-
    dt_expression(Formulas0, DTExpr),
    dict_keys(DTExpr, [DTName]),
    selectchk(DTName-_, Constants0, Constants),
    maplist(intern_constants_(Constants), Formulas0, Formulas).

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
%   @arg Formulas is either a dict or list of pairs mapping Ids to
%   terms formula(Expression, Bindings).
%   @arg DTExpr is a dict _{DTName: DTVar}.
%   @throws model_error(no_time_formulas)

:- det(dt_expression/2).
dt_expression(Formulas, DTExpr) :-
    is_dict(Formulas),
    formula(T+DTVar, Dict) = Formulas.get(t),
    T == Dict.t,
    DTVar == Dict.DTName,
    !,
    dict_pairs(DTExpr, _, [DTName-DTVar]).
dt_expression(Formulas, DTExpr) :-
    member(t-formula(T+DTVar, Dict), Formulas),
    T == Dict.t,
    DTVar == Dict.DTName,
    !,
    dict_pairs(DTExpr, _, [DTName-DTVar]).
dt_expression(_, _) :-
    throw(model_error(no_time_formulas)).

%!  method_params(+Method:atom, +DTExpr:dict, +Constants:pairs,
%!                +FormulasIn:pairs, -FormulasOut:pairs, -MethodOut)
%
%   When using RK4, find the time expression as this needs to be treated
%   special.  Also remove the time expression from FormulasIn.

method_params(euler, DTExpr, Constants, Formulas, Formulas, euler) :-
    DTExpr :< Constants.
method_params(rk4, DTExpr, Constants, Formulas, DFormulas,
              rk4(DTName, DT)) :-
    $dict_keys(DTExpr, [DTName]),
    selectchk(t-_, Formulas, DFormulas),
    dict_pairs(DTExpr, _, [DTName-_DTVar]),
    memberchk(DTName-DT, Constants).

%!  add_tracking(+Formulas:pairs, +Constants:pairs,
%!		 +State0:dict, -State:dict, +Options) is det.
%
%   When track(all) is given, track the values for all formulas. We also
%   have to do this for RK4.

add_tracking(Formulas, Constants, State0, State, Options) :-
    (   option(track(all), Options)
    ;   option(method(rk4), Options)
    ),
    !,
    pairs_keys(Formulas, FKeys),
    dict_pairs(ConstantDict, _, Constants),
    maplist(initial_value(Formulas, ConstantDict, State0), FKeys, Pairs),
    dict_pairs(FState, _, Pairs),
    State = FState.put(State0).
add_tracking(_, _, State, State, _).

initial_value(Formulas, Constants, State0, Key, Key-Value) :-
    memberchk(Key-Formula, Formulas),
    copy_term(Formula, formula(Expr,Bindings)),
    Bindings >:< State0,
    Bindings >:< Constants,
    catch(Value is Expr, error(_,_), fail),
    !.
initial_value(_Formulas, _Constants, _State0, Key, Key-_).

%!  order_formulas(+Formulas:dict, -Ordering:list(list(var)), +Options)
%!                 is det.
%
%   Generate a partial ordering of  Formulas  as   a  list  of layers of
%   variables (keys of the dict).

order_formulas(Formulas, Layers, Options) :-
    formulas_partial_ordering(Formulas, Layers, _SelfLoops, Options).


                /*******************************
                *          SIMULATOR           *
                *******************************/

%!  steps(+N, +Method, +Sample, +Formulas, +State, -Series,
%!        +Options) is det.
%
%   Run the actual simulation.   Options:
%
%     - formula_layers(-Layers)
%       Unify Layers with a list of lists of the final formulas.
%       Each formula takes the shape Var-formula(Expr, Bindings).
%
%   @arg Number of iterations
%   @arg Method is one of `euler` or rk4(DTName, DT)
%   @arg Sample indicates that we create the output Series
%   by sampling every N iterations.
%   @arg Formulas is a list of pairs `Id` - formula(Expr,Bindings)
%   @arg State is a dict `Id` -> `Value`
%   @arg Series is a list of dicts with the same state as
%        `State` but different values.

steps(Count, Method, Sample, Formulas, State0, Series, Options) :-
    layer_formulas(Formulas, FormulaLayers0, Options),
    !,
    insert_propagation_formulas(FormulaLayers0, FormulaLayers, Options),
    option(formula_layers(FormulaLayers), Options, _),
    extend_state(FormulaLayers, State0, State1),
    dict_keys(State1, Keys),
    setup_call_cleanup(
        foldl(compile_formulas(Method, Keys), FormulaLayers, Refs, 0, _),
        ( length(Refs, NLayers),
          steps_(0, NLayers, NLayers, Method, Sample,
                 State1, State2, Series, TSeries)
        ),
        maplist(clean_formulas, Refs)),
    flat_steps(NLayers, Count, Method, Sample, Formulas, State2, TSeries).
steps(Count, Method, Sample, Formulas, State0, Series, _Options) :-
    flat_steps(0, Count, Method, Sample, Formulas, State0, Series).

flat_steps(I, Count, Method, Sample, Formulas, State0, Series) :-
    dict_keys(State0, Keys),
    setup_call_cleanup(
        compile_formulas(Method, Keys, Formulas, Ref, 0, _),
        steps_(I, Count, 1, Method, Sample, State0, _State, Series, []),
        clean_formulas(Ref)).

steps_(I, Count, NLayers, Method, Sample, State0, State, Series, TSeries) :-
    I < Count,
    !,
    (   (   I mod Sample =:= 0
        ;   I =:= Count-1
        )
    ->  Series = [State0|SeriesT]
    ;   SeriesT = Series
    ),
    SubStep is I mod NLayers,
    step(SubStep, Method, State0, State1),
    I1 is I+1,
    steps_(I1, Count, NLayers, Method, Sample, State1, State, SeriesT, TSeries).
steps_(_, _, _, _, _, State, State, Series, Series).

%!  step(+N, +Method, +S0, -S) is det.
%
%   @see https://en.wikipedia.org/wiki/Runge%E2%80%93Kutta_methods

step(N, rk4(DT,H), Ys, Y) =>
    del_dict(t, Ys, T0, Y0),
    H2 is H/2,
    T2 is T0+H/2,
    Te is T0+H,
    eval_d(N, T0, DT, H, Y0, K1),
    slope(Y0, K1, H2, Y1),                         % Y1 is Y0+K1*H/2
    eval_d(N, T2, DT, H, Y1, K2),
    slope(Y0, K2, H2, Y2),
    eval_d(N, T2, DT, H, Y2, K3),
    slope(Y0, K3, H, Y3),
    eval_d(N, Te, DT, H, Y3, K4),
    sum_dict_list([K1,2*K2,2*K3,K4], K),
    H6 is H/6,
    slope(Y0, K, H6, Yt),
    Y = Yt.put(t, Te).
step(N, euler, S0, S) =>
    run_euler_step(N, S0, S).

run_euler_step(N, S0, S) :-
    catch(euler_step(N, S0, S), error(instantiation_error,_), fail),
    !.
run_euler_step(N, S0, S) :-
    copy_term(S0, S1),
    complete_state(S1),
    euler_step(N, S1, S).

%!  complete_state(?State) is det.
%
%   If  we  cannot  execute  the  state  because  it  is  insufficiently
%   instantiated, set the variables to zero   (0).  This is not entirely
%   obvious. Ideally, some state variables will   start unknown and they
%   will eventually be filled. If a first  iteration we used NaN for the
%   initial  value.  Unfortunately  on  some    models   the  NaN  keeps
%   propagating.

complete_state(State) :-
    mapdict(set_var_to_zero, State).

set_var_to_zero(_, V), var(V) =>
    V = 0.			% dubious
set_var_to_zero(_, _) => true.

:- thread_local euler_step/3.                      % +SubStep, +State0, -State

%!  compile_formulas(+Method, +Keys, +Formulas:pairs, -ClauseRef) is
%!                   det.
%
%   Compile Formulas to a clause for   euler_step/2.  When using RK4, we
%   must re-add the time formulas for  eval_d/5   to  work.  We have two
%   modes.  For RK4, we use the first.
%
%     - Evaluate all formulas relative to S0 to create S1
%     - Evaluate the formulas in order, where the next formula
%       takes the value just computed by an earlier.
%
%   This predicate asserts a clause for   euler_step/2  that computes S1
%   from S0.
%
%   A single Euler step can  be  computed   two  ways.  By  default, the
%   equations are ordered and subsequent equations   use  the results of
%   earlier equations. So, using equations Eq1 and  Eq2, we do S0*Eq1 ->
%   Sa, Sa*Eq2 -> S1.  With  the   Prolog  flag  `garp_eval_batch`,  all
%   equations act on S0.

:- set_prolog_flag(garp_eval_batch, false).

compile_formulas(rk4(DTName, _DT), Keys, Formulas, Ref, N, N1) =>
    N1 is N+1,
    step_state_dicts([DTName, t|Keys], S0, S),
    partition(is_delta_formula, Formulas, Deltas, Normal),
    maplist(eval(S0, S), Normal, EvalNormal),
    maplist(eval_delta(S0, S), Deltas, EvalDelta),
    append(EvalNormal, EvalDelta, Eval),
    equal_keys(Keys, Formulas, S0, S),
    assert_step(N, S0, S, Eval, Ref).
compile_formulas(euler, Keys, Formulas, Ref, N, N1),
    current_prolog_flag(garp_eval_batch, true) =>
    N1 is N+1,
    step_state_dicts(Keys, S0, S),
    maplist(eval_batch(S0,S), Formulas, Eval),
    equal_keys(Keys, Formulas, S0, S),
    assert_step(N, S0, S, Eval, Ref).
compile_formulas(euler, Keys, Formulas, Ref, N, N1) =>
    N1 is N+1,
    step_state_dicts(Keys, S0, S),
    foldl(eval_seq(S0,S), Formulas, Eval, #{}, _),
    equal_keys(Keys, Formulas, S0, S),
    assert_step(N, S0, S, Eval, Ref).

%!  step_state_dicts(+Keys:list, -S0, -S) is det.
%
%   True when S0 and S are two dicts with the same keys as the left side
%   of each formula and unbound values.   The  generated stepping clause
%   links these two dicts using a set of arithmetic expressions.

step_state_dicts(Keys, S0, S) :-
    maplist(var_pair, Keys, Pairs),
    dict_pairs(S0, _, Pairs),
    dict_same_keys(S0, S).

var_pair(Key, Key-_).

equal_keys([], _, _, _).
equal_keys([H|T], Formulas, S0, S) :-
    memberchk(H-_, Formulas),
    !,
    equal_keys(T, Formulas, S0, S).
equal_keys([H|T], Formulas, S0, S) :-
    get_dict(H, S0, Var),
    get_dict(H, S, Var),
    equal_keys(T, Formulas, S0, S).

%!  assert_step(+N, +S0, +S, +Eval, -Ref) is det.
%
%   Generate the euler_step/2 clause. Ref is   the clause reference that
%   is used to clean up the clause.

assert_step(N, S0, S, Eval, Ref) :-
    comma_list(Body, Eval),
    assertz((euler_step(N, S0, S) :- Body), Ref),
    (   debugging(euler_step_clause)
    ->  \+ \+ ( name_vars(S0, S),
                numbervars(S0+S+Body, 0, _),
                format(user_error,
                       "euler_step(~d, ~n~11|~p, ~n~11|~p) :-~n",
                       [N, S0, S]),
                format_body(Body) )
    ;   true
    ),
    ieee_floats.

name_vars(S0, S) :-
    mapdict(name_var, S0, S).

name_var(Key, V0, V) :-
    (   key_var_name(Key, Name)
    ->  atom_concat(Name, 0, Name0),
        atom_concat(Name, 1, Name1),
        ignore(V0 = '$VAR'(Name0)),
        ignore(V = '$VAR'(Name1))
    ;   true
    ).

key_var_name(t, 'T') :-
    !.
key_var_name(Key, Name) :-
    quantity_label(Key, Label),
    atom_chars(Label, Chars),
    maplist(varname_char, Chars, [Cl|PlChars]),
    upcase_atom(Cl, Cu),
    atom_chars(Name, [Cu|PlChars]).

varname_char(Char, Char) :-
    char_type(Char, csym),
    !.
varname_char(_, '_').

format_body((A,B)) =>
    format(user_error, "    ~p,~n", [A]),
    format_body(B).
format_body(A) =>
    format(user_error, "    ~p.~n", [A]).

%!  eval(+S0, +S, +FormulaPair, -Eval) is det.
%
%   True when Eval is the body term to evaluate FormulaPair for updating
%   S based on S0.

eval(S0, S, Key-formula(Expr, Bindings), Eval) =>
    Eval = (Value is Expr),
    S0 >:< Bindings,
    get_dict(Key, S, Value).

eval_delta(S0, S, Key-formula(δ(Of), Bindings), Eval) =>
    get_dict(OfKey, Bindings, OfB),
    assertion(Of == OfB),
    get_dict(Key, S, D1),
    get_dict(OfKey, S, V1),
    get_dict(OfKey, S0, V0),
    Eval = ( D1 is V1-V0 ).

%!  eval_seq(+S0, +S, +Formula, -Eval, +IntermediateIn, -Intermediate)
%!           is det.
%
%   Sequential     evaluation     of      formulas.       Note      tbat
%   add_derivative_equations/2 guarantees that  a   derivative  equation
%   (δ(Q)) always follows the equation for Q.
%
%   @arg S0 is the step input state
%   @arg S is the step output state
%   @arg Formula is the formula to evaluate next
%   @arg Eval is its compiled version
%   @arg IntermediateIn contains the outputs of formulas evaluated
%   earlier in the step.

eval_seq(S0, S, Formula, Eval, I0, I) :-
    copy_term(Formula, Formula1),             % variables can be shared
    eval_seq_(S0, S, Formula1, Eval, I0, I).  % between formulas

eval_seq_(S0, S, Key-formula(δ(Of), Bindings), Eval, I0, I) =>
    I is I0.put(Key, D1),
    get_dict(OfKey, Bindings, OfB),
    assertion(Of == OfB),
    get_dict(Key, S, D1),
    get_dict(OfKey, S, V1),
    (   get_dict(OfKey, I0, V0)
    ->  true
    ;   get_dict(OfKey, S0, V0)
    ),
    Eval = ( D1 is V1-V0 ).
eval_seq_(S0, S, Key-formula(Expr, Bindings), Eval, I0, I) =>
    Eval = (Value is Expr),
    S1 = S0.put(I0),
    S1 >:< Bindings,
    get_dict(Key, S, Value),
    I = I0.put(Key, Value).

%!  eval_batch(+S0, +S, +Formula, -Eval) is det.
%
%   Batch evaluation of formulas. This implies   that every formula gets
%   its input value from the previous state rather than after evaluating
%   the previous formula.

:- det(eval_batch/4).
eval_batch(S0, S, Formula, Eval) :-
    copy_term(Formula, Formula1),
    eval_batch_(S0, S, Formula1, Eval).

eval_batch_(S0, S, Key-formula(δ(Of), Bindings), Eval) =>
    get_dict(OfKey, Bindings, OfB),
    assertion(Of == OfB),
    get_dict(Key, S, D1),
    get_dict(OfKey, S, V1),
    get_dict(OfKey, S0, V0),
    Eval = ( D1 is V1-V0 ).
eval_batch_(S0, S, Key-formula(Expr, Bindings), Eval) =>
    Eval = (Value is Expr),
    S0 >:< Bindings,
    get_dict(Key, S, Value).


%!  clean_formulas(+Ref)
%
%   Cleanup after simulation completes:  remove   the  compiled stepping
%   clause and reset arithmetic to default ISO Prolog mode.

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



%!  eval_d(+SubStep, +T, +DT, +H, +Y0, -K) is det.
%
%   K is the derivative of Formulas at T and Y0.
%
%   @arg DT is the name of the variable holding the _delta T_
%   @arg H is the interval we use.  This is irrelevant as we
%   divide by it again.

:- det(eval_d/6).
eval_d(N, T, DT, H, Y0, K) :-
    dict_pairs(Extra, _, [DT-H,t-T]),
    Y1 = Y0.put(Extra),
    run_euler_step(N, Y1, Y2),
    del_dict(DT, Y2, _, Y2a),
    del_dict(t, Y2a, _, Y2b),
    derivative_(Y0,Y2b,H,K).

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

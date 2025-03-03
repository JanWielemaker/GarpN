:- module(model,
          [ propose_model/3,            % +ModelId, -Equations, +Options
            is_placeholder/1,           % @Term
            is_placeholder/2,           % @Term, -Type
            default_nrels/1,            % -NRels:list
            is_expr/1                   % @Expr
          ]).
:- use_module(library(terms)).
:- use_module(library(lists)).
:- use_module(library(exceptions)).
:- use_module(library(apply)).
:- use_module(library(aggregate)).
:- use_module(library(option)).

:- use_module(map).
:- use_module(gsim).
:- use_module(identifiers).

/** <module> Propose a (partial) numeric model from Garp

*/

%!  propose_model(+Model, -Equations, +Options) is det.
%
%   Propose a numerical model based on the qualitative model indentified
%   by Model (an identifier).  The model is based on
%
%     - The qualitative relations except for _correspondence_ relations
%     - Exogenous input
%     - The initial state
%
%   @arg Model is the model identifier.
%   @arg Equations is a list of `X   := Expression` terms, where `X` are
%   ground Prolog terms of the the form Prop(Entity) or 'ΔProp'(Entity).
%   @arg Options supports
%     - mode(+Mode)
%       One of `quantities`, `derivatives` or `mixed`

propose_model(Model, Equations, Options) :-
    findall(QRel, (q_rel(Model, QRel), \+correspondence_rel(QRel)), QRels),
    findall(exogenous(Q,Class), q_exogenous(Model, Q, Class), Exos),
    append(QRels, Exos, Rels),
    valued_quantities(Model, VQs),
    qrel2nrel(Rels, VQs, NRels, Options),
    integrals(VQs, NRels, IRels, Options),
    init_nrels(Model, QRels, NRels, Init),    % Use input scenario
    default_nrels(DefNRels),                  % Defaults (time)
    append([NRels,IRels,DefNRels,Init], Eql0),
    simplify_model(Eql0, Eql1),
    id_mapping(Model, Mapping),
    foldsubterms(id_to_term(Mapping), Eql1, Eql2, [], ConstEql),
    append(Eql2, ConstEql, Equations1),
%   ModelOpts = [id_mapping(Mapping)|Options],
    ModelOpts = Options,
    add_model_init(Model, Equations1, Equations2, Formulas, ModelOpts),
    order_equations(Equations2, Formulas, Equations, ModelOpts).

qrel2nrel(QRels, VQs, NRels, Options) :-
    aggregate_all(min(NLeft, NRels-Left),
                  qrel2nrel(QRels, VQs, Left, NRels, NLeft, Options),
                  min(_, NRels-Left)),
    maplist(rel_unknown, Left).

%!  qrel2nrel(+QRels, +ValuedQuantities, -Left, -NRels, -NLeft,
%!            +Options) is multi.
%
%   True when NRels are the  numeric   relations  for QRels. Enumeration
%   stops if we find an element that   leaves  nothing unmapped. Is this
%   ok? I.e., can we have multiple meaningful mappings?
%
%   @arg Left are unmapped QRels elements.
%   @arg NLeft is the length of Left.

qrel2nrel(QRels, VQs, Left, NRels, NLeft, Options) :-
    mode_indicator(DQ, Options),
    qrel2nrel(DQ, QRels, VQs, Left, NRels),
    length(Left, NLeft),
    (   NLeft =:= 0
    ->  !
    ;   true
    ).

mode_indicator(DQ, Options), option(mode(quantities),  Options) => DQ = q.
mode_indicator(DQ, Options), option(mode(derivatives), Options) => DQ = d.
mode_indicator(DQ, Options), option(mode(mixed),       Options) => DQ = m.

%!  qrel2nrel(+DQ, +QRels, +VQs, -Left, -NRels) is nondet.
%
%   True when NRels  is  a  set   of  numerical  relations  covering the
%   qualitative  relation  set  QRels.  Left  are  QRels  that  are  not
%   processed. DQ describes the _mode_ as one of `q`, `d` or `m`. Steps:
%
%     1. Handle numerical relations (-,+,*,/). Ignore P+, P- and
%        exogenous relations pointing at the result node.
%     2. Handle nodes that have multiple P+ and P- dependencies
%        by creating a single expression.
%     3. Handle nodes that have multiple I+ or I- dependencies
%        by creating a single expression.
%     4. Map individual sub-graphs as defined by qrel_nrel/3.
%        This recognizes a sub-graph and removes it from the
%        remaining QRels set.
%
%   For the `mixed` model mode we shall
%
%     - If all affected quantities have a quantity space, used the `d`
%       mode.
%     - Otherwise use the `q` mode.  Add an integration relation
%       for all involved quantities with a quantity space.

qrel2nrel(DQ, QRels, VQs, Left, [NRel|NRels]) :- % Diff = A-B
    select(equal(min(A,B), Diff), QRels, QRels1),
    !,
    mkrel(DQ, VQs, Diff := A - B, NRel),
    exclude(is_prop(Diff), QRels1, QRels2),
    qrel2nrel(DQ, QRels2, VQs, Left, NRels).
qrel2nrel(DQ, QRels, VQs, Left, [NRel|NRels]) :- % Sum = A-B
    select(equal(plus(A,B), Sum), QRels, QRels1),
    !,
    mkrel(DQ, VQs,
          Sum := A + B,
          d(Sum) := d(A) + d(B),
          NRel),
    exclude(is_prop(Sum), QRels1, QRels2),
    qrel2nrel(DQ, QRels2, VQs, Left, NRels).
qrel2nrel(DQ, QRels, VQs, Left, [NRel|NRels]) :- % Mult = A*B
    select(equal(mult(A,B), Mult), QRels, QRels1),
    !,
    mkrel(DQ, VQs,
          Mult := A * B,
          d(Mult) := d(A) * d(B),
          NRel),
    exclude(is_prop(Mult), QRels1, QRels2),
    qrel2nrel(DQ, QRels2, VQs, Left, NRels).
qrel2nrel(DQ, QRels, VQs, Left, [NRel|NRels]) :- % Div = A/B
    select(equal(diw(A,B), Div), QRels, QRels1),
    !,
    mkrel(DQ, VQs,
          Div := A / B,
          d(Div) := d(A) / d(B),
          NRel),
    exclude(is_prop(Div), QRels1, QRels2),
    qrel2nrel(DQ, QRels2, VQs, Left, NRels).
qrel2nrel(DQ, QRels, VQs, Left, [NRel|NRels]) :- % multiple prop on a target
    select(Prob, QRels, QRels1),
    is_prop(Dep, Prob),
    partition(is_prop(Dep), QRels1, Props, QRels2),
    Props \== [],
    prop_nrel([Prob|Props], NRel0),
    mkrel(DQ, VQs, NRel0, NRel),
    qrel2nrel(DQ, QRels2, VQs, Left, NRels).
qrel2nrel(DQ, QRels, VQs, Left, [NRel|NRels]) :- % multiple integrals on target
    select(Integral, QRels, QRels1),
    is_inf_by(Dep, Integral),
    partition(is_inf_by(Dep), QRels1, Integrals, QRels2),
    Integrals \== [],
    inf_by_nrel(DQ, VQs, [Integral|Integrals], NRel),
    qrel2nrel(DQ, QRels2, VQs, Left, NRels).
qrel2nrel(DQ, QRels, VQs, Left, NRels) :-
    select(exogenous(Dep,Class), QRels, QRels1),
    !,
    exogenous_equation(Class, Dep, Rels),
    append(Rels, NRels1, NRels),
    qrel2nrel(DQ, QRels1, VQs,Left, NRels1).
qrel2nrel(DQ, QRels, VQs, Left, NRels) :-
    qrel_nrel(Qsubgraph, NQRels, DRels),
    select_graph(Qsubgraph, QRels, QRels1),
    !,
    mkrel(DQ, VQs, NQRels, DRels, Rels),
    append(Rels, NRels1, NRels),
    qrel2nrel(DQ, QRels1, VQs, Left, NRels1).
qrel2nrel(_DQ, Left, _VQs, Left, []).

mkrel(q, _VQs, NRel, _, NRel).
mkrel(d, _VQs, _, DRel, DRel).
mkrel(m, VQs, NRel, DRel, Rel) :-
    (   nrel_all_valued(NRel, VQs)
    ->  Rel = NRel
    ;   Rel = DRel
    ).

mkrel(q, _VQs, NRel, NRel).
mkrel(d, _VQs, NRel, DRel) :-
    drel(NRel, DRel).
mkrel(m, VQs, NRel, Rel) :-
    (   nrel_all_valued(NRel, VQs)
    ->  Rel = NRel
    ;   drel(NRel, Rel)
    ).

drel(A := B, Rel) =>
    Rel = (d(A) := DB),
    drel(B, DB).
drel(-A, Rel) =>
    Rel = -DA,
    drel(A, DA).
drel(A - B, Rel) =>
    Rel = DA - DB,
    drel(A, DA),
    drel(B, DB).
drel(A + B, Rel) =>
    Rel = DA + DB,
    drel(A, DA),
    drel(B, DB).
drel(A * B, Rel) =>
    Rel = DA * DB,
    drel(A, DA),
    drel(B, DB).
drel(c, C) =>
    C = c.
drel(c(N), C) =>
    C = c(N).
drel(Q, DQ) =>
    DQ = d(Q).

%!  nrel_all_valued(+NRel, +VQs) is semidet.
%
%   True when all quantities in a numerical relation have a value in the
%   qualitative model.
%
%   @arg NRel is a numeric equation or a list of these.

nrel_all_valued(NRels, VQs), is_list(NRels) =>
    maplist(nrel_all_valued_(VQs), NRels).
nrel_all_valued(NRel, VQs) =>
    nrel_all_valued_(VQs, NRel).

nrel_all_valued_(VQs, NRel) :-
    nrel_quantities(NRel, Qs),
    maplist(has_value(VQs), Qs).

has_value(VQs,  Q), memberchk(Q,VQs) => true.
has_value(_,    t) => true.
has_value(_, 'Δt') => true.
has_value(_,    _) => fail.

%!  nrel_quantities(+NRel, -Qs) is det.
%
%   Find all quantities used in a   numerical  relation (equation). Note
%   that we only need to deal with   functions  that may be generated by
%   the translation. The user may use any  known function, but this code
%   does not touch that.

nrel_quantities(NRel, Qs) :-
    phrase(nrel_quantities(NRel), Qs).

nrel_quantities(A:=B)   ==> nrel_quantities(A), nrel_quantities(B).
nrel_quantities(A+B)    ==> nrel_quantities(A), nrel_quantities(B).
nrel_quantities(A-B)    ==> nrel_quantities(A), nrel_quantities(B).
nrel_quantities(A*B)    ==> nrel_quantities(A), nrel_quantities(B).
nrel_quantities(A/B)    ==> nrel_quantities(A), nrel_quantities(B).
nrel_quantities(A^B)    ==> nrel_quantities(A), nrel_quantities(B).
nrel_quantities(-(A))   ==> nrel_quantities(A).
nrel_quantities(sin(A)) ==> nrel_quantities(A).  % exogenous
nrel_quantities(random) ==> [].                  % exogenous
nrel_quantities(c)      ==> [].
nrel_quantities(c(_))   ==> [].
nrel_quantities(Q)      ==> [Q].

%!  qrel_nrel(+QRel:list, -NRel:list, -DRel:list) is multi.
%
%   Map  a  set  of  qualitative  relations  to  a  set  of  qualitative
%   relations. The rules are ordered to map larger submodels first.
%
%   @tbd include quantity spaces into the picture

qrel_nrel([inf_pos_by(I,D)],
          [I := I + D*'Δt'],
          [d(I) := D*'Δt']).
qrel_nrel([inf_neg_by(I,D)],
          [I := I - D*'Δt'],
          [d(I) := -D*'Δt']).
qrel_nrel([prop_pos(Dep,Infl)],
          [Dep := Dep + c*Infl],
          [d(Dep) := c*d(Infl)]).
qrel_nrel([prop_neg(Dep,Infl)],
          [Dep := Dep - c*Infl],
          [d(Dep) := -(c*d(Infl))]).
/*
qrel_nrel([exogenous(Dep,exogenous_steady)],
          [Dep := c],
          [Dep := c, d(Dep) := 0]).
*/

%!  correspondence_rel(?Rel)
%
%   True  when  Rel  is  a  _correspondence_  relation.  Correspondences
%   typically cannot be used to create   equations.  They should be used
%   during the simulation to verify all constraints are satisfied.

correspondence_rel(dir_q_correspondence(_,_)).
correspondence_rel(dir_v_correspondence(_,_)).
correspondence_rel(q_correspondence(_,_)).
correspondence_rel(v_correspondence(_,_,_,_)).
correspondence_rel(equal(Left,_)) :- \+ is_expr(Left).
correspondence_rel(smaller(Left,_)) :- \+ is_expr(Left).
correspondence_rel(greater(Left,_)) :- \+ is_expr(Left).

%!  is_expr(@Expr) is semidet.
%
%   True when Expr is an arithmetic expression.

is_expr(min(_,_)) => true.                  % not a correspondence
is_expr(plus(_,_)) => true.
is_expr(mult(_,_)) => true.
is_expr(diw(_,_)) => true.
is_expr(_) => false.

is_prop(Dep, prop_pos(Dep,_)).
is_prop(Dep, prop_neg(Dep,_)).
is_prop(Dep, exogenous(Dep,_)).

prop_nrel(Props, Dep := Expr) :-
    Props = [H|_],
    is_prop(Dep, H),
    maplist(one_prop, Props, Parts),
    sum_expressions(Parts, Sum),
    join_sum(Sum, Dep, Expr).

sum_expressions(Parts, Sum) :-
    partition(is_neg, Parts, NegParts, PosParts),
    append(NegParts, PosParts, NegFirst),
    seq_to_sum(NegFirst, Sum).

is_neg(-(_)).

seq_to_sum([One], Sum) =>
    Sum = One.
seq_to_sum([H|T], Sum) =>
    seq_to_sum(T, Sum0),
    join_sum(H, Sum0, Sum).

one_prop(prop_pos(_, Infl),   Expr) => Expr = c*Infl.
one_prop(prop_neg(_, Infl),   Expr) => Expr = -(c*Infl).

join_sum(-(Expr), Sum0, Sum) => Sum = Sum0-Expr.
join_sum(Expr, Sum0, Sum)    => Sum = Sum0+Expr.

%!  is_inf_by(?Dep, +Relation) is semidet.

is_inf_by(Dep, inf_pos_by(Dep, _)).
is_inf_by(Dep, inf_neg_by(Dep, _)).

inf_by_nrel(DQ, VQs, Integrals, Dep := Expr) :-
    Integrals = [H|_],
    is_inf_by(Dep, H),
    maplist(one_inf_by, Integrals, Parts),
    sum_expressions(Parts, Sum),
    (   DQ == d,
        nrel_all_valued(Dep+Sum, VQs)
    ->  Expr = Sum
    ;   sum_expressions([Sum*'Δt', Dep], Expr)
    ).

one_inf_by(inf_pos_by(_, D), Expr) => Expr = c(1)*D.
one_inf_by(inf_neg_by(_, D), Expr) => Expr = -(c(1)*D).

%!  integrals(+ValuedQuantities, +NRels, -IRels, +Options) is det.
%
%   Create integration relations for all quantifies  that have a defined
%   quantity space.

integrals(_VQs, _, IRels, Options),
    option(mode(quantities), Options) =>
    IRels = [].
integrals(VQs, _NRels, IRels, Options),
    option(mode(derivatives), Options) =>
    maplist(integral, VQs, IRels).
integrals(VQs, NRels, IRels, _Options) =>
    exclude(has_value_equation(NRels), VQs, VQs2),
    maplist(integral, VQs2, IRels).

has_value_equation(NRels, Q) :-
    memberchk((Q:=_), NRels).

integral(Q, Q := Q + d(Q)).

%!  valued_quantities(+Model, -Qs:list(atom)) is det.
%
%   True when Qs is a list of quantity identifiers for quantities with a
%   value.

valued_quantities(Model, Qs) :-
    findall(Q, qspace_with_points(Model, Q), Qs).

qspace_with_points(Model, Q) :-
    m_qspace(Model, Q, _QSpaceName, Values),
    memberchk(point(_), Values).

%!  default_nrels(-NRels:list) is det.

default_nrels([ t := t + 'Δt',
                t := 0,
                'Δt' := placeholder(constant,0.1)
              ]).

%!  id_to_term(+IdMapping, +IdTerm, -Term, +S0, -S) is det.

id_to_term(Mapping, c, Term, S0, S) =>
    id_to_term(Mapping, c(_), Term, S0, S).
id_to_term(_Mapping, c(Value), Term, S0, S) =>
    length(S0, N0),
    N is N0+1,
    atom_concat(c,N,Term),
    S = [(Term := placeholder(constant,Value))|S0].
id_to_term(Mapping, d(Id), Term, S0, S), atom(Id) =>
    S = S0,
    Term0 = Mapping.get(Id,Id),
    term_derivative(Term0, Term).
id_to_term(Mapping, Id, Term, S0, S), atom(Id) =>
    S = S0,
    Term = Mapping.get(Id,Id).
id_to_term(_Mapping, _Id, _Term, _S0, _S) =>
    fail.

%!  init_nrels(+Model, +QRels, +NRels, -Init) is det.
%
%   Use the input state (scenario)   to create initialization equations.
%   We create an initialization for each   quantity defined in the input
%   state, unless the quantity is defined by an exogenous steady input.
%
%   We may propagate v_correspondence(Q1,  V1,   Q2,  V2) relations from
%   QRels to instantiate other quantities.
%
%   @tbd: This assumes only initial  values   on  quantities, __not__ on
%   derivatives.

init_nrels(Model, _QRels, NRels, Init) :-
    q_input_state(Model, Input),
    dict_pairs(Input, _, Pairs),
    convlist(init_nrel(NRels), Pairs, Inits),
    append(Inits, Init).

init_nrel(NRels, Id-_, _), memberchk(Id:=c, NRels) =>
    fail.
init_nrel(_NRels, Id-d(Q,D1,_,_), Init) =>
    phrase(init_nrel(Id, Q, D1), Init).

init_nrel(Id, Q, D) -->
    init_v(Id, Q),
    init_d(Id, D).

init_v(Id, point(zero))  ==> [Id := 0].
init_v(Id, Q), nonvar(Q) ==> [Id := placeholder(init, _)].
init_v(_, _)             ==> [].
init_d(Id, zero)         ==> [d(Id) := 0].
init_d(Id, D), nonvar(D) ==> [d(Id) := placeholder(init, _)].
init_d(_, _)             ==> [].

%!  exogenous_equation(+Class, ?Q, -Equations) is det.

exogenous_equation(exogenous_steady, Q, NRel) =>
    NRel = [Q := c + c(0)*d(t)].
exogenous_equation(exogenous_increasing, Q, NRel) =>
    NRel = [Q := c+(c*t)].
exogenous_equation(exogenous_decreasing, Q, NRel) =>
    NRel = [Q := c-(c*t)].
exogenous_equation(exogenous_sinus, Q, NRel) =>
    NRel = [Q := (c*sin(c+c*t))].
exogenous_equation(exogenous_pos_parabola, Q, NRel) =>
    NRel = [Q := (c-c*t^2)].
exogenous_equation(exogenous_pos_parabola, Q, NRel) =>
    NRel = [Q := (c+c*t^2)].
exogenous_equation(exogenous_free, Q, NRel) =>
    NRel = [Q := (c*random)].

%!  add_model_init(+ModelId, +EquationsIn, -EquationsOut, -Formulas,
%!                 +Options) is det.
%
%   Add missing initializations to the equations.

:- exception_type(model_error, model_error(invalid(_))).
:- exception_type(model_error, model_error(no_initial_values(_))).
:- exception_type(model_error, model_error(no_time_formulas)).

add_model_init(Model, Eq0, Eq, Formulas, Options) :-
    catch(read_model(terms(Eq0), Formulas, _Constants, _State,
                     [ allow_placeholders(true)
                     | Options
                     ]),
          model_error, Ball, true),
    (   var(Ball)
    ->  Eq = Eq0
    ;   init_from_error(Ball, Model, Init),
        append(Eq0, Init, Eq1),
        (   Init == []
        ->  Eq = Eq1
        ;   add_model_init(Model, Eq1, Eq, Formulas, Options)
        )
    ).

init_from_error(model_error(invalid(Invalid)),
                Model, Init) =>
    convlist(constant_quantity(Model), Invalid, Init).
init_from_error(model_error(no_initial_values(UnResolved)),
                Model, Init) =>
    convlist(init_quantity(Model), UnResolved, Init).

constant_quantity(_Model, Q, Init) :-
    Init = (Q := placeholder(constant, _)).

init_quantity(Model, Q, Init) :-
    initial_value(Q, Model, Value),
    Init = (Q := Value).

%!  initial_value(+Q, +Model, -Value)
%
%   True when Value is the initial (input) value for Q in Model. This is
%   a placeholder, unless the qualitative value is `zero`.

initial_value(Q, Model, Value) :-
    id_mapping(Model, Mapping),
    once(get_dict(Id, Mapping, Q)),
    q_input_state(Model, Input),
    zero = Input.get(Id),
    !,
    Value = 0.
initial_value(_Q, _Model, placeholder(init,_)).

select_graph([], Set, Set).
select_graph([H|T], Set0, Set) :-
    select(H, Set0, Set1),
    select_graph(T, Set1, Set).


%!  order_equations(+Equations, +Formulas, -Ordered, +Options) is det.
%
%   Order and group the equations.  Ordered is a dict, holding
%
%     - model:    ModelEquationLayers
%       Here, ModelEquationLayers is a list of layers.  Each layer
%       is a list of equations that belong to this layer.  The
%       layers are based on partial ordering of the equations.
%     - time:     Time relations
%     - constant: ConstantEquations
%     - init:     InitializationEquations

order_equations(Equations, Formulas, Ordered, Options) :-
    map_list_to_pairs(equation_class, Equations, Classified),
    keysort(Classified, ByClass),
    group_pairs_by_key(ByClass, Grouped),
    select(model-ModelEq, Grouped, GroupedRest),
    map_list_to_pairs(equation_term_id(Options), ModelEq, IdPaired),
    order_formulas(Formulas, Layers),
    maplist(layer_equations(IdPaired), Layers, LayeredEquations0),
    exclude(==([]), LayeredEquations0, LayeredEquations),
    dict_pairs(Ordered, #, [model-LayeredEquations|GroupedRest]).

:- det(equation_term_id/3).
equation_term_id(Options, (QTerm := _), QId) =>
    option(id_mapping(IdMapping), Options, #{}),
    term_key(QTerm, QId, IdMapping).

equation_class(t:=_, Class)                       => Class = time.
equation_class(_:=placeholder(constant,_), Class) => Class = constant.
equation_class(_:=placeholder(init,_), Class)     => Class = init.
equation_class(_:=Num, Class), number(Num)        => Class = init.
equation_class(_, Class)                          => Class = model.

layer_equations(IdPaired, IdOrder, Equations) :-
    convlist(select_eq(IdPaired), IdOrder, Equations).

select_eq(Pairs, QId, Equation) :-
    memberchk(QId-Equation, Pairs).

%!  simplify_model(+Eql0, -Eql) is det.
%
%   The task of this is to simplify the  model, in particular try to get
%   rid of constants that are zero.  Note   that  any  `c` is a _unique_
%   constant.

simplify_model(Eql0, Eql) :-
    remove_constant_zero(Eql0, Eql1),
    simplify_zero_init(Eql1, Eql2),
    exclude(identity_rel, Eql2, Eql).

simplify_zero_init(Eql0, Eql) :-
    simplify_init_graph(In, Out),
    select_graph(In, Eql0, Eql1),
    !,
    append(Out, Eql1, Eql2),
    simplify_zero_init(Eql2, Eql).
simplify_zero_init(Eql, Eql).

simplify_init_graph([ Q := c + X*t,
                      Q := 0
                    ],
                    [ Q := X*t,
                      Q := 0
                    ]).
simplify_init_graph([ Q := c - X*t,
                      Q := 0
                    ],
                    [ Q := X*t,
                      Q := 0
                    ]).

remove_constant_zero(Eql0, Eql) :-
    select(Q := 0, Eql0, Eql1),
    \+ memberchk(Q := _, Eql1),
    !,
    mapsubterms(replace(Q, 0), Eql1, Eql2),
    maplist(simplify_rel, Eql2, Eql3),
    remove_constant_zero(Eql3, Eql).
remove_constant_zero(Eql, Eql).

replace(F, T, F, T).

simplify_rel(Q := Expr0, Q := Expr) :-
    simplify_expr(Expr0, Expr).

simplify_expr(A+0, Expr) => Expr = A.
simplify_expr(A-0, Expr) => Expr = A.
simplify_expr(0+A, Expr) => Expr = A.
simplify_expr(0-A, Expr) => Expr = A.
simplify_expr(_*0, Expr) => Expr = 0.
simplify_expr(0*_, Expr) => Expr = 0.
simplify_expr(0/_, Expr) => Expr = 0.
simplify_expr(Exp, Expr) => Expr = Exp.

identity_rel(Q := Q).

%!  is_placeholder(@Term) is semidet.
%!  is_placeholder(@Term, -Type) is semidet.
%
%   True when Term is a placeholder (of Type).

is_placeholder(Term) :-
    is_placeholder(Term, _).

is_placeholder(placeholder(Id, _Value), Type) => Type = Id.
is_placeholder(_, _) => false.

		 /*******************************
		 *           MESSAGES		*
		 *******************************/

rel_unknown(Rel) :-
    print_message(warning, unknown_relation(Rel)).


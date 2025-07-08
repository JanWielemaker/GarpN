:- module(model,
          [ propose_model/3,            % +ModelId, -Equations, +Options
            is_placeholder/1,           % @Term
            is_placeholder/2,           % @Term, -Type
            default_nrels/2,            % -NRels:list, +Options
            is_expr/1,                  % @Expr
            correspondences/2           % +ModelId, -Correspondences
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
:- use_module(library(pairs)).

/** <module> Propose a (partial) numeric model from Garp

*/

%!  propose_model(+ModelId, -Equations, +Options) is det.
%
%   Propose a numerical model based on the qualitative model indentified
%   by Model (an identifier).  The model is based on
%
%     - The qualitative relations except for _correspondence_ relations
%     - Exogenous input
%     - The initial state
%
%   @arg ModelId is the model identifier.
%   @arg Equations is a list of `X   := Expression` terms, where `X` are
%   ground Prolog terms of the the form Prop(Entity) or 'ΔProp'(Entity).
%   @arg Options supports
%     - mode(+Mode)
%       One of `quantities`, `derivatives` or `mixed`
%     - old_model(+Model)
%     - old_qspaces(+QSpaces)
%       These two options provide the current qualitative model.  They
%       are used to preserve as much as possible of the current
%       model.

propose_model(Model, Equations, Options0) :-
    opt_id_mapping(Model, Options0, Options1),
    Options = [model(Model)|Options1],
    findall(QRel, (q_rel(Model, QRel), \+correspondence_rel(QRel)), QRels),
    findall(exogenous(Q,Class), q_exogenous(Model, Q, Class), Exos),
    append(QRels, Exos, Rels),
    valued_quantities(Model, VQs),
    qrel2nrel(Rels, VQs, NRels, Options),
    integrals(VQs, NRels, IRels, Options),
    init_nrels(Model, QRels, NRels, Init, Options), % Use input scenario
    default_nrels(DefNRels, Options),		    % Defaults (time)
    append([NRels,IRels,DefNRels,Init], Eql0),
    simplify_model(Eql0, Eql1),
    option(id_mapping(Mapping), Options),
    foldsubterms(id_to_term(Mapping), Eql1, Eql2, [], ConstEql),
    preserve_constants(ConstEql, Eql2, Options),
    append(Eql2, ConstEql, Equations1),
    add_model_init(Model, Equations1, Equations2, Formulas, Options),
    order_equations(Equations2, Formulas, Equations, Options).

opt_id_mapping(_, Options0, Options) :-
    option(id_mapping(_), Options0),
    !,
    Options = Options0.
opt_id_mapping(Model, Options0, Options) :-
    id_mapping(Model, IdMapping),
    !,
    Options = [id_mapping(IdMapping)|Options0].
opt_id_mapping(_, Options, Options).

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
%   Options processed:
%
%     - mode(+Mode)
%       One of `quantities`, `derivatives` or `mixed`
%     - model(+ModelId)
%       Identifier for the model being converted
%
%   @arg Left are unmapped QRels elements.
%   @arg NLeft is the length of Left.

qrel2nrel(QRels, VQs, Left, NRels, NLeft, Options) :-
    mode_indicator(DQ, Options),
    dq_qrel2nrel(DQ, QRels, VQs, Left, NRels, Options),
    length(Left, NLeft),
    (   NLeft =:= 0
    ->  !
    ;   true
    ).

mode_indicator(DQ, Options), option(mode(quantities),  Options) => DQ = q.
mode_indicator(DQ, Options), option(mode(derivatives), Options) => DQ = d.
mode_indicator(DQ, Options), option(mode(mixed),       Options) => DQ = m.

%!  dq_qrel2nrel(+DQ, +QRels, +VQs, -Left, -NRels, +Options) is nondet.
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

dq_qrel2nrel(DQ, QRels, VQs, Left, [NRel|NRels], Options) :- % Diff = A-B
    select(equal(min(A,B), Diff), QRels, QRels1),
    !,
    mkrel(DQ, VQs, Diff := A - B, NRel),
    exclude(is_prop(Diff), QRels1, QRels2),
    dq_qrel2nrel(DQ, QRels2, VQs, Left, NRels, Options).
dq_qrel2nrel(DQ, QRels, VQs, Left, [NRel|NRels], Options) :- % Sum = A-B
    select(equal(plus(A,B), Sum), QRels, QRels1),
    !,
    mkrel(DQ, VQs,
          Sum := A + B,
          d(Sum) := d(A) + d(B),
          NRel),
    exclude(is_prop(Sum), QRels1, QRels2),
    dq_qrel2nrel(DQ, QRels2, VQs, Left, NRels, Options).
dq_qrel2nrel(DQ, QRels, VQs, Left, [NRel|NRels], Options) :- % Mult = A*B
    select(equal(mult(A,B), Mult), QRels, QRels1),
    !,
    mkrel(DQ, VQs,
          Mult := A * B,
          d(Mult) := d(A) * d(B),
          NRel),
    exclude(is_prop(Mult), QRels1, QRels2),
    dq_qrel2nrel(DQ, QRels2, VQs, Left, NRels, Options).
dq_qrel2nrel(DQ, QRels, VQs, Left, [NRel|NRels], Options) :- % Div = A/B
    select(equal(diw(A,B), Div), QRels, QRels1),
    !,
    mkrel(DQ, VQs,
          Div := A / B,
          d(Div) := d(A) / d(B),
          NRel),
    exclude(is_prop(Div), QRels1, QRels2),
    dq_qrel2nrel(DQ, QRels2, VQs, Left, NRels, Options).
dq_qrel2nrel(DQ, QRels, VQs, Left, [NRel|NRels], Options) :- % one or more prop on a target
    select(Prob, QRels, QRels1),
    is_prop(Dep, prop, Prob),
    partition(is_prop(Dep), QRels1, Props, QRels2),
    prop_nrel(DQ, [Prob|Props], VQs, NRel, Options),
    dq_qrel2nrel(DQ, QRels2, VQs, Left, NRels, Options).
dq_qrel2nrel(DQ, QRels, VQs, Left, [NRel|NRels], Options) :- % multiple integrals on target
    select(Integral, QRels, QRels1),
    is_inf_by(Dep, Integral),
    partition(is_inf_by(Dep), QRels1, Integrals, QRels2),
    Integrals \== [],
    inf_by_nrel(DQ, VQs, [Integral|Integrals], NRel),
    dq_qrel2nrel(DQ, QRels2, VQs, Left, NRels, Options).
dq_qrel2nrel(DQ, QRels, VQs, Left, NRels, Options) :-
    select(exogenous(Dep,Class), QRels, QRels1),
    !,
    exogenous_equation(Class, Dep, Rels),
    append(Rels, NRels1, NRels),
    dq_qrel2nrel(DQ, QRels1, VQs,Left, NRels1, Options).
dq_qrel2nrel(DQ, QRels, VQs, Left, NRels, Options) :-
    qrel_nrel(Qsubgraph, NQRels, DRels),
    select_graph(Qsubgraph, QRels, QRels1),
    !,
    mkrel(DQ, VQs, NQRels, DRels, Rels),
    append(Rels, NRels1, NRels),
    dq_qrel2nrel(DQ, QRels1, VQs, Left, NRels1, Options).
dq_qrel2nrel(_DQ, Left, _VQs, Left, [], _Options).

%!  select_graph(+SubGraph, +Graph, -Rest) is nondet.
%
%   Select a subgraph from Graph

select_graph([], Set, Set).
select_graph([H|T], Set0, Set) :-
    select(H, Set0, Set1),
    select_graph(T, Set1, Set).

%!  mkrel(+DQ, +VQs, +NRel, +DRel, -Rel) is det.
%!  mkrel(+DQ, +VQs, +NRel, -Rel) is det.
%
%   Given the mode (DQ), the  set  of   quantities  with  a value in the
%   qualitative model (VQs), select either the  quantity relation or the
%   derivative variant.
%
%   The mkrel/4 is similar, but deduces the   DRel from VRel by symbolic
%   derivation.

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

%!  correspondences(+ModelId, -Correspondences:list) is det.
%
%   True when Correspondences is a list   of correspondence relations in
%   ModelId.

correspondences(ModelId, Correspondences) :-
    findall(Corr, (q_rel(ModelId, Corr), correspondence_rel(Corr)), Correspondences).

%!  correspondence_rel(?Rel)
%
%   True  when  Rel  is  a  _correspondence_  relation.  Correspondences
%   typically cannot be used to create   equations.  They should be used
%   during the simulation to verify all constraints are satisfied.

correspondence_rel(dir_q_correspondence(_,_)).
correspondence_rel(dir_mirror_q_correspondence(_,_)).
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

is_prop(Dep, QRel) :-
    is_prop(Dep, _, QRel).

is_prop(Dep, prop,      prop_pos(Dep,_From)).
is_prop(Dep, prop,      prop_neg(Dep,_From)).
is_prop(Dep, exogenous, exogenous(Dep,_Class)).

%!  prop_nrel(+DQ, +Props, +VQs, -NRel, +Options) is det.
%
%   Generate a numeric relation NRel that handles   all P relations to a
%   single dependency.

prop_nrel(DQ0, Props, VQs, Dep := Expr, Options) :-
    option(model(Model), Options, engine),
    q_input_state(Model, Input),
    prop_dq_mode(DQ0, Props, VQs, DQ),
    Props = [H|_],
    is_prop(Dep, H),
    maplist(one_prop_r(DQ, [input_state(Input)|Options]), Props, Parts),
    sum_expressions(Parts, Sum),
    join_sum(Sum, Dep, Expr).

one_prop_r(DQ, Options, Prop, Expr) :-
    one_prop(DQ, Prop, Expr, Options).

%!  prop_dq_mode(+DQM, +Props, +VQs, -DQ) is det.
%
%   Find the translation mode for Props.   As  our conversion depends on
%   this, we must do this upfront, rather than relying on mkrel/4.

prop_dq_mode(d, _, _, d).
prop_dq_mode(q, _, _, q).
prop_dq_mode(m, Props, VQs, Mode) :-
    include(is_prop(_,prop), Props, RealProps),
    maplist(arg(2), RealProps, Infs),
    (   maplist(has_value(VQs), Infs)
    ->  Mode = q
    ;   Mode = d
    ).

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

%!  one_prop(+DQ, +Prop, -Relation, +Options)
%
%   Decide on the formula for translating Prop. The translation depends
%   on
%
%     - The translation mode (d/q)
%     - The value space on both sides (if known)
%     - The initial value on both sides (if known)
%
%   Options processed:
%
%     - input_state(+InputState)
%       Dict for _Q_ -> d(V,D1,D2,D3) initial values
%     - model(+ModelId)
%       Model from which to extract the quantity spaces

one_prop(q, prop_pos(_, Infl), Expr, _Options) => Expr = Expr + c*Infl.
one_prop(q, prop_neg(_, Infl), Expr, _Options) => Expr = Expr - c*Infl.
one_prop(d, prop_pos(_, Infl), Expr, _Options) => d(Expr) = c*d(Infl).
one_prop(d, prop_neg(_, Infl), Expr, _Options) => d(Expr) = -(c*d(Infl)).

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

%!  default_nrels(-NRels:list, +Options) is det.

default_nrels([ t := t + 'Δt',
                t := 0,
                'Δt' := placeholder(constant,Value)
              ], Options) :-
    (   option(old_model(Equations), Options),
        memberchk('Δt' := Value, Equations)
    ->  true
    ;   Value = 0.1
    ).

%!  id_to_term(+IdMapping, +IdTerm, -Term, +S0, -S) is det.
%
%   Rewrite  the  model  from  using  internal   atomic  keys  from  the
%   qualitative model to terms and map  app   `c`  and c(Value) terms to
%   unique constants.

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

%!  init_nrels(+Model, +QRels, +NRels, -Init, +Options) is det.
%
%   Use the input state (scenario)   to create initialization equations.
%   We create an initialization for each   quantity defined in the input
%   state, unless the quantity is defined by an exogenous steady input.
%
%   We may propagate v_correspondence(Q1,  V1,   Q2,  V2) relations from
%   QRels to instantiate other quantities.

init_nrels(Model, _QRels, NRels, Init, Options) :-
    q_input_state(Model, Input),
    dict_pairs(Input, _, Pairs),
    convlist(init_nrel(NRels, Options), Pairs, Inits),
    append(Inits, Init).

init_nrel(NRels, _, Id-_, _),
    memberchk(Id:=Expr, NRels),
    noinit_expression(Expr) =>
    fail.
init_nrel(_NRels, Options, Id-d(Q,D1,_,_), Init) =>
    phrase(init_nrel(Id, Q, D1, Options), Init).

noinit_expression(A+B)  => noinit_expression(A), noinit_expression(B).
noinit_expression(A-B)  => noinit_expression(A), noinit_expression(B).
noinit_expression(-A)   => noinit_expression(A).
noinit_expression(A*B)  => noinit_expression(A), noinit_expression(B).
noinit_expression(A/B)  => noinit_expression(A), noinit_expression(B).
noinit_expression(c)    => true.
noinit_expression(c(_)) => true.
noinit_expression(d(t)) => true.
noinit_expression(t)    => true.
noinit_expression(placeholder(_,_)) => true.
noinit_expression(_)    => fail.

%!  init_nrel(+Id, +QV, +D, +Options)// is det.
%
%   Create an initialization relation for Id  (an atom) that has initial
%   value QV (a point or  interval  in   a  quantity  space) and initial
%   derivative D (neg,zero,plus). This first uses the qualitative model.
%   When unsuccessful, we try  the  old   model  to  preserve a possible
%   value.

init_nrel(Id, Q, D, Options) -->
    init_v(Id, Q, Options),
    init_d(Id, D, Options).

init_v(Id, point(zero), _)     ==> [Id := 0].
init_v(Id, Q, Opts), nonvar(Q) ==> init_placeholder(Id, Opts).
init_v(_, _, _)                ==> [].
init_d(Id, zero, _)            ==> [d(Id) := 0].
init_d(Id, D, Opts), nonvar(D) ==> init_placeholder(d(Id), Opts).
init_d(_, _, _)                ==> [].

init_placeholder(d(Id), Opts) -->
    { option(id_mapping(IdMapping), Opts),
      option(old_model(_Equations), Opts),
      !,
      term_key(Q, Id, IdMapping),
      term_derivative(Q, DQ),
      preserve_init(DQ, Value, Opts)
    },
    [ Id := placeholder(init, Value)].
init_placeholder(Id, Opts) -->
    { option(id_mapping(IdMapping), Opts),
      option(old_model(_Equations), Opts),
      !,
      term_key(Q, Id, IdMapping),
      preserve_init(Q, Value, Opts)
    },
    [ Id := placeholder(init, Value)].
init_placeholder(Id, _Opts) -->
    [ Id := placeholder(init, _)].

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
    ;   init_from_error(Ball, Model, Init, Eq0, Options),
        append(Eq0, Init, Eq1),
        (   Init == []
        ->  Eq = Eq1
        ;   add_model_init(Model, Eq1, Eq, Formulas, Options)
        )
    ).

init_from_error(model_error(invalid(Invalid)),
                Model, Init, Equations, Options) =>
    convlist(constant_quantity(Model, Equations, Options), Invalid, Init).
init_from_error(model_error(no_initial_values(UnResolved)),
                Model, Init, _Equations, Options) =>
    convlist(init_quantity(Model, Options), UnResolved, Init).

%!  constant_quantity(+ModelId, +Options, +Equations, +Q, -Init) is
%!                    det.

constant_quantity(_Model, _Options, _Equations, Q, Init) :-
    Init = (Q := placeholder(constant, _)).

init_quantity(Model, Options, Q, Init) :-
    initial_value(Q, Model, Value, Options),
    Init = (Q := Value).

%!  initial_value(+Q, +Model, -Value, +Options)
%
%   True when Value is the initial (input) value for Q in Model. This is
%   a placeholder, unless the qualitative value is  `zero` or we have an
%   old model with a numeric expression.

initial_value(Q, Model, Value, _Options) :-
    id_mapping(Model, Mapping),
    once(get_dict(Id, Mapping, Q)),
    q_input_state(Model, Input),
    zero = Input.get(Id),
    !,
    Value = 0.
initial_value(Q, _Model, placeholder(init,Value), Options) :-
    preserve_init(Q, Value, Options).

                /*******************************
                *         PRESERVATION         *
                *******************************/

%!  preserve_constants(+ConstantExpressions, +Equations, +Options) is det.
%
%   Try to preserve as much as possible from the old constants.
%
%   @arg CostantExpressions is a list of `cN := placeholder(constant,V)`,
%   where V is often unbound.  That is the value we try to bind.

preserve_constants(ConstEq, Equations, Options) :-
    option(old_model(OldEquations), Options),
    !,
    maplist(preserve_constant(Equations, OldEquations), ConstEq).
preserve_constants(_, _, _).

preserve_constant(Equations, OldEquations, C := placeholder(constant, V)) :-
    member(OldC := V, OldEquations),
    numeric_expression(V),
    similar_relation(C, Equations, OldC, OldEquations),
    !.
preserve_constant(_, _, _).

similar_relation(C1, Eql1, C2, Eql2) :-
    member(Eq1, Eql1),
    c_rel(C1, Eq1, Rel),
    member(Eq2, Eql2),
    c_rel(C2, Eq2, Rel),
    !.

c_rel(C, Q := Expr, rel(Q,Pos)) :-
    ground(Expr),
    c_pos(C, Expr, Pos).

c_pos(C, C, C).
c_pos(C, -A, X)    :- c_pos(C, A, X0), neg(X0, X).
c_pos(C, A*B, X*B) :- c_pos(C, A, X).
c_pos(C, A*B, X*A) :- c_pos(C, B, X).
c_pos(C, A+_, X)   :- c_pos(C, A, X).
c_pos(C, _+B, X)   :- c_pos(C, B, X).
c_pos(C, A-_, X)   :- c_pos(C, A, X).
c_pos(C, _-B, X)   :- c_pos(C, B, X0), neg(X0, X).

neg(-X, X).
neg(X, -X).

%!  preserve_init(+Q, -Value, +Options) is det.
%
%   Try   to   preserve   the   initialization     for    Q   from   the
%   old_model(Equations)  option.  On  success,  unify    Value  with  a
%   numerical expression. On failure, Value is left unbound.

preserve_init(Q, Value, Options) :-
    option(old_model(OldEquations), Options),
    member(Q := Right, OldEquations),
    numeric_expression(Right),
    !,
    Value = Right.
preserve_init(_, _, _).

%!  numeric_expression(@Expr) is semidet.
%
%   True when Expr is an  expression   that  only holds numbers.

numeric_expression(Expr) :-
    catch(_ is Expr, error(_,_), fail).


                /*******************************
                *      ORGANISE EQUATIONS      *
                *******************************/

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
    selectchk(model-ModelEq, Grouped, GroupedRest),
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


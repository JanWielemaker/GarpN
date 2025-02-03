:- module(model,
          [ init_model/3,               % +Model, -Equations, +Options
            is_placeholder/1,           % @Term
            is_placeholder/2,           % @Term, -Type
            default_nrels/1             % -NRels:list
          ]).
:- use_module(library(terms)).
:- use_module(library(lists)).
:- use_module(library(exceptions)).

:- use_module(map).
:- use_module(gsim).
:- use_module(library(apply)).
:- use_module(library(aggregate)).
:- use_module(library(option)).

/** <module> Propose a (partial) numeric model from Garp

*/

%!  init_model(+Model, -Equations, +Options) is det.

init_model(Model, Equations, Options) :-
    findall(QRel, q_rel(Model, QRel), QRels), % Use relations
    findall(exogenous(Q,Class), q_exogenous(Model, Q, Class), Exos),
    append(QRels, Exos, Rels),
    qrel2nrel(Rels, NRels, Options),
    intergrals(Model, IRels, Options),
    init_nrels(Model, NRels, Init),           % Use input scenario
    default_nrels(DefNRels),                  % Defaults (time)
    append([NRels,IRels,DefNRels,Init], Eql0),
    simplify_model(Eql0, Eql1),
    id_mapping(Model, Mapping),
    foldsubterms(id_to_term(Mapping), Eql1, Eql2, [], ConstEql),
    append(Eql2, ConstEql, Equations1),
    add_model_init(Model, Equations1, Equations).

qrel2nrel(QRels, NRels, Options) :-
    aggregate_all(min(NLeft, NRels-Left),
                  qrel2nrel(QRels, Left, NRels, NLeft, Options),
                  min(_, NRels-Left)),
    maplist(rel_unknown, Left).

%!  qrel2nrel(+QRels, -Left, -NRels, -NLeft, +Options) is multi.
%
%   True when NRels are the  numeric   relations  for QRels. Enumeration
%   stops if we find an element that   leaves  nothing unmapped. Is this
%   ok? I.e., can we have multiple meaningful mappings?
%
%   @arg Left are unmapped QRels elements.
%   @arg NLeft is the length of Left.

qrel2nrel(QRels, Left, NRels, NLeft, Options) :-
    (   option(mode(derivatives), Options)
    ->  qrel2nrel(d, QRels, Left, NRels)
    ;   qrel2nrel(q, QRels, Left, NRels)
    ),
    length(Left, NLeft),
    (   NLeft =:= 0
    ->  !
    ;   true
    ).

qrel2nrel(DQ, QRels, Left, [NRel|NRels]) :- % Diff = A-B
    select(equal(min(A,B), Diff), QRels, QRels1),
    !,
    mkrel(DQ, Diff := A - B, NRel),
    exclude(is_prop(Diff), QRels1, QRels2),
    qrel2nrel(DQ, QRels2, Left, NRels).
qrel2nrel(DQ, QRels, Left, [NRel|NRels]) :- % Sum = A-B
    select(equal(plus(A,B), Sum), QRels, QRels1),
    !,
    mkrel(DQ,
          Sum := A + B,
          d(Sum) := d(A) + d(B),
          NRel),
    exclude(is_prop(Sum), QRels1, QRels2),
    qrel2nrel(DQ, QRels2, Left, NRels).
qrel2nrel(DQ, QRels, Left, [NRel|NRels]) :- % Mult = A*B
    select(equal(mult(A,B), Mult), QRels, QRels1),
    !,
    mkrel(DQ,
          Mult := A * B,
          d(Mult) := d(A) * d(B),
          NRel),
    exclude(is_prop(Mult), QRels1, QRels2),
    qrel2nrel(DQ, QRels2, Left, NRels).
qrel2nrel(DQ, QRels, Left, [NRel|NRels]) :- % Div = A/B
    select(equal(diw(A,B), Div), QRels, QRels1),
    !,
    mkrel(DQ,
          Div := A / B,
          d(Div) := d(A) / d(B),
          NRel),
    exclude(is_prop(Div), QRels1, QRels2),
    qrel2nrel(DQ, QRels2, Left, NRels).
qrel2nrel(DQ, QRels, Left, [NRel|NRels]) :-       % multiple prop on a target
    select(Prob, QRels, QRels1),
    is_prop(Dep, Prob),
    partition(is_prop(Dep), QRels1, Props, QRels2),
    Props \== [],
    prop_nrel([Prob|Props], NRel0),
    mkrel(DQ, NRel0, NRel),
    qrel2nrel(DQ, QRels2, Left, NRels).
qrel2nrel(DQ, QRels, Left, [NRel|NRels]) :-       % multiple integrals on a target
    select(Integral, QRels, QRels1),
    is_inf_by(Dep, Integral),
    partition(is_inf_by(Dep), QRels1, Integrals, QRels2),
    Integrals \== [],
    inf_by_nrel(DQ, [Integral|Integrals], NRel),
    qrel2nrel(DQ, QRels2, Left, NRels).
qrel2nrel(DQ, QRels, Left, NRels) :-
    qrel_nrel(DQ, Q, NRel),
    select_graph(Q, QRels, QRels1),
    !,
    append(NRel, NRels1, NRels),
    qrel2nrel(DQ, QRels1, Left, NRels1).
qrel2nrel(_DQ, Left, Left, []).

mkrel(q, NRel, _, NRel).
mkrel(d, _, DRel, DRel).

mkrel(q, NRel, NRel).
mkrel(d, NRel, DRel) :-
    drel(NRel, DRel).

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

%!  qrel_nrel(+QRel:list, -NRel:list) is multi.
%
%   Map  a  set  of  qualitative  relations  to  a  set  of  qualitative
%   relations. The rules are ordered to map larger submodels first.
%
%   @tbd include quantity spaces into the picture

qrel_nrel(q, [inf_pos_by(I,D)], [I := I + D*'Δt']).
qrel_nrel(d, [inf_pos_by(I,D)], [d(I) := D*'Δt']).
qrel_nrel(q, [inf_neg_by(I,D)], [I := I - D*'Δt']).
qrel_nrel(d, [inf_neg_by(I,D)], [d(I) := -D*'Δt']).
qrel_nrel(q, [prop_pos(Dep,Infl)], [Dep := c*Infl]).
qrel_nrel(d, [prop_pos(Dep,Infl)], [d(Dep) := c*d(Infl)]).
qrel_nrel(q, [prop_neg(Dep,Infl)], [Dep := -(c*Infl)]).
qrel_nrel(d, [prop_neg(Dep,Infl)], [d(Dep) := -(c*d(Infl))]).
qrel_nrel(DQ, [exogenous(Dep,Class)], RRels) :-
    freeze(Class, exogenous_equation(Class, DQ, Dep, RRels)).
% Correspondences typically cannot be used to create
% equations.  They should be used during the simulation
% to verify all constraints are satisfied.
qrel_nrel(_, [dir_q_correspondence(_,_)], []).
qrel_nrel(_, [q_correspondence(_,_)], []).
qrel_nrel(_, [equal(_,_)], []).
qrel_nrel(_, [smaller(_,_)], []).
qrel_nrel(_, [greater(_,_)], []).

is_prop(Dep, prop_pos(Dep,_)).
is_prop(Dep, prop_neg(Dep,_)).
is_prop(Dep, exogenous(Dep,_)).

prop_nrel(Props, Dep := Sum) :-
    Props = [H|_],
    is_prop(Dep, H),
    maplist(one_prop, Props, Parts),
    sum_expressions(Parts, Sum).

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

inf_by_nrel(DQ, Integrals, Dep := Expr) :-
    Integrals = [H|_],
    is_inf_by(Dep, H),
    maplist(one_inf_by, Integrals, Parts),
    sum_expressions(Parts, Sum),
    (   DQ == q
    ->  sum_expressions([Sum*'Δt', Dep], Expr)
    ;   Expr = Sum
    ).

one_inf_by(inf_pos_by(_, D), Expr) => Expr = c(1)*D.
one_inf_by(inf_neg_by(_, D), Expr) => Expr = -(c(1)*D).

%!  intergrals(+Model, -IRels, +Options) is det.
%
%   Create integration relations for all quantifies  that have a defined
%   quantity space.

intergrals(_Model, IRels, Options),
    option(mode(quantities), Options) =>
    IRels = [].
intergrals(Model, IRels, Options),
    option(mode(derivatives), Options) =>
    findall(Q, qspace_with_points(Model, Q), Qs),
    maplist(integral, Qs, IRels).

qspace_with_points(Model, Q) :-
    m_qspace(Model, Q, _QSpaceName, Values),
    memberchk(point(_), Values).

integral(Q, Q := Q + d(Q)).

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
    d_term(Term0, Term).
id_to_term(Mapping, Id, Term, S0, S), atom(Id) =>
    S = S0,
    Term = Mapping.get(Id,Id).
id_to_term(_Mapping, _Id, _Term, _S0, _S) =>
    fail.

%!  d_term(+Term, -DTerm) is det.
%
%   Translate a term that identifies a quantity (Attr(Entity)) into a an
%   identifier for its 1st derivative by prepenting   the  name with a Δ
%   symbol.

d_term(Term, DTerm), atom(Term) =>
    atom_concat('Δ', Term, DTerm).
d_term(Term, DTerm), compound(Term) =>
    compound_name_arguments(Term, Name, Args),
    atom_concat('Δ', Name, DName),
    compound_name_arguments(DTerm, DName, Args).

%!  init_nrels(+Model, +NRels, -Init) is det.
%
%   Use the input state (scenario)   to create initialization equations.
%   We create an initialization for each   quantity defined in the input
%   state, unless the quantity is defined by an exogenous steady input.
%
%   @tbd: This assumes only initial  values   on  quantities, __not__ on
%   derivatives.

init_nrels(Model, NRels, Init) :-
    q_input_state(Model, Input),
    dict_pairs(Input, _, Pairs),
    convlist(init_nrel(NRels), Pairs, Init).

init_nrel(_NRels, Id-d(zero,_,_,_), Init) =>
    Init = (Id := 0).
init_nrel(_NRels, Id-d(_,zero,_,_), Init) =>
    Init = (d(Id) := 0).
init_nrel(NRels, Id-_, _), memberchk(Id:=c, NRels) =>
    fail.
init_nrel(_NRels, Id-_QVal, Init) =>
    Init = (Id := placeholder(init, _)).

%!  exogenous_equation(+Class, +DQ, ?Q, -Equations) is det.

exogenous_equation(exogenous_steady, q, Q, NRel) =>
    NRel = [Q := c].
exogenous_equation(exogenous_steady, d, Q, NRel) =>
    NRel = [Q := c, d(Q) := 0].
exogenous_equation(exogenous_increasing, _DQ, Q, NRel) =>
    NRel = [Q := c+c*t].
exogenous_equation(exogenous_decreasing, _DQ, Q, NRel) =>
    NRel = [Q := c-(c*t)].
exogenous_equation(exogenous_sinus, _DQ, Q, NRel) =>
    NRel = [Q := (c*sin(c+c*t))].
exogenous_equation(exogenous_pos_parabola, _DQ, Q, NRel) =>
    NRel = [Q := (c-c*t^2)].
exogenous_equation(exogenous_pos_parabola, _DQ, Q, NRel) =>
    NRel = [Q := (c+c*t^2)].
exogenous_equation(exogenous_free, _DQ, Q, NRel) =>
    NRel = [Q := (c*random)].

%!  add_model_init(+EquationsIn, -EquationsOut) is det.
%
%   Add missing initializations to the equations.

:- exception_type(model_error, model_error(invalid(_))).
:- exception_type(model_error, model_error(no_initial_values(_))).
:- exception_type(model_error, model_error(no_time_formulas)).

add_model_init(Model, Eq0, Eq) :-
    catch(read_model(terms(Eq0), _Formulas, _Constants, _State,
                     [ allow_placeholders(true)
                     ]),
          model_error, Ball, true),
    (   var(Ball)
    ->  Eq = Eq0
    ;   init_from_error(Ball, Model, Init),
        append(Eq0, Init, Eq1),
        (   Init == []
        ->  Eq = Eq1
        ;   add_model_init(Model, Eq1, Eq)
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


%!  simplify_model(+Eql0, -Eql) is det.
%
%   The task of this is to simplify the  model, in particular try to get
%   rid of constants that are zero.  Note   that  any  `c` is a _unique_
%   constant.

simplify_model(Eql0, Eql) :-
    remove_constant_zero(Eql0, Eql1),
    simplify_zero_init(Eql1, Eql).

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


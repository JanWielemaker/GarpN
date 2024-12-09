:- module(model,
          [ init_model/2,               % +Model, -Equations
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

/** <module> Propose a (partial) numeric model from Garp

*/

%!  init_model(+Model, -Equations)

init_model(Model, Equations) :-
    findall(QRel, q_rel(Model, QRel), QRels), % Use relations
    findall(exogenous(Q,Class), q_exogenous(Model, Q, Class), Exos),
    append(QRels, Exos, Rels),
    qrel2nrel(Rels, NRels),
    init_nrels(Model, NRels, Init),           % Use input scenario
    default_nrels(DefNRels),                  % Defaults (time)
    append([NRels,DefNRels,Init], Eql0),
    simplify_model(Eql0, Eql1),
    id_mapping(Model, Mapping),
    foldsubterms(id_to_term(Mapping), Eql1, Eql2, [], ConstEql),
    append(Eql2, ConstEql, Equations1),
    add_model_init(Model, Equations1, Equations).

qrel2nrel(QRels, NRels) :-
    aggregate_all(min(NLeft, NRels-Left),
                  qrel2nrel(QRels, Left, NRels, NLeft),
                  min(_, NRels-Left)),
    maplist(rel_unknown, Left).

%!  qrel2nrel(+QRels, -Left, -NRels, -NLeft) is multi.
%
%   True when NRels are the  numeric   relations  for QRels. Enumeration
%   stops if we find an element that   leaves  nothing unmapped. Is this
%   ok? I.e., can we have multiple meaningful mappings?
%
%   @arg Left are unmapped QRels elements.
%   @arg NLeft is the length of Left.

qrel2nrel(QRels, Left, NRels, NLeft) :-
    qrel2nrel(QRels, Left, NRels),
    length(Left, NLeft),
    (   NLeft =:= 0
    ->  !
    ;   true
    ).

qrel2nrel(QRels, Left, [NRel|NRels]) :-
    select(Prob, QRels, QRels1),
    is_prop(Dep, Prob),
    partition(is_prop(Dep), QRels1, Props, QRels2),
    Props \== [],
    prop_nrel([Prob|Props], NRel),
    qrel2nrel(QRels2, Left, NRels).
qrel2nrel(QRels, Left, [NRel|NRels]) :-
    select(Integral, QRels, QRels1),
    is_inf_by(Dep, Integral),
    partition(is_inf_by(Dep), QRels1, Integrals, QRels2),
    Integrals \== [],
    inf_by_nrel([Integral|Integrals], NRel),
    qrel2nrel(QRels2, Left, NRels).

qrel2nrel(QRels, Left, NRels) :-
    qrel_nrel(Q, N),
    select_graph(Q, QRels, QRels1),
    !,
    append(N, NRels1, NRels),
    qrel2nrel(QRels1, Left, NRels1).
qrel2nrel(Left, Left, []).

%!  qrel_nrel(+QRel:list, -NRel:list) is multi.
%
%   Map  a  set  of  qualitative  relations  to  a  set  of  qualitative
%   relations. The rules are ordered to map larger submodels first.
%
%   @tbd include quantity spaces into the picture

qrel_nrel([ equal(min(InflA,InflB), Dep),
            prop_pos(Dep,InflA),
            prop_neg(Dep,InflB)
          ],
          [ Dep := InflA-InflB
          ]).
qrel_nrel([ equal(min(InflA,InflB), Dep),
            prop_pos(Dep,InflB),
            prop_neg(Dep,InflA)
          ],
          [ Dep := InflB-InflA
          ]).
qrel_nrel([ equal(min(InflA,InflB), Dep),
            prop_pos(Dep,InflA)
          ],
          [ Dep := InflA-InflB
          ]).
qrel_nrel([ equal(min(InflA,InflB), Dep)
          ],
          [ Dep := InflB-InflA
          ]).
qrel_nrel([inf_pos_by(I,D)], [I := I + D*'Δt']).
qrel_nrel([inf_neg_by(I,D)], [I := I - D*'Δt']).
qrel_nrel([prop_pos(Dep,Infl)], [Dep := c*Infl]).
qrel_nrel([prop_neg(Dep,Infl)], [Dep := -(c*Infl)]).
qrel_nrel([exogenous(Dep,Class)], [Dep := Expr]) :-
    freeze(Class, exogenous_equation(Class, Expr)).
% Correspondences typically cannot be used to create
% equations.  They should be used during the simulation
% to verify all constraints are satisfied.
qrel_nrel([dir_q_correspondence(_,_)], []).
qrel_nrel([q_correspondence(_,_)], []).
qrel_nrel([equal(_,_)], []).
qrel_nrel([smaller(_,_)], []).
qrel_nrel([greater(_,_)], []).

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
%one_prop(exogenous(_, Class), Expr) => exogenous_equation(Class, Expr).

join_sum(-(Expr), Sum0, Sum) => Sum = Sum0-Expr.
join_sum(Expr, Sum0, Sum)    => Sum = Sum0+Expr.

%!  is_inf_by(?Dep, +Relation) is semidet.

is_inf_by(Dep, inf_pos_by(Dep, _)).
is_inf_by(Dep, inf_neg_by(Dep, _)).

inf_by_nrel(Integrals, Dep := Expr) :-
    Integrals = [H|_],
    is_inf_by(Dep, H),
    maplist(one_inf_by, Integrals, Parts),
    sum_expressions(Parts, Sum),
    sum_expressions([Sum*'Δt', Dep], Expr).

one_inf_by(inf_pos_by(_, D), Expr) => Expr = c*D.
one_inf_by(inf_neg_by(_, D), Expr) => Expr = -(c*D).


%!  default_nrels(-NRels:list) is det.

default_nrels([ t := t + 'Δt',
                t := 0,
                'Δt' := placeholder(constant,0.1)
              ]).

id_to_term(_Mapping, c, Term, S0, S) =>
    length(S0, N0),
    N is N0+1,
    atom_concat(c,N,Term),
    S = [(Term := placeholder(constant,_))|S0].
id_to_term(Mapping, Id, Term, S0, S), atom(Id) =>
    S = S0,
    Term = Mapping.get(Id,Id).
id_to_term(_Mapping, _Id, _Term, _S0, _S) =>
    fail.

%!  init_nrels(+Model, +NRels, -Init) is det.
%
%   Use the input state (scenario)   to create initialization equations.
%   We create an initialization for each   quantity defined in the input
%   state, unless the quantity is defined by an exogenous steady input.

init_nrels(Model, NRels, Init) :-
    q_input_state(Model, Input),
    dict_pairs(Input, _, Pairs),
    convlist(init_nrel(NRels), Pairs, Init).

init_nrel(_NRels, Id-zero, Init) =>
    Init = (Id := 0).
init_nrel(NRels, Id-_, _), memberchk(Id:=c, NRels) =>
    fail.
init_nrel(_NRels, Id-_QVal, Init) =>
    Init = (Id := placeholder(init, _)).

%!  exogenous_equation(+Class, -Equation) is det.

exogenous_equation(exogenous_steady, Eq) =>
    Eq = (c).
exogenous_equation(exogenous_increasing, Eq) =>
    Eq = c+c*t.
exogenous_equation(exogenous_decreasing, Eq) =>
    Eq = c-(c*t).
exogenous_equation(exogenous_sinus, Eq) =>
    Eq = (c*sin(c+c*t)).
exogenous_equation(exogenous_pos_parabola, Eq) =>
    Eq = (c-c*t^2).
exogenous_equation(exogenous_pos_parabola, Eq) =>
    Eq = (c+c*t^2).
exogenous_equation(exogenous_free, Eq) =>
    Eq = (c*random).

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
    simplify_graph(In, Out),
    select_graph(In, Eql0, Eql1),
    !,
    append(Out, Eql1, Eql2),
    simplify_model(Eql2, Eql).
simplify_model(Eql, Eql).

simplify_graph([ Q := c + X*t,
                 Q := 0
               ],
               [ Q := X*t,
                 Q := 0
               ]).
simplify_graph([ Q := c - X*t,
                 Q := 0
               ],
               [ Q := X*t,
                 Q := 0
               ]).



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


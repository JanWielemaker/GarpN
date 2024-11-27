:- module(model,
          [ init_model/2,               % +Model, -Equations
            is_placeholder/1,           % @Term
            is_placeholder/2            % @Term, -Type
          ]).
:- use_module(library(terms)).
:- use_module(library(lists)).
:- use_module(library(exceptions)).

:- use_module(map).
:- use_module(gsim).
:- use_module(library(apply)).
:- use_module(library(pairs)).

/** <module> Propose a (partial) numeric model from Garp
 *
*/

%!  init_model(+Model, -Equations)

init_model(Model, Equations) :-
    findall(QRel, q_rel(Model, QRel), QRels),
    qrel2nrel(QRels, NRels),
    default_nrels(DefNRels),
    append(NRels, DefNRels, Eql0),
    id_mapping(Model, Mapping),
    foldsubterms(id_to_term(Mapping), Eql0, Eql1, [], ConstEql),
    append(Eql1, ConstEql, Equations1),
    add_model_init(Model, Equations1, Equations).

qrel2nrel(QRels, NRels) :-
    findall(t(NRels,Left,NLeft),
            qrel2nrel(QRels, Left, NRels, NLeft),
            Tuples),
    sort(3, =<, Tuples, [t(NRels,Left,NLeft)|_]),
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

qrel2nrel(QRels, Left, NRels) :-
    qrel_nrel(Q, N),
    select_graph(Q, QRels, QRels1),
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
qrel_nrel([ prop_pos(Dep,Infl),
            dir_q_correspondence(Dep,Infl)
          ],
          [ Dep := Infl/c
          ]).
qrel_nrel([inf_pos_by(I,D)], [I := I + D*'Δt']).
qrel_nrel([prop_pos(Dep,Infl)], [Dep := c*Infl]).

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

%!  add_model_init(+EquationsIn, -EquationsOut) is det.
%
%   Add missing initializations to the equations.

:- exception_type(model_error,
                  error(existence_error(initial_values, _Unresolved), _)).
:- exception_type(model_error,
                  error(validation_error(_Invalid), _)).
:- exception_type(model_error,
                  model_error(_)).

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

init_from_error(error(validation_error(Invalid), _),
                Model, Init) =>
    convlist(init_quantity(Model), Invalid, Init).
init_from_error(error(existence_error(initial_values, UnResolved), _),
                Model, Init) =>
    convlist(init_quantity(Model), UnResolved, Init).

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
    print_message(warning, unknown_relation(Rel)),
    fail.


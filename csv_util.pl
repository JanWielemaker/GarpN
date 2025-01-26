:- module(csv_util,
          [ key_label/3,                % +IdMapping, +Key, -Label
            order_keys/3,               % +IdMapping, +Keys, -Ordered
            series_key_derivative/3,    % +Series, +Key, -KerDer:pair
            key_state_derivative/3,     % +Key, +State, -Der:nonneg
            state_row/4,                % +Keys, +State:dict, +Empty, -Row:list
            state_trace_value/4,        % +KeyDer, +Empty, +State, -Value
            round_float_row/3,          % +Decimals, +RowIn, -Row
            round_float/3,              % +Decimals, +Value, -Rounded
            key_obj_attr/4              % +IdMapping, +Key, -Obj:atom,-Attr:atom
          ]).
:- use_module(library(pairs)).
:- use_module(library(terms)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(dcg/high_order)).

%!  key_label(+IdMapping, +Key, -Label) is det.

key_label(_, t, "Time") :-
    !.
key_label(_, garp_states, "Garp states") :-
    !.
key_label(IdMapping, Key, Label) :-
    format(atom(Label), '~w', [IdMapping.get(Key)]),
    !.
key_label(_, Key, Key).

%!  order_keys(+IdMapping, +Keys, -Ordered) is det.

order_keys(IdMapping, Keys0, Keys) :-
    map_list_to_pairs(csv_column_rank(IdMapping), Keys0, Pairs),
    keysort(Pairs, PairsS),
    pairs_values(PairsS, Keys).

csv_column_rank(_IdMapping, t,           Rank) => Rank = 1-1.
csv_column_rank(_IdMapping, state,       Rank) => Rank = 1-1.
csv_column_rank(_IdMapping, garp_states, Rank) => Rank = 4-1.
csv_column_rank(IdMapping,  Key,         Rank),
    key_obj_attr(IdMapping, Key, Obj, Attr) =>
    Rank = 2-t(Obj-Attr).
csv_column_rank(_IdMapping, Key,         Rank) =>
    Rank = 3-Key.

%!  key_obj_attr(+IdMapping, +Key, -Obj:atom, -Attr:atom) is det.

key_obj_attr(IdMapping, Key, Obj, Attr),
    Term = IdMapping.get(Key),
    functor(Term, Attr, 1) =>
    arg(1, Term, Obj).
key_obj_attr(_, Key, _Obj, Attr) =>
    Attr = Key.

%!  series_key_derivative(+Series, +Key, -KerDer:integer) is det.
%!  key_state_derivative(+Key, +State, -Der:nonneg) is det.
%
%   Determine the derivative representation for Key in State or a Series
%   of states. The derivative is 0 if  this   is  a  plain value, 1 if a
%   value and first derivative are combined on Key, etc.

series_key_derivative(States, Key, Der) :-
    sample(States, Sample),
    maplist(key_state_derivative(Key), Sample, Ders),
    max_list(Ders, Der).

key_state_derivative(garp_states, _, 0) :-
    !.
key_state_derivative(t, _, 0) :-
    !.
key_state_derivative(Key, State, Der) :-
    d_derivative(State.get(Key), Der).

d_derivative(d(_,D1,D2,D3), Der) :-
    (   nonvar(D3) -> Der = 3
    ;   nonvar(D2) -> Der = 2
    ;   nonvar(D1) -> Der = 1
    ;                 Der = 0
    ).

sample(Series, Sample) :-
    length(Series, Len),
    (   Len < 100
    ->  Sample = Series
    ;   Nth is Len/10,
        sample(0, Nth, Series, Sample)
    ).

sample(_, _, [Last], [Last]) :-
    !.
sample(I, Nth, [H|T0], Sample) :-
    (   I mod Nth =:= 0
    ->  Sample = [H|ST]
    ;   Sample = ST
    ),
    I2 is I+1,
    sample(I2, Nth, T0, ST).

%!  state_row(+KeysDers, +State:dict, +Empty, -Row:list)
%
%   Turn a state into a row,   unfolding  derivative terms (d(...)) into
%   multiple columns. Possibly missing cells are   filled with a copy of
%   Empty.
%
%   @arg KeysDers is a list of pairs Key-Ders, where Der is the
%   list of derivatives to show.
%   @arg Row is a list of `K-V` pairs, where `K` is a key or a term
%   der(K,N), with `N>0`.

state_row(KeysDers, State, Empty, Row) :-
    phrase(state_row(KeysDers, State, Empty), Row).

state_row([], _, _) -->
    [].
state_row([K-Ders|T], State, Empty) -->
    sequence(der_column(K, State, Empty), Ders),
    state_row(T, State, Empty).

der_column(K, State, Empty, 0) ==>
    { (   state_value(K, State, Value)
      ->  true
      ;   copy_term(Empty, Value)
      )
    },
    [K-Value].
der_column(K, State, Empty, Der) ==>
    { I is Der+1,
      (   D = State.get(K),
          arg(I, D, Value),
          nonvar(Value)
      ->  true
      ;   copy_term(Empty, Value)
      )
    },
    [der(K,Der)-Value].

state_value(Key, State, Value) :-
    V0 = State.get(Key),
    (   V0 = d(Value, _, _, _)
    ->  nonvar(Value)
    ;   Value = V0
    ).


%!  state_trace_value(+KeyDer, +Empty, +State, -Value) is det.
%
%   Extract the value for the trace  Key-Der   of  State. If this is not
%   available, Value is a copy of Empty.

state_trace_value(K-D, Empty, State, V) :-
    (   get_dict(K, State, V0)
    ->  val_or_der(D, V0, V, Empty)
    ;   copy_term(Empty, V)
    ).

val_or_der(0, V, H, _Empty), compound(V), compound_name_arity(V,d,_) =>
    arg(1, V, H).
val_or_der(0, V, H, _Empty) =>
    H = V.
val_or_der(D, V, H, Empty), compound(V), compound_name_arity(V,d,_) =>
    FA is D+1,
    (   arg(FA, V, H)
    ->  true
    ;   copy_term(Empty, H)
    ).
val_or_der(_D, _V, H, Empty) =>
    copy_term(Empty, H).

%!  round_float_row(+Decimals, +RowIn, -Row) is det.
%
%   Round all floats in RowIn to Decimals.

round_float_row(N, Row0, Row) :-
    same_functor(Row0, Row),
    functor(Row0, _, Arity),
    round_float_cols(1, Arity, N, Row0, Row).

round_float_cols(I, Arity, N, Row0, Row) :-
    I =< Arity,
    !,
    arg(I, Row0, F0),
    arg(I, Row, F),
    (   float(F0)
    ->  round_float(N, F0, F)
    ;   F = F0
    ),
    I2 is I+1,
    round_float_cols(I2, Arity, N, Row0, Row).
round_float_cols(_, _, _, _, _).

%!  round_float(+Decimals, +Value, -Rounded) is det.

round_float(_Decimals, Value, Rounded) :-
    Value =:= 0,
    !,
    Rounded = 0.0.
round_float(Decimals, Value, Rounded) :-
    Value < 0,
    !,
    round_float(Decimals, -Value, MinRounded),
    Rounded is -MinRounded.
round_float(Decimals, Value, Rounded) :-
    FracDecimals is Decimals-floor(log10(Value))-1,
    (   FracDecimals > 0
    ->  Mult is 10.0^FracDecimals,
        Rounded is round(Value*Mult)/Mult
    ;   Mult is 10.0^(-FracDecimals),
        Rounded is round(Value/Mult)*Mult
    ).

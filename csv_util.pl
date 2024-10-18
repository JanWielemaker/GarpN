:- module(csv_util,
          [ key_label/3,
            order_keys/2,
            state_row/4,                % +Keys, +State:dict, +Empty, -Row:list
            round_float_row/3,          % +Decimals, +RowIn, -Row
            round_float/3
          ]).
:- use_module(library(pairs)).
:- use_module(library(terms)).
:- use_module(library(dcg/high_order)).

%!  key_label(+IdMapping, +Key, -Label) is det.

key_label(_, t, "Time") :-
    !.
key_label(_, garp_states, "Garp states") :-
    !.
key_label(IdMapping, Key, Label) :-
    growth(Q) = IdMapping.get(Key),
    !,
    format(atom(Label), '\u0394~w', [Q]).
key_label(IdMapping, Key, Label) :-
    number_of(Q) = IdMapping.get(Key),
    !,
    format(atom(Label), '#~w', [Q]).
key_label(IdMapping, Key, Label) :-
    format(atom(Label), '~w', [IdMapping.get(Key)]),
    !.
key_label(_, Key, Key).

%!  order_keys(+Keys, -Ordered) is det.

order_keys(Keys0, Keys) :-
    map_list_to_pairs(csv_column_rank, Keys0, Pairs),
    keysort(Pairs, PairsS),
    pairs_values(PairsS, Keys).

csv_column_rank(t,     0) :- !.
csv_column_rank(state, 0) :- !.
csv_column_rank(Key,   1) :- sub_atom(Key, _, _, _, number_of), !.
csv_column_rank(Key,   2) :- sub_atom(Key, _, _, _, growth), !.
csv_column_rank(_,     3).

%!  state_row(+Keys, +State:dict, +Empty, -Row:list)
%
%   Turn a state into a row,   unfolding  derivative terms (d(...)) into
%   multiple columns. Possibly missing cells are   filled with a copy of
%   Empty.
%
%   @arg Row is a list of `K-V` pairs, where `K` is a key or a term
%   der(K,N), with `N>0`.

state_row(Keys, State, Empty, Row) :-
    phrase(state_row(Keys, State, Empty), Row).

state_row([], _, _) -->
    [].
state_row([K|T], State, Empty) -->
    state_cell(K, State.get(K)),
    !,
    state_row(T, State, Empty).
state_row([K|T], State, Empty) -->
    { copy_term(Empty, Cell) },
    [K-Cell],
    state_row(T, State, Empty).

state_cell(K, V) -->
    { compound(V),
      compound_name_arguments(V, d, Args)
    },
    !,
    der_columns(K, 0, Args).
state_cell(K, V) -->
    [K-V].

der_columns(_,_,[]) -->
    !.
der_columns(K,0,[H|T]) -->
    !,
    [K-H],
    der_columns(K,1,T).
der_columns(K,N,[H|T]) -->
    !,
    [der(K,N)-H],
    {N1 is N+1},
    der_columns(K,N1,T).


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

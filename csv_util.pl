:- module(csv_util,
          [ key_label/3,
            order_keys/2,
            round_float_row/3,          % +Decimals, +RowIn, -Row
            round_float/3
          ]).
:- use_module(library(pairs)).
:- use_module(library(terms)).

%!  key_label(+IdMapping, +Key, -Label) is det.

key_label(_, t, "Time") :-
    !.
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

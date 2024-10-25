:- module(csv_util,
          [ key_label/3,                % +IdMapping, +Key, -Label
            order_keys/2,               % +Keys, -Ordered
            series_key_derivative/3,    % +Series, +Key, -KerDer:pair
            key_state_derivative/3,     % +Key, +State, -Der:nonneg
            state_row/4,                % +Keys, +State:dict, +Empty, -Row:list
            state_trace_value/4,        % +KeyDer, +State, +Empty, -Value
            round_float_row/3,          % +Decimals, +RowIn, -Row
            round_float/3               % +Decimals, +Value, -Rounded
          ]).
:- use_module(library(pairs)).
:- use_module(library(terms)).

%!  key_label(+IdMapping, +Key, -Label) is det.

key_label(_, t, "Time") :-
    !.
key_label(_, garp_states, "Garp states") :-
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
csv_column_rank(garp_states, 4) :- !.
csv_column_rank(_,     3).

%!  series_key_derivative(+Series, +Key, -KerDer:pair) is det.
%!  key_state_derivative(+Key, +State, -Der:nonneg) is det.
%
%   Determine the derivative representation for Key in State or a Series
%   of states. The derivative is 0 if  this   is  a  plain value, 1 if a
%   value and first derivative are combined on Key, etc.

series_key_derivative(States, Key, Key-Der) :-
    maplist(key_state_derivative(Key), States, Ders),
    max_list(Ders, Der).

key_state_derivative(Key, State, Der) :-
    (   Value = State.get(Key),
        compound(Value),
        compound_name_arity(Value, d, Arity)
    ->  Der is Arity-1
    ;   Der = 0
    ).

%!  state_row(+KeysDers, +State:dict, +Empty, -Row:list)
%
%   Turn a state into a row,   unfolding  derivative terms (d(...)) into
%   multiple columns. Possibly missing cells are   filled with a copy of
%   Empty.
%
%   @arg KeysDers is a list of pairs Key-Der, where Der is the
%   highest derivative shown or 0 if this is just a plain value.
%   @arg Row is a list of `K-V` pairs, where `K` is a key or a term
%   der(K,N), with `N>0`.

state_row(KeysDers, State, Empty, Row) :-
    phrase(state_row(KeysDers, State, Empty), Row).

state_row([], _, _) -->
    [].
state_row([K-D|T], State, Empty) -->
    state_cell(K, D, State.get(K), Empty),
    !,
    state_row(T, State, Empty).
state_row([K-D|T], State, Empty) -->
    { copy_term(Empty, Cell) },
    state_cell(K, D, Cell, Empty),
    state_row(T, State, Empty).

state_cell(K, 0, V, _Empty) -->
    !,
    [K-V].
state_cell(K, D, V, Empty) -->
    der_columns(0, D, K, V, Empty).

der_columns(N,D,_,_,_) -->
    { N > D },
    !.
der_columns(0,D,K,V, Empty) -->
    !,
    { val_or_der(0, V, H, Empty) },
    [K-H],
    der_columns(1, D, K, V, Empty).
der_columns(N,D,K,V, Empty) -->
    !,
    { val_or_der(0, V, H, Empty) },
    [der(K,N)-H],
    {N1 is N+1},
    der_columns(N1,D,K,V, Empty).

%!  state_trace_value(+KeyDer, +State, +Empty, -Value) is det.
%
%   Extract the value for the trace  Key-Der   of  State. If this is not
%   available, Value is a copy of Empty.

state_trace_value(K-D, State, Empty, V) :-
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

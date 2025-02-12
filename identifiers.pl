:- module(identifiers,
          [ term_key/3,                 % +Term, -Key, +IdMapping
            term_derivative/2,          % ?Term, ?DTerm
            key_derivative/3,           % ?Key, ?DKey, +IdMapping
            is_derivative_term/1        % ++Term
          ]).

/** <module> Translate identifier representations

Quantities  in  Garp  are  represented  as  Property(Entity).  Dynalearn
generates arbitrary `n<num>` terms for them. For efficiency, the gsim.pl
module represents quantities using either the  Dynalearn atoms or simple
Prolog serialization.

Derivatives of a quantity using the  Δ   symbol.  That  again is used in
several ways. In the Dynalearn representation,   we simply prepent the Δ
symbol. In the term representation we use ΔProperty(Entity), where the Δ
is prepended to the functor name.  This   leads  to single quotes in the
serialization.

This module provides utilities to deal with the various representations.
*/

%!  term_key(+Term, -Key, +IdMapping) is det.
%!  term_key(-Term, +Key, +IdMapping) is det.

term_key(Term, Key, IdMapping), atom(Key) =>
    (   get_dict(Key, IdMapping, Term0)
    ->  Term = Term0
    ;   term_string(Term, Key)
    ).
term_key(Term, Key, IdMapping), ground(Term) =>
    (   get_dict(Key0, IdMapping, Term)
    ->  Key = Key0
    ;   format(atom(Key), '~q', Term)
    ).

%!  term_derivative(+Term, -DTerm) is det.
%!  term_derivative(-Term, +DTerm) is semidet.
%
%   Translate a term that identifies a quantity (Attr(Entity)) into a an
%   identifier for its 1st derivative by prepending   the  name with a Δ
%   symbol.

:- det(term_derivative/2).
term_derivative(Term, DTerm), (atom(Term) ; atom(DTerm)) =>
    atom_concat('Δ', Term, DTerm).
term_derivative(Term, DTerm), compound(Term) =>
    compound_name_arguments(Term, Name, Args),
    atom_concat('Δ', Name, DName),
    compound_name_arguments(DTerm, DName, Args).
term_derivative(Term, DTerm), compound(DTerm) =>
    compound_name_arguments(DTerm, DName, Args),
    atom_concat('Δ', Name, DName),
    compound_name_arguments(Term, Name, Args).

%!  is_derivative_term(@Term) is semidet.
%
%   True if Term represents a derivative.

is_derivative_term(Term), atom(Term) =>
    sub_atom(Term, 0, _, _, 'Δ').
is_derivative_term(Term), compound(Term), functor(Term, DName, 1) =>
    sub_atom(DName, 0, _, _, 'Δ').
is_derivative_term(_) =>
    fail.

%!  key_derivative(+Key, -DKey, +IdMapping) is det.
%!  key_derivative(-Key, +DKey, +IdMapping) is det.
%
%   Derivatives   have   a   key   that    is   the   serialization   of
%   ΔProperty(Entity). To find the matching quantity  key, we remove the
%   Δ, deserialize (parse) and find the   quantity  key in the IdMapping
%   dict.

key_derivative(Key, DKey, IdMapping), atom(Key) =>
    get_dict(Key, IdMapping, Term),
    term_derivative(Term, DTerm),
    term_key(DTerm, DKey, IdMapping).
key_derivative(Key, DKey, IdMapping), atom(DKey) =>
    sub_atom(DKey, B, _, A, 'Δ'),
    sub_atom(DKey, 0, B, _, Before),
    sub_atom(DKey, _, A, 0, After),
    string_concat(Before, After, S),
    term_string(IdTerm, S),
    get_dict(Key, IdMapping, IdTerm).



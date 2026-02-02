:- module(garpn_debug,
          [ set_model/1,
            quantity_label/2                    % +QId, -Label
          ]).
:- use_module(csv_util).
:- use_module(map).
:- use_module(identifiers).

:- dynamic
    current_model/1.

%!  set_model(+ModelId) is det.

set_model(ModelId) :-
    (   var(ModelId)
    ->  true
    ;   retractall(current_model(_)),
        asserta(current_model(ModelId))
    ).

:- multifile user:portray/1.

user:portray(QId) :-
    quantity_label(QId, Label),
    writeq(Label).

%!  quantity_label(+QId, -Label) is semidet.
%
%   True when Qid is a quantity name   from Dynalearn (nXXXXX) and Label
%   is a symbolic name for it. This   is  used for portraying values for
%   debugging.

quantity_label(QId, Label) :-
    atom(QId),
    atom_concat(n, Aid, QId),
    atom_number(Aid, _),
    current_model(Model),
    id_mapping(Model, IdMapping),
    key_label(IdMapping, QId, Label).
quantity_label(DQId, Label) :-
    atom(DQId),
    atom_concat('Δn', Aid, DQId),
    atom_number(Aid, _),
    current_model(Model),
    id_mapping(Model, IdMapping),
    atom_concat('Δ', QId, DQId),
    key_label(IdMapping, QId, Label0),
    atom_concat('Δ', Label0, Label).

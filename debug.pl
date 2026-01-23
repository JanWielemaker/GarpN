:- module(garpn_debug,
          [ set_model/1
          ]).
:- use_module(csv_util).
:- use_module(map).

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
    atom(QId),
    atom_concat(n, Aid, QId),
    atom_number(Aid, _),
    current_model(Model),
    id_mapping(Model, IdMapping),
    key_label(IdMapping, QId, Label),
    writeq(Label).

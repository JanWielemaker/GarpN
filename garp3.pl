:- module(garp3,
          [ garp_load_model/1,
            garp_full_simulation/0,
            garp_freeze/1
          ]).
:- use_module(library(pce)).
:- use_module(map).

%!  garp_freeze(+FileOrDir) is det.
%
%   Run all models from a directory or a   single  model from a file and
%   store the results in ``garp/<Model>.db``. Normally executed as
%
%       ?- garp_freeze(models).

garp_freeze(Dir), exists_directory(Dir) =>
    forall(directory_member(Dir, File,
                            [ extensions([hgp])
                            ]),
           garp_freeze(File)).
garp_freeze(File), exists_file(File) =>
    file_base_name(File, Base),
    file_name_extension(Model, hgp, Base),
    saved_model_file(Model, SaveFile),
    (   must_make(SaveFile, File)
    ->  garp_load_model(File),
        garp_full_simulation,
        save_garp_results(Model)
    ;   true
    ).

must_make(File, Source) :-
    exists_file(File),
    !,
    time_file(Source, STime),
    time_file(File, FTime),
    STime > FTime.
must_make(_, _).


garp_load_model(File) :-
    in_pce_thread_sync(send(@app, loadModelFromHGPFile, File)).

garp_full_simulation :-
    in_pce_thread_sync(send(@app, startFullSimulation, @app?model?lastEditedIS)).

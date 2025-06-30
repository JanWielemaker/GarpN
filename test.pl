:- module(garp_test,
          [ save_test/2                 % +Test, +ModelId
          ]).
:- use_module(dynalearn, [download_model/2]).
:- use_module(gui, [numeric_model_file/2]).
:- use_module(library(filesex),
              [directory_file_path/3, make_directory_path/1, copy_file/2]).

/** <module> Manage automated tests

## Test data

Test data is stored in a  directory   per  test.  The directory contains
several files:

  - dynalearn.json
    The model as downloaded from Dynalearn
  - model.pl
    The final simulation model
  - results.pl
    A Prolog file holding
    - The final qualitative series with mapping to Garp as
      garp_mapping(Series)
    - The model as generated in the three modes as
      generated_model(Mode, Model)
*/

%!  save_test(+Test, +ModelId) is det.

save_test(Test, ModelId) :-
    save_dynalearn(Test, ModelId),
    save_model(Test, ModelId),
    save_results(Test, ModelId).

%!  save_dynalearn(+Test, +ModelId) is det.
%
%   Save the JSON as downloaded from Dynalearn.
%
%   @tbd: Should we also save the converted model?

save_dynalearn(Test, ModelId) :-
    test_file(Test, 'dynalearn.json', JSON),
    download_model(ModelId, JSON).

test_file(Test, File, Path) :-
    test_directory(Test, Dir),
    directory_file_path(Dir, File, Path).

test_directory(Test, Dir) :-
    directory_file_path(tests, Test, Dir),
    make_directory_path(Dir).

%!  save_model(++Test, ++ModelId) is det.
%
%   Save the final model. For  now,  we   use  the  saved numeric model.
%   Should we merge this with the GUI?

save_model(Test, ModelId) :-
    test_file(Test, 'model.pl', ModelFile),
    numeric_model_file(ModelId, ReferenceFile),
    copy_file(ReferenceFile, ModelFile).

%!  save_results(++Test, ++ModelId) is det.

save_results(Test, ModelId) :-
    test_file(Test, 'results.pl', ResultsFile).


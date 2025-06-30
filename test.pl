:- module(garp_test,
          [ save_test/2,                % +Test, +Data
            existing_test_files/2       % ++Model, -TestFiles
          ]).
:- use_module(dynalearn, [download_model/2]).
:- use_module(library(filesex), [directory_file_path/3]).

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

%!  save_test(+Test, +Data) is det.

save_test(Test, Data) :-
    download_model(Data.model, Dynalearn),
    Data1 = Data.put(#{dynalearn:Dynalearn}),
    save_title(Data.model, Data.get(title,_)),
    test_data_file(Data.model, Test, File),
    setup_call_cleanup(
        open(File, write, Out, [encoding(utf8)]),
        print_term(Data1, [ output(Out),
                            right_margin(78),
                            fullstop(true),
                            nl(true)
                          ]),
        close(Out)).

test_data_file(Model, Test, TestPath) :-
    model_test_directory(Model, ModelDir),
    file_name_extension(Test, test, TestFile),
    directory_file_path(ModelDir, TestFile, TestPath).

save_title(Model, Title) :-
    nonvar(Title),
    model_test_directory(Model, ModelDir),
    directory_file_path(ModelDir, 'Title.txt', File),
    setup_call_cleanup(
        open(File, write, Out, [encoding(utf8)]),
        format(Out, '~w~n', [Title]),
        close(Out)).

model_test_directory(Model, ModelDir) :-
    directory_file_path(tests, Model, ModelDir),
    make_directory_path(ModelDir).

%!  existing_test_files(++Model, -TestFiles) is det.
%
%   True when TestFiles is a list of existing test files. The returned
%   files have no extension.

existing_test_files(Model, TestFiles) :-
    model_test_directory(Model, ModelDir),
    directory_files(ModelDir, Existing),
    convlist(clean_extension(test), Existing, TestFiles).

clean_extension(Ext, File, Base) :-
    file_name_extension(Base, Ext, File).

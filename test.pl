:- module(garp_test,
          [ save_test/2,                % +Test, +Data
            existing_test_files/2,      % ++Model, -TestFiles
            run_test/2                  % ++File, +Options
          ]).
:- use_module(library(filesex), [directory_file_path/3, make_directory_path/1]).
:- use_module(library(ansi_term), [ansi_format/3]).
:- use_module(library(apply), [convlist/3]).
:- use_module(library(pprint), [print_term/2]).
:- use_module(library(readutil), [read_file_to_terms/3]).
:- use_module(library(statistics), [call_time/2]).
:- use_module(library(http/json), [atom_json_dict/3]).

:- use_module(equations).
:- use_module(gsim).
:- use_module(dynalearn,
              [download_model/2, dynalearn_model/2, set_dynalearn_model/2]).
:- use_module(map).
:- use_module(diff).

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

:- set_prolog_flag(rational_syntax, compatibility).

%!  save_test(+Test, +Data) is det.

save_test(Test, Data) :-
    Model = Data.model,
    download_model(Model, DynalearnJSON),
    dynalearn_model(Model, Dynalearn),
    Data1 = Data.put(#{ dynalearn:
                          #{ json: DynalearnJSON,
                             prolog:Dynalearn
                           }
                      }),
    save_title(Data.model, Data.get(title,_)),
    test_data_file(Data.model, Test, File),
    setup_call_cleanup(
        open(File, write, Out, [encoding(utf8)]),
        print_term(Data1, [ output(Out),
                            right_margin(78),
                            fullstop(true),
                            nl(true),
                            module(garp_test)
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

                /*******************************
                *        RUNNING TESTS         *
                *******************************/

:- meta_predicate
    expect(+, 0).

%!  run_test(++File, +Options)
%
%

run_test(File, Options) :-
    read_file_to_terms(File, [TestDict],
                       [ encoding(utf8),
                         module(garp_test)
                       ]),
    test_gui_import(TestDict, Options).

test_gui_import(TestDict, Options) :-
    test_and_set_dynalearn_model(TestDict, Options),
    test_gui_model_import(TestDict, Options),
    test_gui_qspaces_import(TestDict, Options),
    test_simulate(TestDict, Options).

%!  test_and_set_dynalearn_model(+TestDict, +Options)
%
%   Test the conversion of the Dynalearn JSON   document and set this as
%   the current model.

test_and_set_dynalearn_model(TestDict, _Options) :-
    Model = TestDict.model,
    atom_json_dict(TestDict.dynalearn.json, JSONDict,
                   [ value_string_as(atom)
                   ]),
    dynalearn:import_model(JSONDict, PrologModel),
    PrologModelExp = TestDict.dynalearn.prolog,
    expect("Parse Dynalearn JSON", PrologModel =@= PrologModelExp),
    set_dynalearn_model(Model, PrologModelExp).

%!  test_gui_model_import(+TestDict, +Options)
%
%   Test parsing the LaTeX representation of the model into Prolog.

test_gui_model_import(TestDict, _Options) :-
    latex_to_prolog_ex(TestDict.web_data.ml_source, Equations),
    Expected = TestDict.prolog_data.equations,
    expect("Parse LaTeX equations", Equations == Expected).

%!  test_gui_qspaces_import(+TestDict, +Options)
%
%   Test the translation of the quantity spaces from the JavaScript JSON
%   representation.

test_gui_qspaces_import(TestDict, _Options) :-
    Model = TestDict.model,
    JQspaces = TestDict.web_data.jqspaces,
    QSpacesEx = TestDict.prolog_data.qspaces,
    gui:qspaces(Model, JQspaces, QSpaces),
    expect("Parse quantity spaces", QSpaces == QSpacesEx).

%!  test_simulate(+TestDict, +Options)
%
%   Run the simulation

test_simulate(TestDict, _Options) :-
    Model = TestDict.model,
    #{ iterations:  Iterations,
       sample:      Sample,
       method:      Method,
       derivatives: Derivatives } :< TestDict.parameters,
    #{ qspaces:     QSpaces,
       equations:   Equations} :< TestDict.prolog_data,
    id_mapping(Model, IdMapping),
    SimOptions = [ model(Model),
                   match(Derivatives),
                   iterations(Iterations),
                   method(Method),
                   track(initialized),
                   sample(Sample),
                   id_mapping(IdMapping),
                   qspaces(QSpaces),
                   equations(Equations)
                 ],
    call_time(simulate(terms(Equations), Series0, SimOptions), _Time),
    init_derivatives(Series0, Series, IdMapping),
    nq_series(Series, QSeries, [link_garp_states(true)|SimOptions]),
    QSeriesExp = TestDict.results.qseries,
    expect("Qualitative series", QSeries =@= QSeriesExp).

%!  expect(++Msg, :Goal)
%
%   Expect Goal to be true.  Similar   to  assertion/1,  but attempts to
%   print failures in a more readable way.

expect(_Msg, Goal), call(Goal) =>
    true.
expect(Msg, Goal) =>
    unexpected(Msg, Goal).

unexpected(Msg, _:Goal) =>
    unexpected(Msg, Goal).
unexpected(Msg, V == E) =>
    ansi_format(error, '~s: unexpected result:~n', [Msg]),
    term_diff(V, E).


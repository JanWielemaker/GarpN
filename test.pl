:- module(garp_test,
          [ save_test/2,                % +Test, +Data
            existing_test_files/2,      % ++Model, -TestFiles
            run_garp_tests/0,
            run_garp_tests/1,           % +Options
            run_garp_test/2,            % ++File, +Options
            load_test/4                 % +Model, +Test, -Source, -Settings
          ]).
:- use_module(library(filesex),
              [directory_file_path/3, make_directory_path/1, directory_member/3]).
:- use_module(library(ansi_term), [ansi_format/3]).
:- use_module(library(apply), [convlist/3, maplist/2, maplist/3, partition/4]).
:- use_module(library(pprint), [print_term/2]).
:- use_module(library(readutil), [read_file_to_terms/3, read_file_to_string/3]).
:- use_module(library(statistics), [call_time/2]).
:- use_module(library(http/json), [atom_json_dict/3]).
:- use_module(library(http/http_dispatch), [http_handler/3, http_link_to_id/3]).
:- use_module(library(lists), [append/3]).
:- use_module(library(pairs), [group_pairs_by_key/2]).
:- use_module(library(http/html_write), [reply_html_page/2, html/3]).
:- use_module(library(dcg/high_order), [sequence/4]).
:- use_module(library(http/http_parameters), [http_parameters/2]).
:- use_module(library(streams), [with_output_to/3]).
:- use_module(library(http/htmx), [reply_htmx/1]).

:- use_module(equations).
:- use_module(gsim).
:- use_module(dynalearn,
              [download_model/2, dynalearn_model/2, set_dynalearn_model/2]).
:- use_module(gui, [scripts//0]).
:- use_module(map).
:- use_module(diff).
:- use_module(library(pcre), [re_split/3]).
:- use_module(library(dcg/basics), [remainder/3]).

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
    get_time(Now),
    Data1 = Data.put(#{ saved: Now,
                        dynalearn:
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

%!  load_test(+Model, +Test, -Source, -Settings) is det.
%
%   Load a test back as it was saved for the test.

load_test(Model, Test, Source,
          [ qspaces(QSpaces),
            iterations(Iterations),
            method(Method),
            title(Title)
          ]) :-
    test_file(File, Model, Test),
    read_file_to_terms(File, [TestDict],
                       [ encoding(utf8),
                         module(garp_test)
                       ]),
    Source     = terms(TestDict.prolog_data.equations),
    Title      = TestDict.title,
    Iterations = TestDict.parameters.iterations,
    Method     = TestDict.parameters.method,
    QSpaces    = TestDict.prolog_data.qspaces.


                /*******************************
                *        RUNNING TESTS         *
                *******************************/

%!  run_garp_tests.
%!  run_garp_tests(+Options).
%
%   Run saved GarpN tests.

run_garp_tests :-
    run_garp_tests([]).

run_garp_tests(Options) :-
    tests_by_model(ByModel),
    maplist(garp_test_model(Options), ByModel).

tests_by_model(ByModel) :-
    findall(Model-Test, garp_test(Model, Test), Pairs),
    keysort(Pairs, PairsSorted),
    group_pairs_by_key(PairsSorted, ByModel).

garp_test_model(Options, Model-Tests) =>
    progress(start_model(Model), Options),
    maplist(garp_test_model_test_r(Options, Model), Tests).

garp_test_model_test_r(Options, Model, Test) :-
    progress(start_test(Model, Test), Options),
    garp_test_model_test(Model, Test, Options).

garp_test_model_test(Model, Test, Options) :-
    test_file(File, Model, Test),
    run_garp_test(File, Options).

:- meta_predicate
    expect(+, 0).

%!  run_garp_test(++File, +Options)
%
%

run_garp_test(File, Options) :-
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

test_simulate(TestDict, Options) :-
    Model = TestDict.model,
    #{ iterations:  Iterations,
       sample:      Sample,
       method:      Method,
       track:	Track,
       derivatives: Derivatives } >:< TestDict.parameters,
    ignore(Track = all),
    #{ qspaces:     QSpaces,
       equations:   Equations} :< TestDict.prolog_data,
    id_mapping(Model, IdMapping),
    SimOptions = [ model(Model),
                   match(Derivatives),
                   iterations(Iterations),
                   method(Method),
                   track(Track),
                   sample(Sample),
                   id_mapping(IdMapping),
                   qspaces(QSpaces),
                   equations(Equations)
                 ],
    call_time(simulate(terms(Equations), Series0, SimOptions), _Time),
    init_derivatives(Series0, Series, IdMapping),
    nq_series(Series, QSeries, [link_garp_states(true)|SimOptions]),
    QSeriesExp = TestDict.results.qseries,
    expect("Qualitative series", QSeries =@= QSeriesExp),
    report_garp_mapping_quality(QSeries, QSeriesExp, Options).

report_garp_mapping_quality(QSeries, QSeriesExp, Options) :-
    QSeries =@= QSeriesExp,
    !,
    report_garp_mapping_quality(QSeries, Options).

report_garp_mapping_quality(QSeries, Options) :-
    maplist(get_dict(garp_states), QSeries, Mapping),
    partition(perfect, Mapping, Perfect, Imperfect),
    partition(ambiguous, Imperfect, Ambiguous, Unmapped),
    maplist(length,
            [Perfect, Ambiguous, Unmapped],
            [PerfectCnt, AmbiguousCnt, UnmappedCnt]),
    progress(mapping_quality(PerfectCnt, AmbiguousCnt, UnmappedCnt), Options).

perfect([_]).
ambiguous([_,_|_]).


                /*******************************
                *            EXPECT            *
                *******************************/

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
unexpected(Msg, V =@= E) =>
    ansi_format(error, '~s: unexpected result:~n', [Msg]),
    \+ \+ ( numbervars(V, 0, _, [singletons(true)]),
            numbervars(E, 0, _, [singletons(true)]),
            term_diff(V, E)
          ).


                /*******************************
                *            FILES             *
                *******************************/

%!  garp_test(=Model, =Test) is nondet.
%
%   True when Test is a test for Model.

garp_test(Model, Test) :-
    directory_member(tests, File,
                     [ recursive(true),
                       extensions([test])
                     ]),
    test_file(File, Model, Test).

%!  test_file(+File, -Model, -Test) is det.

test_file(File, Model, Test), atomic(File) =>
    split_string(File, "/", "/", Parts),
    append(_, [Model,Base], Parts),
    file_name_extension(Test, test, Base).
test_file(File, Model, Test), atomic(Model), atomic(Test) =>
    directory_file_path(tests, Model, ModelDir),
    file_name_extension(Test, test, TestFile),
    directory_file_path(ModelDir, TestFile, File).

model_title(Model, Title) :-
    directory_file_path(tests, Model, Dir),
    directory_file_path(Dir, 'Title.txt', TitleFile),
    read_file_to_string(TitleFile, Title0, [encoding(utf8)]),
    split_string(Title0, "", "\n \t", [Title]).

                /*******************************
                *          WEB ACCESS          *
                *******************************/

:- http_handler(garp(tests), show_tests, [id(show_tests)]).
:- http_handler(htmx(test),  run_test,   [id(run_test)]).

show_tests(_Request) :-
    reply_html_page([ title('GarpN -- tests'),
                      link([rel(stylesheet), href('/garp/simulator.css')]),
                      link([rel(icon), type('image/png'), sizes('32x32'),
                            href('https://www.swi-prolog.org/icons/favicon.ico')])
                    ],
                    [ \show_tests,
                      \scripts
                    ]).

show_tests -->
    { tests_by_model(ByModel)
    },
    html([ h1('GarpN -- Tests'),
           ul(\sequence(model_tests, ByModel))
         ]).

model_tests(Model-Tests) -->
    { model_title(Model, Title) },
    html([ li(b(Title)),
           ul(style('list-style-type:none'), \sequence(model_test(Model), Tests))
         ]).

model_test(Model, Test) -->
    { http_link_to_id(home, [model(Model), test(Test)], ViewModel),
      http_link_to_id(run_test, [model(Model), test(Test)], RunTest),
      variant_sha1(Model+Test, Hash),
      atom_concat(r, Hash, TestResultId),
      atom_concat('#', TestResultId, HXTarget)
    },
    html(li([ a([ href(ViewModel),
                  title('View model'),
                  class(button)
                ],
                '👁️'),
              a([ href(RunTest),
                  'hx-get'(RunTest),
                  'hx-target'(HXTarget),
                  title('Run test'),
                  class(button)
                ],
                '▶️'),
              ' ',
              Test,
              div(id(TestResultId), [])
            ])).

%!  run_test(+Request) is det.
%
%   Run a test, presenting the results in the browser.

run_test(Request) :-
    http_parameters(Request,
                    [ model(Model, []),
                      test(Test, [])
                    ]),
    with_output_to(string(Ansi),
                   ignore(garp_test_model_test(Model, Test, [])),
                   [ capture([user_output,user_error]),
                     color(true)
                   ]),
    ansi_to_html(Ansi, HTML),
    reply_htmx(pre(HTML)).

%!  ansi_to_html(+Ansi, -HTML) is det.
%
%   Convert ANSI color sequences to HTML.

ansi_to_html(Ansi, HTML) :-
    re_split("\e\\[\\d+(;\\d+)*m", Ansi, Parts),
    phrase(to_html(HTML), Parts).

to_html([Plain,Span|T]) -->
    [Plain],
    [Start, Text, "\e[0m"], !,
    {to_style(Start, Text, Span)},
    to_html(T).
to_html(Last) --> remainder(Last).

to_style(Start, Text, Span) :-
    string_concat("\e[", S1, Start),
    string_concat(S2, "m", S1),
    split_string(S2, ";", "", Parts),
    maplist(number_string, Args, Parts),
    sgr_style(Args, Text, Span).

sgr_style([], Text, Text).
sgr_style([1|T], Text, Span) :-
    !,
    sgr_style(T, b(style('white-space:pre-wrap;'),Text), Span).
sgr_style([FG|T], Text, Span) :-
    between(30,39,FG),
    Color is FG-30,
    ansi_term:ansi_color(Name, Color),
    format(string(Style), 'white-space:pre-wrap;color:~w;', [Name]),
    sgr_style(T, span(style(Style), Text), Span).



                /*******************************
                *           FEEDBACK           *
                *******************************/

progress(Term, _Options) :-
    print_message(informational, garp_test(Term)).

:- multifile
    prolog:message//1.

prolog:message(garp_test(Msg)) -->
    message(Msg).

message(start_model(Model)) -->
    { model_title(Model, Title)
    },
    [ 'Tests for ', ansi(code, '~w', [Title]), ' ...' ].
message(start_test(_Model, Test)) -->
    [ ' Test ', ansi(code, '~q', [Test]), ' ...' ].
message(mapping_quality(PerfectCnt, 0, 0)) -->
    [ '   ✅ Matched ~D states'-[PerfectCnt] ].
message(mapping_quality(PerfectCnt, AmbiguousCnt, 0)) -->
    [ '   🟡 Matched ~D states; ~D ambiguous'-[PerfectCnt,AmbiguousCnt] ].
message(mapping_quality(PerfectCnt, AmbiguousCnt, UnmappedCnt)) -->
    { PerfectCnt > (PerfectCnt+AmbiguousCnt+UnmappedCnt)*0.75 },
    [ '   🟡 Matched ~D states; ~D ambiguous, ~D unmatched'-[PerfectCnt,AmbiguousCnt,UnmappedCnt] ].
message(mapping_quality(PerfectCnt, AmbiguousCnt, UnmappedCnt)) -->
    [ '   🔴 Matched ~D states; ~D ambiguous, ~D unmatched'-[PerfectCnt,AmbiguousCnt,UnmappedCnt] ].


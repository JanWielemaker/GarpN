:- module(diff,
          [ term_diff/2
          ]).
:- use_module(library(ansi_term)).
:- use_module(library(apply)).
:- use_module(library(pprint)).
:- use_module(library(terms)).

term_diff(T10, T20) :-
    mapsubterms(canonical, T10, T1),
    mapsubterms(canonical, T20, T2),
    term_subsumer(T1, T2, Gen),
    diff_term(Gen, T1, T2, DiffTerm),
    print_term(DiffTerm, []).

:- det(diff_term/4).
diff_term(Gen, T1, _T2, Diff), Gen == T1 =>
    Diff = Gen.
diff_term(Gen, T1, T2, Diff), var(Gen) =>
    Diff = '$diff$'(T1,T2).
diff_term(Gen, T1, T2, Diff) =>
    compound_name_arguments(Gen, Name, ArgsGen),
    compound_name_arguments(T1, Name, ArgsT1),
    compound_name_arguments(T2, Name, ArgsT2),
    maplist(diff_term, ArgsGen, ArgsT1, ArgsT2, ArgsDiff),
    compound_name_arguments(Diff, Name, ArgsDiff).

user:portray('$diff$'(T1,T2)) :-
    ansi_format(fg(red), '~@', [print_term(T1, [output(current_output)])]),
    ansi_format([fg(blue)], ' \U0001f846 ', []),
    ansi_format(fg(green), '~@', [print_term(T2, [output(current_output)])]).

canonical(system_elements(List), system_elements(Ordered)) :-
    sort(List, Ordered).
canonical(parameters(List), parameters(Ordered)) :-
    sort(List, Ordered).
canonical(par_values(List), par_values(Ordered)) :-
    sort(List, Ordered).
canonical(List, Sorted) :-
    is_list(List),
    unordered_element(El),
    maplist(subsumes_term(El), List),
    !,
    sort(List, Sorted).

unordered_element(_/_).
unordered_element(relation(_,_,_)).
unordered_element(instance(_,_)).
unordered_element(value(_,_,_,_)).

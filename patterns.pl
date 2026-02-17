:- module(patterns,
          [ final_cycle/3,                      % ++List, -Cycle:list, -CycleCount
            canonical_cycle/2                   % +Cycle0, -Cycle
          ]).
:- autoload(library(lists), [reverse/2, append/3, min_member/2]).
:- autoload(library(aggregate), [foldall/4]).

/** <module> Various algorithms on lists (of states)
*/

%!  final_cycle(+List:list, -Cycle:list, -CycleCount) is semidet.
%
%   Given a List (of states) that starts  with an arbitrary sequence and
%   then starts showing repeating (cyclic)   behavior,  where there last
%   cycle is cut of arbitrarily, find the   cyclic pattern. If the cycle
%   is short we demand more repeats.
%
%   @arg Cycle is a _canonical cycle_.
%   @arg CycleCount is the number of full   cycles found. The minimum is
%   2.

final_cycle(List, Cycle, Repeats) :-
    reverse(List, RevList),
    rcycle(RevList, RCycle, Repeats),
    reverse(RCycle, Cycle0),
    canonical_cycle(Cycle0, Cycle).

rcycle(List, Cycle, Repeats) :-
    length(List, N),
    Max is N // 2,
    between(2, Max, CLen),
    take_upto(CLen, List, Repeating),
    length(Cycle, CLen),
    append(Cycle, Rest, Repeating),
    count_cycles(Cycle, Rest, 1, Repeats),
    Repeats >= 2,
    !.

take_upto(_, List, List).
take_upto(N, [_|List], Rest) :-
    N > 0,
    N1 is N-1,
    take_upto(N1, List, Rest).

count_cycles(Cycle, List, C0, C) :-
    append(Cycle, Rest, List),
    !,
    C1 is C0+1,
    count_cycles(Cycle, Rest, C1, C).
count_cycles(_, _, C, C).

%!  canonical_cycle(+Cycle0, -Cycle) is det.
%
%   Get the canonical form of a cycle. The canonical cycle is defined to
%   the the cycle that is lowest  in   the  standard  order of terms. As
%   standard order is left-to-right, we first   find  the minimum of the
%   list, allowing the cycle to start at any occurrence of the minimum.

canonical_cycle(Cycle0, Cycle) :-
    foldall(std_min(C), canonical_cycle_(Cycle0, C),
            c(_,_,_), % c/3 is higher in order than a list
            Cycle).

canonical_cycle_(Cycle0, Cycle) :-
    min_member(Min, Cycle0),
    append(Pre, [Min|Post], Cycle0),
    append([Min|Post], Pre, Cycle).

std_min(A, B, Min) :-
    compare(Order, A, B),
    std_min_(Order, A, B, Min).

std_min_(<, A, _, A).
std_min_(=, A, _, A).
std_min_(>, _, A, A).

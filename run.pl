:- use_module(gui).
:- use_module(map).
:- if(current_prolog_flag(dynalearn, true)).
:- use_module(dynalearn).
:- else.
:- ensure_loaded('../Garp3-v1.5.2/startup').
:- use_module(garp3).
:- endif.

:- if(exists_source(library(in_make))).
:- use_module(library(in_make)).
:- in_make.
:- endif.

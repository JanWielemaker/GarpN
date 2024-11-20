:- use_module(gui).
:- ensure_loaded('../Garp3-v1.5.2/startup').
:- use_module(map).

:- if(exists_source(library(in_make))).
:- use_module(library(in_make)).
:- in_make.
:- endif.

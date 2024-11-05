:- module(equations,
          [ equations//1
          ]).
:- use_module(library(dcg/high_order)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).

:- html_resource(mathlive,
                 [ virtual(true),
                   requires([ 'https://unpkg.com/mathlive'
                            ])
                 ]).
:- html_resource('https://unpkg.com/mathlive',
                 [ mime_type(text/javascript)
                 ]).

%!  equations(+Equations:list)//
%
%   Render a list of equations as a div holding mathlive expressions.

equations(Eqs) -->
    html_requires(mathlive),
    html(div(class(equations),
             \sequence(equation, Eqs))).

equation(Eq) -->
    { phrase(eq_to_mathjax(Eq), Codes),
      string_codes(String, Codes)
    },
    html(div(class(equation),
             'math-field'(String))).

eq_to_mathjax(Left := Right) ==>
    !,
    quantity(Left),
    "=",
    expression(Right).

quantity(Q) -->
    format('\\text{~q}', [Q]).

expression(A + B) ==> expression(A), "+", expression(B).
expression(A - B) ==> expression(A), "-", expression(B).
expression(A * B) ==> expression(A), "\\times ", expression(B).
expression(A / B) ==> "\\frac{", expression(A), "}{", expression(B), "}".
expression(Q), ground(Q) ==> quantity(Q).

format(Fmt, Args, Head, Tail) :-
    format(codes(Head, Tail), Fmt, Args).
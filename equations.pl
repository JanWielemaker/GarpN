:- module(equations,
          [ equations//2,               % +Term, +Options
            latex_to_prolog_source/2    % +LaTeX:list(string), -Source:string
          ]).
:- use_module(library(dcg/high_order)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(http/html_head)).
:- use_module(library(dcg/basics)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(listing)).

:- html_resource(mathlive,
                 [ virtual(true),
                   ordered(true),
                   requires([ 'https://unpkg.com/mathlive',
                              %'https://unpkg.com/@cortex-js/compute-engine',
                              %'/garp/mathlive.js',
                              '/garp/equations.js'
                            ])
                 ]).
:- html_resource('https://unpkg.com/mathlive',
                 [ mime_type(text/javascript)
                 ]).
:- html_resource('https://unpkg.com/@cortex-js/compute-engine',
                 [ mime_type(text/javascript)
                 ]).

%!  equations(+Equations:list, +Options)//
%
%   Render a list of equations as a div holding mathlive expressions.

equations(Eqs, Options) -->
    { order_equations(Eqs, Eqs1, Options)
    },
    html_requires(mathlive),
    html(div([ id(equations),
               'hx-post'('/garp/htmx/analyze'),
               'hx-vals'('js:{"ml_data": ml_value()}'),
               'hx-trigger'('input delay:1000ms'),
               'hx-target'('#quantity_controls'),
               'hx-ext'('json-enc'),
               class(equations)
             ],
             \sequence(equation, Eqs1))),
    js_script({|javascript||
               ml_init();
              |}).

order_equations(Equations0, Equations, Options),
    option(grouped(true), Options) =>
    group_equations(Equations0, Groups), % list(Type-Equations)
    pairs_values(Groups, Eql),
    append(Eql, Equations).
order_equations(Equations0, Equations, _Options) =>
    Equations = Equations0.

equation(Eq) -->
    { phrase(eq_to_mathjax(Eq), Codes),
      string_codes(String, Codes),
      debug(eq(to_latex), '~p --> ~p', [Eq, String])
    },
    html(div(class(equation),
             'math-field'(String))).

eq_to_mathjax(Left := Right) ==>
    !,
    quantity(Left),
    "=",
    expression(Right, 699).

quantity(placeholder(Name, Init)), number(Init) ==>
    format('\\placeholder[~w]{~W}', [Name, Init, [float_format('~999h')]]).
quantity(placeholder(Name, Init)), var(Init) ==>
    format('\\placeholder[~w]{?}', [Name]).
quantity(Q), compound(Q), Q =.. [A,E] ==>
    format('\\prop{~w}{~w}', [A,E]).
quantity(Q), atom(Q) ==>
    format('\\variable{~w}', [Q]).
quantity(Q), number(Q) ==>
    format('\\placeholder[c]{~W}', [Q, [float_format('~999h')]]).

expression(Exp, Pri), pri(Exp, EPri), EPri > Pri ==>
    "(", expression(Exp, 1200), ")".
expression(Exp, _) ==>
    expression(Exp).

expression(-(A))  ==> "-", expression(A, 200).
expression(A + B) ==> expression(A, 500), " + ", expression(B, 499).
expression(A - B) ==> expression(A, 500), " - ", expression(B, 499).
expression(A * B) ==> expression(A, 400), " \\cdot ", expression(B, 399).
expression(A / B) ==> "\\frac{", expression(A), "}{", expression(B), "}".
expression(A ^ B) ==> expression(A, 199), "^{", const_expression(B, 200), "}".
expression(placeholder(Name, Init)), var(Init) ==>
    format('\\placeholder[~w]{?}', [Name]).
expression(Q), ground(Q) ==> quantity(Q).

const_expression(C, _Pri), number(C) ==>
    format('~W', [C, [float_format('~999h')]]).
const_expression(E, Pri) ==>
    expression(E, Pri).

%!  pri(+Exp, -Pri) is semidet.

pri(-(_), 200).
pri(_+_, 500).
pri(_-_, 500).
pri(_*_, 400).
pri(_^_, 200).

format(Fmt, Args, Head, Tail) :-
    format(codes(Head, Tail), Fmt, Args).


		 /*******************************
		 *            PARSE		*
		 *******************************/

%!  latex_to_prolog_source(+LaTeX, -Source:string) is det.
%
%   Translate MathLive LaTeX output to a source text.
%
%   @arg LaTeX is either a list of strings or a string
%   where the equations are separated by \v (vertical tab)

latex_to_prolog_source(LaTeX, Source), is_list(LaTeX) =>
    maplist(latex_prolog, LaTeX, Prolog),
    with_output_to(string(Source),
                   maplist(portray_clause, Prolog)).
latex_to_prolog_source(LaTeX, Source) =>
    split_string(LaTeX, "\v", "", Equations),
    latex_to_prolog_source(Equations, Source).

%!  latex_prolog(+LaTeX:string, -Prolog:term) is det.

latex_prolog(LaTeX, Prolog) :-
    parse_latex(LaTeX, LaTexCmd),
    phrase(latex_prolog(Prolog), LaTexCmd),
    !,
    debug(eq(from_prolog), '~p --> ~p', [LaTeX, Prolog]).
latex_prolog(LaTeX, _) :-
    throw(error(latex_syntax(LaTeX), _)).

latex_prolog(Q:=Expr) -->               % TBD: decide on := vs =
    latex_var(Q), latex_symbol(=), latex_expression(Expr).

latex_var(Q) -->
     latex_quantity(Q), !.
latex_var(Q) -->
     latex_variable(Q).

latex_expression(Expr) -->
    latex_symbol('('),
    !,
    latex_expression(Expr),
    latex_symbol(')').
latex_expression(Expr) -->
    latex_whites,
    latex_add_expression(Left),
    (   add_op(Op)
    ->  latex_expression(Right),
        { Expr =.. [Op,Left,Right] }
    ;   { Expr = Left }
    ).

latex_add_expression(Expr) -->
    latex_whites,
    latex_mul_expression(Left),
    (   mul_op(Op)
    ->  latex_add_expression(Right),
        { Expr =.. [Op,Left,Right] }
    ;   { Expr = Left }
    ).

latex_mul_expression(Expr) -->
    latex_whites,
    latex_exp_expression(Left),
    (   exp_op(Op)
    ->  latex_mul_expression(Right),
        { Expr =.. [Op,Left,Right] }
    ;   { Expr = Left }
    ).

latex_exp_expression(Expr) -->
    latex_var(Expr),
    !.
latex_exp_expression(Expr) -->
    latex_number(Expr),
    !.
latex_exp_expression(-Expr) -->
    latex_symbol(-),
    !,
    latex_exp_expression(Expr).
latex_exp_expression(Expr) -->
    cmd_expresion(Expr).

add_op(+) --> latex_symbol(+), !.
add_op(-) --> latex_symbol(-).

mul_op(*) --> latex_whites, [cdot()], !, latex_whites.

exp_op(^) --> latex_symbol(^).

cmd_expresion(L/R) -->
    [frac(EL, ER)],
    { phrase(latex_expression(L), EL),
      phrase(latex_expression(R), ER)
    }.
cmd_expresion(Expr) -->
    [placeholder(_Id, Content)],
    { phrase(latex_expression(Expr), Content)
    }.

latex_quantity(Q) -->
    [prop(LtxAtt,LtxEnt)],
    { phrase(latex_name(Att), LtxAtt),
      phrase(latex_name(Ent), LtxEnt),
      Q =.. [Att,Ent]
    }.

latex_variable(V) -->
    [variable(LtxName)],
    { phrase(latex_name(V), LtxName) }.

latex_symbol(Code) -->
    latex_whites,
    [Code],
    latex_whites.

latex_name(Name) -->
    latex_whites,
    [First],
    { string(First) },
    prolog_name_followups(Parts),
    { atomic_list_concat([First|Parts], Name) },
    latex_whites.

prolog_name_followups([H|T]) -->
    prolog_name_part(H),
    !,
    prolog_name_followups(T).
prolog_name_followups([]) -->
    [].

prolog_name_part(String) --> [String], {string(String)}, !.
prolog_name_part('_') --> ['_'], !.
prolog_name_part(Int) --> [Int], {integer(Int)}, !.

latex_number(Value) -->
    latex_whites, [Value], { number(Value) }, !, latex_whites.
latex_number(Value) -->
    [placeholder(_Id,Arg)],
    { phrase(placeholder_value(Value), Arg) }.

placeholder_value(Value) -->
    latex_number(Value),
    !.
placeholder_value(_) -->
    latex_symbol(?),
    !.

latex_whites --> latex_white, !, latex_whites.
latex_whites --> [].

latex_white -->
    [blanks(_)].

%!  parse_latex(+LaTeX:string, -LaTexCmd:list) is det.

parse_latex(String, LaTexCmd) :-
    string_codes(String, Codes),
    phrase(latex(eos, LaTexCmd), Codes).

latex(End, List) -->
    is_end(End),
    !,
    { List = [] }.
latex(End, [H|T]) -->
    latex_1(H),
    latex(End, T).

latex_1(Token) -->
    latex_cmd(Token), !.
latex_1(blanks(Blanks)) -->
    blank(H), !,
    blanks(T),
    { string_codes(Blanks, [H|T]) }.
latex_1(Number) -->
    number(Number),
    !.
latex_1(Word) -->
    alpha(H),
    !,
    alphas(T),
    { string_codes(Word, [H|T]) }.
latex_1(Char) -->
    [C],
    { char_code(Char, C) }.

is_end(eos) --> eos, !.
is_end(String), [C]--> [C], {string_code(_, String, C)}, !.

latex_cmd(Command) -->
    "\\", ltx_name(Name), ltx_args(Name, Args),
    { compound_name_arguments(Command, Name, Args) }.

ltx_name(Name) -->
    csym(Name).

ltx_args(prop,        Args) ==> ltx_nargs(2, Args).
ltx_args(variable,    Args) ==> ltx_nargs(1, Args).
ltx_args(cdot,        Args) ==> {Args = []}.
ltx_args(placeholder, Args) ==> opt_arg(Name), curl_arg(Value),
				{Args = [Name,Value]}.
ltx_args(_,           Args) ==> curl_args(Args).

ltx_nargs(0, []) --> !.
ltx_nargs(N, [H|T]) --> ltx_arg(H), {N1 is N-1}, ltx_nargs(N1, T).

ltx_arg(Arg) --> curl_arg(Arg), !.

curl_args([H|T]) -->
    peek('{'),
    !,
    curl_arg(H),
    curl_args(T).
curl_args([]) -->
    [].

curl_arg(Arg) --> "{", !, latex("}", Arg), "}".
opt_arg(Arg) --> "[", !, latex("]", Arg), "]".

peek(Char), [C] --> [C], {char_code(Char,C)}.

blanks([H|T]) -->
    blank(H),
    !,
    blanks(T).
blanks([]) -->
    [].

blank(C) -->
    [C],
    { nonvar(C),
      code_type(C, space)
    }.

alphas([H|T]) -->
    alpha(H),
    !,
    alphas(T).
alphas([]) -->
    [].

alpha(C) -->
    [C],
    { nonvar(C),
      code_type(C, alpha)
    }.

		 /*******************************
		 *       LOGICAL GROUPING	*
		 *******************************/

%!  group_equations(+List, -Groups) is det.
%
%   Group the equations by type.

:- det(group_equations/2).
group_equations(Eql, Groups) :-
    map_list_to_pairs(classify_equation, Eql, Pairs),
    keysort(Pairs, Sorted),
    group_pairs_by_key(Sorted, Groups0),
    maplist(label_group, Groups0, Groups).

classify_equation(t := _, Order) =>
    eq_type_order(time, Order).
classify_equation('Δt' := _, Order) =>
    eq_type_order(time, Order).
classify_equation(_Left := Number, Order), number(Number) =>
    eq_type_order(init_value, Order).
classify_equation(_Left := placeholder(Type,_), Order) =>
    eq_type_order(Type, Order).
classify_equation(_, Order) =>
    eq_type_order(formula, Order).

label_group(Order-Eql, Type-Eql) :-
    eq_type_order(Type, Order),
    !.

eq_type_order(formula,    1).
eq_type_order(time,       2).
eq_type_order(constant,   3).
eq_type_order(init,       4).
eq_type_order(init_value, 5).

		 /*******************************
		 *            MESSAGES		*
		 *******************************/

:- multifile prolog:error_message//1.

prolog:error_message(latex_syntax(String)) -->
    [ 'Invalid equation: ~s'-[String] ].

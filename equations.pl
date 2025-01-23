:- module(equations,
          [ equations//2,               % +Term, +Options
            latex_to_prolog_source/2,   % +LaTeX:list(string), -Source:string
            latex_to_prolog/2,          % +LaTeX, -Prolog:list(term)
            latex_to_prolog_ex/2        % +LaTeX, -Prolog:list(term)
          ]).
:- use_module(library(dcg/high_order)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(http/html_head)).
:- use_module(library(dcg/basics)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(listing)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(pairs)).

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
    { order_equations(Eqs, Eqs1, Options),
      quantities(Quantities, Options)
    },
    html_requires(mathlive),
    html(div([ id(equations),
               'hx-post'('/garp/htmx/analyze'),
               'hx-vals'('js:{"ml_data": ml_value()}'),
               'hx-trigger'('input delay:200ms'),
               'hx-target'('#quantity_controls'),
               'hx-ext'('json-enc'),
               class(equations)
             ],
             \eq_list(Eqs1, Options))),
    js_script({|javascript(Quantities)||
               ml_init(Quantities);
              |}).

order_equations(Equations0, Equations, Options),
    option(grouped(true), Options) =>
    group_equations(Equations0, Groups), % list(Type-Equations)
    pairs_values(Groups, Eql),
    append(Eql, Equations).
order_equations(Equations0, Equations, _Options) =>
    Equations = Equations0.

eq_list(Eqs, _Options) -->
    sequence(equation, Eqs),
    html(div(class('add-equation'),
             [ +
             ])).

equation(Eq) -->
    { phrase(eq_to_mathjax(Eq), Codes),
      string_codes(String, Codes),
      debug(eq(to_latex), '~p --> ~p', [Eq, String])
    },
    html(div(class(equation),
             [ 'math-field'(String),
               span(class('delete-equation'), '\u2716')
             ])).

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

%!  quantities(-Quantities:list(string), +Options) is det.
%
%   Produce LaTeX strings for all quantities.

quantities(List, Options) :-
    option(id_mapping(Mapping), Options, _{}),
    dict_pairs(Mapping, _, Pairs),
    pairs_values(Pairs, Values),
    maplist(quantity_string, Values, List0),
    append(List0,
           [ "\\variable{t}"
           ], List).

quantity_string(Q, S) :-
    phrase(quantity(Q), Codes),
    string_codes(S, Codes).


		 /*******************************
		 *            PARSE		*
		 *******************************/

test_eq :-
    read_line_to_string(user_input, LaTeX),
    asserta(user:latex(LaTeX)),
    gtrace,
    latex_prolog(LaTeX, Prolog),
    print_term(Prolog, []).

%!  latex_to_prolog(+LaTeX, -Prolog:list(term)) is det.
%!  latex_to_prolog_ex(+LaTeX, -Prolog:list(term)) is det.
%
%   Translate MathLive LaTeX  output  into  Prolog.   If  the  LaTeX  is
%   invalid,  latex_to_prolog/2  returns  a  dict    for  every  invalid
%   equation. latex_to_prolog_ex/2 only returns   valid Prolog equations
%   and throws invalid_model(Errors) in case one   or more equations are
%   invalid.
%
%   @arg LaTeX is either a list of strings or a string
%   where the equations are separated by \v (vertical tab)

latex_to_prolog(LaTeX, Prolog), is_list(LaTeX) =>
    foldl(latex_prolog, LaTeX, Prolog, 1, _).
latex_to_prolog(LaTeX, Prolog) =>
    split_string(LaTeX, "\v", "", Equations),
    latex_to_prolog(Equations, Prolog).

latex_to_prolog_ex(LaTeX, Prolog) :-
    latex_to_prolog(LaTeX, Prolog),
    include(is_dict, Prolog, Errors),
    (   Errors == []
    ->  true
    ;   throw(invalid_model(Errors))
    ).

%!  latex_to_prolog_source(+LaTeX, -Source:string) is det.
%
%   Translate  a  correct   set   of    equations   into   their  Prolog
%   representation as a string.

latex_to_prolog_source(LaTeX, Source) :-
    latex_to_prolog_ex(LaTeX, Prolog),
    with_output_to(string(Source),
                   maplist(portray_clause, Prolog)).

%!  latex_prolog(+LaTeX:string, -Prolog:term, +Line0, -Line) is det.

latex_prolog(LaTeX, Prolog, N0, N) :-
    N is N0+1,
    latex_prolog(LaTeX, Prolog),
    !,
    debug(eq(from_prolog), '~p --> ~p', [LaTeX, Prolog]).
latex_prolog(LaTeX, error{ line: N0,
                           latex:LaTeX,
                           message: "Invalid equation"
                         }, N0, N) :-
    N is N0+1.

latex_prolog(LaTeX, Prolog) :-
    parse_latex(LaTeX, LaTexCmd),
    phrase(latex_prolog(Prolog), LaTexCmd),
    !.

latex_prolog(Q:=Expr) -->               % TBD: decide on := vs =
    latex_var(Q), latex_symbol(=), latex_expression(Expr).

latex_var(Q) -->
     latex_quantity(Q), !.
latex_var(Q) -->
     latex_variable(Q).

latex_expression(Expr) -->
    latex_expression(Expr, 1200, _).

%!  latex_expression(-Expr, +MaxPri, -Pri)//

latex_expression(Expr, MaxPri, Pri) -->
    prefix_op(Op, Pri, MaxPriRight),
    { Pri =< MaxPri
    },
    latex_expression(Right, MaxPriRight, _),
    { Expr =.. [Op,Right]
    }.
latex_expression(Expr, MaxPri, Pri) -->
    string(Tokens),
    infix_op(Op, MaxPriLeft, Pri, MaxPriRight),
    { debug(latex(op), 'Infix ~p; left ~p', [Op, Tokens]),
      Pri =< MaxPri,
      phrase(latex_expression(Left, MaxPriLeft, _), Tokens)
    },
    latex_expression(Right, MaxPriRight, _),
    { Expr =.. [Op,Left,Right]
    }.
latex_expression(L/R, MaxPri, 400) -->
    [frac(EL, ER)],
    { 400 =< MaxPri,
      phrase(latex_expression(L), EL),
      phrase(latex_expression(R), ER)
    }.
latex_expression(Expr, _, 0) -->
    embraced_expression(Expr),
    !.
latex_expression(Expr, _, 0) -->
    latex_var(Expr),
    !.
latex_expression(Expr, _, 0) -->
    latex_number(Expr),
    !.
latex_expression(Expr, _, 0) -->
    cmd_expression(Expr).

embraced_expression(Expr) -->
    [group(LaTeX)],
    !,
    { phrase(latex_expression(Expr), LaTeX) }.
embraced_expression(Expr) -->
    latex_symbol('('),
    !,
    latex_expression(Expr),
    latex_symbol(')'),
    !.
embraced_expression(Expr) -->
    latex_whites, [left("(")], latex_whites,
    latex_expression(Expr),
    latex_whites, [right(")")],
    !.

cmd_expression(pi)     --> [pi()],  !.
cmd_expression(sin(X)) --> [sin()], !, latex_expression(X).
cmd_expression(cos(X)) --> [cos()], !, latex_expression(X).
cmd_expression(tan(X)) --> [tan()], !, latex_expression(X).
cmd_expression(log(X)) --> [log()], !, latex_expression(X).
cmd_expression(sqrt(X)) -->
    [sqrt(Ltx)],
    { phrase(latex_expression(X), Ltx) }.
cmd_expression(Expr) -->
    [placeholder(_Id, Content)],
    { phrase(latex_expression(Expr), Content)
    }.

latex_quantity(Q) -->
    [prop(LtxAtt,LtxEnt)],
    !,
    { phrase(latex_name(Att), LtxAtt),
      phrase(latex_name(Ent), LtxEnt),
      Q =.. [Att,Ent]
    }.
latex_quantity(Q) -->
    [','(), text(LtxAtt), ^, group([text(LtxEnt)]), ','(), group([])],
    { phrase(latex_name(Att), LtxAtt),
      phrase(latex_name(Ent), LtxEnt),
      Q =.. [Att,Ent]
    }.

latex_variable(V) -->
    [variable(LtxName)],
    !,
    { phrase(latex_name(V), LtxName) }.
latex_variable(V) -->
    [','(), text(LtxName)],
    !,
    { phrase(latex_name(V), LtxName) }.
latex_variable(V) -->
    [ Name ],
    { string(Name),
      atom_string(V, Name)
    }.

prefix_op(Op, OpPri, RightPri) -->
    latex_symbol(Op),
    { c_prefix_op(Op, OpPri, RightPri)
    }.

c_prefix_op(+, 200, 200).
c_prefix_op(-, 200, 200).

infix_op(*, LeftPri, OpPri, RightPri) -->
    latex_whites, [cdot()],
    { c_infix_op(*, LeftPri, OpPri, RightPri)
    }.
infix_op(Op, LeftPri, OpPri, RightPri) -->
    latex_symbol(Op),
    { c_infix_op(Op, LeftPri, OpPri, RightPri)
    }.

c_infix_op(+, 500, 500, 499).
c_infix_op(-, 500, 500, 499).
c_infix_op(*, 400, 400, 399).
c_infix_op(/, 400, 400, 399).
c_infix_op(^, 199, 200, 200).

latex_symbol(Code) -->
    latex_whites,
    [Code], {atom(Code)},
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
prolog_name_part(String) --> [blanks(String)], !.
prolog_name_part(Atom) --> [Atom], {atom(Atom)}, !.
prolog_name_part('_') --> ['_'()], !.
prolog_name_part(Int) --> [Int], {integer(Int)}, !.

latex_number(Value) -->
    latex_whites, [Value], { number(Value) }, !, latex_whites.
latex_number(Value) -->
    [placeholder(Id,Arg)],
    { phrase(placeholder_value(Id, Value), Arg) }.

placeholder_value(_, Value) -->
    latex_number(Value),
    !.
placeholder_value(LtxId, placeholder(Id,_)) -->
    latex_symbol(?),
    !,
    { phrase(latex_name(Id), LtxId) }.

latex_whites --> latex_white, !, latex_whites.
latex_whites --> [].

latex_white --> [blanks(_)], !.

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
latex_1(group(LaTeX)) -->
    "{",
    !,
    latex("}", LaTeX), "}".
latex_1(blanks(Blanks)) -->
    blank(H), !,
    blanks(T),
    { string_codes(Blanks, [H|T]) }.
latex_1(Number) -->
    peek_float,
    number(Number),
    !.
latex_1(Number) -->
    peek(D),
    { char_type(D, digit) },
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

peek_float, [0'0,0'.,D] -->
    ".", [D],
    { code_type(D, digit) }.

is_end(eos) --> !, eos.
is_end(String), [C]--> [C], {string_code(_, String, C)}, !.

latex_cmd(Command) -->
    "\\", ltx_name(Name), !, blanks, ltx_args(Name, Args),
    { compound_name_arguments(Command, Name, Args) }.
latex_cmd(Command) -->
    "\\", [C],
    { atom_codes(Name, [C]),
      compound_name_arguments(Command, Name, [])
    }.

ltx_name(Name) -->
    ltx_name_char(H),
    ltx_name_chars(T),
    { atom_codes(Name, [H|T]) }.

ltx_name_chars([H|T]) --> ltx_name_char(H), !, ltx_name_chars(T).
ltx_name_chars([]) --> [].

ltx_name_char(C) -->
    [C],
    { (   between(0'a, 0'z, C)
      ;   between(0'Z, 0'Z, C)
      )
    }, !.

ltx_args(prop,        Args) ==> ltx_nargs(2, Args).
ltx_args(variable,    Args) ==> ltx_nargs(1, Args).
ltx_args(cdot,        Args) ==> {Args = []}.
ltx_args(left,        Args) ==> ltx_nargs(1, Args).
ltx_args(right,       Args) ==> ltx_nargs(1, Args).
ltx_args(frac,        Args) ==> ltx_nargs(2, Args).
ltx_args(placeholder, Args) ==> opt_arg(Name), curl_arg(Value),
				{Args = [Name,Value]}.
ltx_args(_,           Args) ==> curl_args(Args).

ltx_nargs(0, []) --> !.
ltx_nargs(N, [H|T]) --> ltx_arg(H), {N1 is N-1}, ltx_nargs(N1, T).

ltx_arg(Arg) --> curl_arg(Arg), !.
ltx_arg([Num]) --> digit(D), !, { number_codes(Num, [D]) }.
ltx_arg([Arg]) --> [C], { string_codes(Arg, [C]) }.

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
		 *          EXCEPTIONS		*
		 *******************************/
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This maps the invalid_model(+JSON) into  an   HTML  document suitable as
htmx  reply.  The  'hx-target-error'('#errors')`  statement  in  home//0
ensures this is forwarded to the #errors div, but in this case it simply
executes ml_errors(Data).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


:- multifile
    http:map_exception_to_http_status_hook/4.

http:map_exception_to_http_status_hook(
         invalid_model(Errors),
         html(Tokens),
         [ status(400) ],
         []) :-
    maplist(html_message, Errors, Errors1),
    phrase(js_script({|javascript(Errors1)||
                      ml_errors(Errors1);
                     |}), Tokens).

html_message(Error0, Error) :-
    _{message: Msg0, latex: LaTeX} :< Error0,
    phrase(html([ span(class(msg), Msg0),
                  span(class(latex), LaTeX)
                ]), Tokens),
    with_output_to(string(HTML), print_html(Tokens)),
    Error = Error0.put(html, HTML).

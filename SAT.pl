% Student exercise profile
:- set_prolog_flag(occurs_check, error).      % disallow cyclic terms
:- set_prolog_stack(global, limit(8000000)).  % limit term space (8Mb)
:- set_prolog_stack(local,  limit(2000000)).  % limit environment space

% 2025 -- Rodrigo Monteiro Junior
% SAT.pl

% -------------------------

sat(WFF, Result) :-
    nif(WFF, NIF),
    nnf(NIF, NNF),
    cnf(NNF, CNF),
    
    flatten_and(CNF, AndFlattened),
    maplist(flatten_or, AndFlattened, PreparedCNF),
    dpll(PreparedCNF).

% first pass: identify units
unit_propagation_1([[not(X)]|Xs], True, False, Result) :-
    !,
    unit_propagation_1(Xs, True, [X|False], Result).
unit_propagation_1([[X]|Xs], True, False, Result) :-
    unit_propagation(Xs, [X|True], False, Result).

flatten_or(or(P, Q), L) :-
    !,
    flatten_or(P, Pf),
    flatten_or(Q, Qf),
    append(Pf, Qf, L).
flatten_or(name(P), [P]) :-
    !.
flatten_or(not(name(P)), [not(P)]) :-
    !.
flatten_or(P, [P]).

flatten_and(and(P, Q), L) :-
    !,
    flatten_and(P, Pf),
    flatten_and(Q, Qf),
    append(Pf, Qf, L).
flatten_and(P, [P]).

% Normalized inference form

nif(bool(T), bool(T)).
nif(name(P), name(P)).

nif(implies(P, Q), or(not(P_), Q_)) :-
    nif(P, P_),
    nif(Q, Q_).

nif(not(P), not(P_)) :-
    nif(P, P_).

nif(and(P, Q), and(P_, Q_)) :-
    nif(P, P_),
    nif(Q, Q_).

nif(or(P, Q), or(P_, Q_)) :-
    nif(P, P_),
    nif(Q, Q_).

% -------------------------

% Normalized negation form

nnf(bool(T), bool(T)).
nnf(name(P), name(P)).

nnf(not(name(P)), not(name(P))).
nnf(not(bool(true)), bool(false)).
nnf(not(bool(false)), bool(true)).

nnf(not(not(P)), P_) :- nnf(P, P_).

nnf(not(and(P, Q)), or(NotP, NotQ)) :-
    nnf(not(P), NotP),
    nnf(not(Q), NotQ).

nnf(not(or(P, Q)), and(NotP, NotQ)) :-
    nnf(not(P), NotP),
    nnf(not(Q), NotQ).

nnf(and(P, Q), and(P_, Q_)) :-
    nnf(P, P_),
    nnf(Q, Q_).

nnf(or(P, Q), or(P_, Q_)) :-
    nnf(P, P_),
    nnf(Q, Q_).

% -------------------------

% conjunction normal form

cnf(name(P), name(P)).
cnf(bool(T), bool(T)).
cnf(not(P), not(P)).

cnf(or(or(P, Q), C), Result) :-
    !,
    cnf(or(P, Q), Inner),
    cnf(or(Inner, C), Result).

cnf(or(C, and(P, Q)), and(or(P_, C_), or(Q_, C_))) :-
    !,
    cnf(P, P_),
    cnf(Q, Q_),
    cnf(C, C_).

cnf(or(and(P, Q), C), and(or(P_, C_), or(Q_, C_))) :-
    !,
    cnf(P, P_),
    cnf(Q, Q_),
    cnf(C, C_).

cnf(or(P, Q), or(P_, Q_)) :-
    cnf(P, P_),
    cnf(Q, Q_).

cnf(and(P, Q), and(P_, Q_)) :-
    cnf(P, P_),
    cnf(Q, Q_).

% -------------------------

% lexer

lex([Token|Tokens]) -->
    whites,
    tok(Token), !,
    lex(Tokens).

lex([]) -->
    whites,
    [].

tok(name(P)) -->
    [C],
    { code_type(C, csymf),
      char_code(P, C)
    }, !.

tok(operator(O)) -->
    operator(O), !.

tok(bool(true)) --> [0'1], !.
tok(bool(false)) --> [0'0].


whites -->
    [C],
    { code_type(C, space)
    }, !,
    whites.
whites -->
    [].

% -----------------------------------

operator(lparen) -->
	[C],
    { code_type(C, paren(_))
    }, !.
operator(rparen) -->
	[C],
    { code_type(_, paren(C))
    }, !.
operator(not) -->
    [C],
    { char_code('~', C)
    }, !.
operator(and) -->
    [C],
    { char_code('&', C)
    }, !.
operator(or) -->
    [C],
    { char_code('|', C)
    }, !.
operator(implies) -->
    [C,C1],
    { char_code('-', C),
      char_code('>', C1)
    }.

% end of lexer ----------------------

% Parser

parse(Ast) -->
    expr(Ast).

% -----------------------------------

expr(A) -->
    term(T0),
    expr_r(T0, A).

expr_r(T0, A) -->
    [operator(and)], !,
    term(T1),
    expr_r(and(T0, T1), A).
expr_r(T0, A) -->
    [operator(or)], !,
    term(T1),
    expr_r(or(T0, T1), A).
expr_r(T0, A) -->
    [operator(implies)], !,
    term(T1),
    expr_r(implies(T0, T1), A).
expr_r(T, T) -->
    [].

term(not(F)) -->
    [operator(not)], !,
    factor(F).
term(F) -->
    factor(F).

factor(name(I)) -->
    [name(I)], !.

factor(bool(X)) -->
    [bool(X)], !.

factor(A) -->
    [operator(lparen)],
    expr(A),
    [operator(rparen)].

% -----------------------------------

/** <examples>

?- string_codes("p&q->c", Expr), time(phrase(lex(Tokens), Expr)), time(phrase(parse(Ast), Tokens)).
?- evaluate.

*/

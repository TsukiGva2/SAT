% Student exercise profile
:- set_prolog_flag(occurs_check, error).      % disallow cyclic terms
:- set_prolog_stack(global, limit(8000000)).  % limit term space (8Mb)
:- set_prolog_stack(local,  limit(2000000)).  % limit environment space

% 2025 -- Rodrigo Monteiro Junior
% SAT.pl

% -------------------------

sat(WFF, _Result) :-
    nif(WFF, NIF),
    nnf(NIF, NNF),
    cnf(NNF, CNF),
    
    bake(CNF, B),
    vars(B, [], _SYM).

% SAT
% -SAT is the set of attributions to the vars
dpll([], SAT, SAT, _) :- !.

% UNSAT
dpll(Phi, _, _, _) :-
    member([], Phi), !,
    fail.

dpll(Phi, Test, SAT, [Sym|Symbols]) :-
    !,
    assign(Phi, Sym, T, Assigned),
    dpll(Assigned, [T|Test], SAT, Symbols).

assign(Phi, Sym, t(Sym), Assigned) :-
    assign_true(Phi, Sym, [], Assigned).
assign(Phi, Sym, f(Sym), Assigned) :-
    assign_false(Phi, Sym, [], Assigned).

% False assignment ------------------------------------------
assign_false([Clause|Clauses], Sym, NewClauses, Assigned) :-
    member(not(Sym), Clause), !,
    assign_false(Clauses, Sym, NewClauses, Assigned).

assign_false([Clause|Clauses], Sym, NewClauses, Assigned) :-
    include({Sym}/[P]>>(P\=Sym), Clause, NoSym),
    assign_false(Clauses, Sym, [NoSym|NewClauses], Assigned).

assign_false([], _, NewClauses, NewClauses).
% -----------------------------------------------------------

% True assignment -------------------------------------------
assign_true([Clause|Clauses], Sym, NewClauses, Assigned) :-
    member(Sym, Clause), !,
    assign_true(Clauses, Sym, NewClauses, Assigned).

assign_true([Clause|Clauses], Sym, NewClauses, Assigned) :-
    include({Sym}/[P]>>(P\=not(Sym)), Clause, NoSym),
    assign_false(Clauses, Sym, [NoSym|NewClauses], Assigned).

assign_true([], _, NewClauses, NewClauses).
% -----------------------------------------------------------

vars([Clause|Clauses], Found, SYM) :-
    var_scan(Clause, Found, NewFound),
    vars(Clauses, NewFound, SYM).
vars([], Found, Found).

var_scan([not(P)|Rest], Found, NewFound) :-
    !,
    (   member(P, Found)
    ->  var_scan(Rest, Found, NewFound)
    ;   var_scan(Rest, [P|Found], NewFound)
    ).
var_scan([P|Rest], Found, NewFound) :-
    (   member(P, Found)
    ->  var_scan(Rest, Found, NewFound)
    ;   var_scan(Rest, [P|Found], NewFound)
    ).
var_scan([], Found, Found).

bake(CNF, BCNF) :-
    flatten_and(CNF, AndFlattened),
    maplist(flatten_or, AndFlattened, BCNF).

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

cnf(or(P, Q), Result) :-
    cnf(P, P_),
    cnf(Q, Q_),
    distribute_or(P_, Q_, Result).

cnf(and(P, Q), and(P_, Q_)) :-
    cnf(P, P_),
    cnf(Q, Q_).

distribute_or(and(A, B), Q, and(R1, R2)) :-
    !,
    cnf(or(A, Q), R1),
    cnf(or(B, Q), R2).
distribute_or(Q, and(A, B), and(R1, R2)) :-
    !,
    cnf(or(A, Q), R1),
    cnf(or(B, Q), R2).
distribute_or(A, B, or(A, B)).

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
    expr_r(T0, A0),
    implies(A0, A).

implies(T0, A) -->
    [operator(implies)], !,
    term(T1),
    expr_r(T1, E),
    implies(implies(T0, E), A).
implies(T, T) -->
    [].

expr_r(T0, A) -->
    [operator(and)], !,
    term(T1),
    expr_r(and(T0, T1), A).
expr_r(T0, A) -->
    [operator(or)], !,
    term(T1),
    expr_r(or(T0, T1), A).
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

?- string_codes("p&q->c", _Expr), time(phrase(lex(_Tokens), _Expr)), time(phrase(parse(Ast), _Tokens)),
sat(Ast, Vars).

?- string_codes("p&q|v&k", _Expr), time(phrase(lex(_Tokens), _Expr)), time(phrase(parse(Ast), _Tokens)),
nif(Ast, NIF),
nnf(NIF, NNF),
cnf(NNF, CNF),
bake(CNF, B),
vars(B, SYM).

?- string_codes("p&q|v&k", _Expr), time(phrase(lex(_Tokens), _Expr)), time(phrase(parse(Ast), _Tokens)),
nif(Ast, NIF),
nnf(NIF, NNF),
cnf(NNF, CNF),
bake(CNF, B),
vars(B, [], _SYM),
dpll(B, [], SAT, _SYM).

*/

% 2025 -- Rodrigo Monteiro Junior
% sat_DIMACS.pl

:- use_module(library(dcg/basics)).

solve :-
    read_line_to_codes(user_input, Expr),
    phrase(lex([header(Vars, _)|Tokens]), Expr),
    phrase(parse(CNF), Tokens),
    
    numlist(1, Vars, SYM),
    findall(SAT, sat(CNF, SAT, SYM), Solutions),
    format('solutions: ~w~n', [Solutions]).

solve(Str) :-
    string_codes(Str, Expr),
    phrase(lex([header(Vars, _)|Tokens]), Expr),
    phrase(parse(CNF), Tokens),
    
    numlist(1, Vars, SYM),
    findall(SAT, sat(CNF, SAT, SYM), Solutions),
    format('solutions: ~w~n', [Solutions]).

% -------------------------

sat(CNF, SAT, SYM) :-
    dpll(CNF, [], SAT, SYM).

% SAT
% -SAT is the set of attributions to the vars
dpll([], SAT, SAT, _) :- !.

% UNSAT
dpll(Phi, _, _, _) :-
    member([], Phi), !,
    fail.

dpll(Phi, Test, SAT, Symbols) :-
	member(Sym, Symbols),
    unit(Phi, Sym, T), !,
    assign(Phi, Sym, T, Assigned),
    select(Sym, Symbols, Chopped),
    dpll(Assigned, [T|Test], SAT, Chopped).

dpll(Phi, Test, SAT, Symbols) :-
    member(Sym, Symbols),
    pure_literal(Phi, Sym, T), !,
    assign(Phi, Sym, T, Assigned),
    select(Sym, Symbols, Chopped),
    dpll(Assigned, [T|Test], SAT, Chopped).

dpll(Phi, Test, SAT, [Sym|Symbols]) :-
    !,
    assign(Phi, Sym, T, Assigned),
    dpll(Assigned, [T|Test], SAT, Symbols).

pure_literal(Phi, Sym, t(Sym)) :-
    \+ (	member(C, Phi)
       ,	member(not(Sym), C)
       ).
pure_literal(Phi, Sym, f(Sym)) :-
    \+ (	member(C, Phi)
       ,	member(Sym, C)
       ).

unit(Phi, Sym, t(Sym)) :- member([Sym], Phi).
unit(Phi, Sym, f(Sym)) :- member([not(Sym)], Phi).

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
    assign_true(Clauses, Sym, [NoSym|NewClauses], Assigned).

assign_true([], _, NewClauses, NewClauses).
% -----------------------------------------------------------

% parser ----------------------------------------------------

parse([Clause|Clauses]) -->
    disjunction([], ClauseRev), !,
    { reverse(ClauseRev, Clause)
    },
    parse(Clauses).

parse([]) -->
    [].

disjunction(C, C) -->
    [end_clause], !.

disjunction(Acc, Clause) -->
    [var(V)],
    disjunction([V|Acc], Clause).

% lexer -----------------------------------------------------

lex([Token|Tokens]) -->
    ignored,
    tok(Token), !,
    lex(Tokens).

lex([]) -->
    [].

ignored -->
    blank, !,
    ignored.
ignored -->
    comment, !,
    ignored.
ignored -->
    [].

comment -->
    "c",
    up_to_nl.

tok(end_clause) --> "0".

tok(var(not(X))) -->
    "-", integer(X).

tok(var(X)) -->
    integer(X).

tok(header(Vars, Clauses)) -->
    "p",           " ",
    "cnf",         " ",
    integer(Vars), " ",
    integer(Clauses).

up_to_nl -->
    eol, !.
up_to_nl -->
    [_], up_to_nl.

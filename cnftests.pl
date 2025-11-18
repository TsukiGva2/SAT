#!/usr/bin/env swipl

:- use_module(sat_dimacs).

:- initialization(test_program, main).

test_program :-
	working_directory(_, 'test'),
	directory_files('.', Fdot),
	select('.', Fdot, Fdd),
	select('..', Fdd, Files),

	format("Run ALL tests [y/N]? (Warning: WILL lag)~n"),
	read_line_to_codes(user_input, Line),

	(	member(0'y, Line)
	->	time(forall(member(File, Files), solve_file(File)))
	;	run_step(Files)
	).

run_step(Files) :-
	(	forall(member(File, Files), time(solve_file_ask(File)))
	->	format("All tests done.~n")
	;	format("break.~n")
	).

solve_file_ask(File) :-
	format("Will try solving: ~w, confirm? [Y/n]~n", [File]),
	read_line_to_codes(user_input, Line),
	(	member(0'n, Line)
	->	fail, !
	;	solve_file(File)
	).


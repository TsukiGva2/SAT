# SAT
A bare bones solution to the boolean satisfiability problem in prolog

Note: this is not a #SAT, altough sometimes it generates more than one solution (due to findall/3).
This is intended for deugging purposes, i'll probably remove this soon (already did for sat_dimacs.pl, which runs the benchmarks)

[Try it online!](https://swish.swi-prolog.org/p/sat_solver.pl)
[Notebook version](https://swish.swi-prolog.org/p/sat_solver%20notebook.swinb)

## Expression syntax

Right now only a small subset of logical operators are defined for this proof of concept:

    Implemented:
    | (OR),
    & (AND),
    -> (Implies),
    ~ (Negation)

    Partially implemented:
    1 (true),
    0 (false) Which are still unhandled in dpll and dont work

    Not implemented, ordered by priority:
    <-> (Biimplies),
    (+) (XOR)
    
    other meta stuff.

The program also supports CNF formulas in DIMACS format (although in a separate file, called sat_dimacs.pl):

    c
    c Solve me!
    c
    p cnf 3  2
    1 2 3 0
    -3 2 0

Though the support is still really janky, it's specifically made for handling the SATLIB tests.

The format is described in further detail on the following pages:

https://jix.github.io/varisat/manual/0.2.0/formats/dimacs.html
https://web.archive.org/web/20190325181937/https://www.satcompetition.org/2009/format-benchmarks2009.html    

## Benchmarks:

Benchmarks can be found under the "benchmarks" folder, containing the test name, description and swi-prolog provided metrics.

The solver is currently only tested under [uf20-91 - SATLIB](https://www.cs.ubc.ca/~hoos/SATLIB/benchm.html), it may take ages to complete in any other problemset.

more tests can be found [here](https://www.cs.ubc.ca/~hoos/SATLIB/benchm.html)

## Running locally / Testing

If you have swi-prolog installed, you can clone the repository and run the tests yourself (or toy with the boolean solver)

    $ git clone https://github.com/TsukiGva2/SAT
    $ cd SAT
    $ ./run.pl      # The boolean expression solver
    $ ./cnftests.pl # Run the tests (you will be prompted before running all tests)

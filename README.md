# SAT
A bare bones solution to the boolean satisfiability problem in prolog

[Try it online!](https://swish.swi-prolog.org/p/sat_solver.pl)
[Notebook version](https://swish.swi-prolog.org/p/sat_solver%20notebook.swinb)

<img width="575" height="111" alt="image" src="https://github.com/user-attachments/assets/ea9aa2ab-8dd5-483f-ba2f-398f7953e49c" />

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

## Simple benchmarks:

![Screenshot_2025-11-17-21-41-51-195_com android chrome](https://github.com/user-attachments/assets/847bfe21-65b7-4a70-a94d-fd5a294f190a)

## Command line

### Requirements: swi—prolog

    $ git clone https://github.com/TsukiGva2/SAT
    $ cd SAT
    $ ./run.pl

<img width="324" height="60" alt="image" src="https://github.com/user-attachments/assets/397fd0fd-b1bb-4658-9b79-bd82f8080856" />

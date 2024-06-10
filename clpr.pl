:- use_module(library(clpr)).

% https://www.swi-prolog.org/pldoc/man?section=clpqr
:- begin_tests(clpr_example).

test(unification_examples) :-
  { X =:= 1.0 },
  { Y = 2.2 },
  Z = 3.33,
  assertion(X =:= 1.0),
  assertion(Y =:= 2.2),
  assertion(Z =:= 3.33).

test(min_max_abs_examples) :-
  X = min(1.23, 4.56),
  Y = max(7.89, 10),
  Z = abs(-9.87),
  assertion(X =:= 1.23),
  assertion(Y =:= 10),
  assertion(Z =:= 9.87).

test(exponent_examples) :-
  X = 2 ^ 3,
  Y = 4 ^ 5,
  Z = 6 ^ 7,
  assertion(X =:= 8),
  assertion(Y =:= 1024),
  assertion(Z =:= 279936).

:- end_tests(clpr_example).
:- run_tests.
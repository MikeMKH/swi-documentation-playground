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

:- end_tests(clpr_example).
:- run_tests.
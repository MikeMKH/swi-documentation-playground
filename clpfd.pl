:- use_module(library(clpfd)).

% https://www.swi-prolog.org/pldoc/man?section=clpfd
:- begin_tests(clpfd_example).

test(simple_arithmetic_examples) :-
  X #= 1+2,
  assertion(X =:= 3),
  3 #= Y+2,
  assertion(Y =:= 1).
  
:- end_tests(clpfd_example).
:- run_tests.
:- use_module(library(clpfd)).

% https://www.swi-prolog.org/pldoc/man?section=clpfd
:- begin_tests(clpfd_example).

test(simple_arithmetic_examples) :-
  X #= 1+2,
  assertion(X =:= 3),
  3 #= Y+2,
  assertion(Y =:= 1).

n_factorial(0, 1).
n_factorial(N, F) :-
  N #> 0, F #> 0,
  N1 #= N - 1,
  F #= N * F1,
  n_factorial(N1, F1).

test(n_factorial_0_is_1, all(R=[1])) :-
  n_factorial(0, R),
  assertion(R =:= 1).

test(n_factorial_1_is_1, all(R=[1])) :-
  n_factorial(1, R),
  assertion(R =:= 1).

test(n_factorial_47_is_large, all(R=[258623241511168180642964355153611979969197632389120000000000])) :-
  n_factorial(47, R),
  assertion(R =:= 258623241511168180642964355153611979969197632389120000000000).

test(n_factorial_3_is_6, all(N=[3])) :-
  n_factorial(N, 6),
  assertion(N =:= 3).

test(n_factorial_resulting_in_3_fail, [fail]) :-
  n_factorial(_, 3).

% ?- plunit_clpfd_example:n_factorial(N,1).
% N = 0 ;
% N = 1 ;
% false.

% ?- plunit_clpfd_example:n_factorial(N,F).
% N = 0,
% F = 1 ;
% N = F, F = 1 ;
% N = F, F = 2 ;
% N = 3,
% F = 6 ;
% N = 4,
% F = 24 ;
% N = 5,
% F = 120 ;
% N = 6,
% F = 720 ;
% N = 7,
% F = 5040 .

:- end_tests(clpfd_example).
:- run_tests.
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

% completly stolen from https://www.swi-prolog.org/pldoc/man?section=clpfd A.9.7
sudoku(Rows) :-
  length(Rows, 9), maplist(same_length(Rows), Rows),
  append(Rows, Vs), Vs ins 1..9,
  maplist(all_distinct, Rows),
  transpose(Rows, Columns),
  maplist(all_distinct, Columns),
  Rows = [As,Bs,Cs,Ds,Es,Fs,Gs,Hs,Is],
  blocks(As, Bs, Cs),
  blocks(Ds, Es, Fs),
  blocks(Gs, Hs, Is).

blocks([], [], []).
blocks([N1,N2,N3|Ns1], [N4,N5,N6|Ns2], [N7,N8,N9|Ns3]) :-
  all_distinct([N1,N2,N3,N4,N5,N6,N7,N8,N9]),
  blocks(Ns1, Ns2, Ns3).

problem(1, [[_,_,_,_,_,_,_,_,_],
      [_,_,_,_,_,3,_,8,5],
      [_,_,1,_,2,_,_,_,_],
      [_,_,_,5,_,7,_,_,_],
      [_,_,4,_,_,_,1,_,_],
      [_,9,_,_,_,_,_,_,_],
      [5,_,_,_,_,_,_,7,3],
      [_,_,2,_,1,_,_,_,_],
      [_,_,_,_,4,_,_,_,9]]).

% ?- problem(1, Rows), sudoku(Rows), maplist(portray_clause, Rows).
% [9, 8, 7, 6, 5, 4, 3, 2, 1].
% [2, 4, 6, 1, 7, 3, 9, 8, 5].
% [3, 5, 1, 9, 2, 8, 7, 4, 6].
% [1, 2, 8, 5, 3, 7, 6, 9, 4].
% [6, 3, 4, 8, 9, 2, 1, 5, 7].
% [7, 9, 5, 4, 6, 1, 8, 3, 2].
% [5, 1, 9, 2, 8, 6, 4, 7, 3].
% [4, 7, 2, 3, 1, 9, 5, 6, 8].
% [8, 6, 3, 7, 4, 5, 2, 1, 9].
% Rows = [[9, 8, 7, 6, 5, 4, 3, 2|...], [2, 4, 6, 1, 7, 3, 9|...], [3, 5, 1, 9, 2, 8|...], [1, 2, 8, 5, 3|...], [6, 3, 4, 8|...], [7, 9, 5|...], [5, 1|...], [4|...], [...|...]].

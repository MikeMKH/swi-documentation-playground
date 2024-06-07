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

% ?- X #> 3.
% X in 4..sup.

% ?- X #\= 20.
% X in inf..19\/21..sup.

test(x_times_2_eq_10) :-
  2*X #= 10,
  assertion(X =:= 5).

% ?- X*X #= 144.
% X in -12\/12.

test(system_of_equations_can_be_solved) :-
  4*X + 2*Y #= 24,
  X + Y #= 9,
  [X,Y] ins 0..sup,
  assertion(X =:= 3),
  assertion(Y =:= 6).

% not sure what #<==>/2 means https://www.swi-prolog.org/pldoc/doc_for?object=%23%3C%3D%3D%3E%20/%202
% ?- X #= Y #<==> B, X in 0..3, Y in 4..5.
% B = 0,
% X in 0..3,
% Y in 4..5.

% ?- X #= Y + Z, X in 0..5, copy_term([X,Y,Z], [X,Y,Z], Gs).
% Gs = [clpfd:(Y+Z#=X), clpfd:(X in 0..5)],
% Y+Z#=X,
% X in 0..5.

puzzle([S,E,N,D] + [M,O,R,E] = [M,O,N,E,Y]) :-
  Vars = [S,E,N,D,M,O,R,Y],
  Vars ins 0..9,
  all_different(Vars),
            S*1000 + E*100 + N*10 + D +
            M*1000 + O*100 + R*10 + E #=
  M*10000 + O*1000 + N*100 + E*10 + Y,
  M #\= 0, S #\= 0.

test(puzzle, all(As=[[9, 5, 6, 7]])) :-
  puzzle(As+Bs=Cs), label(As),
  assertion(As ==    [9, 5, 6, 7]),
  assertion(Bs ==    [1, 0, 8, 5]),
  assertion(Cs == [1, 0, 6, 5, 2]).

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

% completly stolen from https://www.swi-prolog.org/pldoc/man?section=clpfd A.9.10
n_queens(N, Qs) :-
  length(Qs, N),
  Qs ins 1..N,
  safe_queens(Qs).

safe_queens([]).
safe_queens([Q|Qs]) :- safe_queens(Qs, Q, 1), safe_queens(Qs).

safe_queens([], _, _).
safe_queens([Q|Qs], Q0, D0) :-
  Q0 #\= Q,
  abs(Q0 - Q) #\= D0,
  D1 #= D0 + 1,
  safe_queens(Qs, Q0, D1).

% ?- n_queens(8, Qs), label(Qs).
% Qs = [1, 5, 8, 6, 3, 7, 2, 4] ;
% Qs = [1, 6, 8, 3, 7, 4, 2, 5] ;
% Qs = [1, 7, 4, 6, 8, 2, 5, 3] ;
% Qs = [1, 7, 5, 8, 2, 4, 6, 3] ;
% Qs = [2, 4, 6, 8, 3, 1, 7, 5] ;
% Qs = [2, 5, 7, 1, 3, 8, 6, 4] ;
% Qs = [2, 5, 7, 4, 1, 8, 6, 3] ;
% Qs = [2, 6, 1, 7, 4, 8, 3, 5] ;
% Qs = [2, 6, 8, 3, 1, 4, 7, 5] ;
% Qs = [2, 7, 3, 6, 8, 5, 1, 4] ;
% Qs = [2, 7, 5, 8, 1, 4, 6, 3] ;
% Qs = [2, 8, 6, 1, 3, 5, 7, 4] ;
% Qs = [3, 1, 7, 5, 8, 2, 4, 6] .
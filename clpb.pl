:- use_module(library(clpb)).

% https://www.swi-prolog.org/pldoc/man?section=clpb
:- begin_tests(clpb_example).

% A.8.4 Examples

% Expr * Expr	logical AND
test(sat_x_and_y_x_is_1_y_is_1) :-
  sat(X*Y),
  assertion(X == 1),
  assertion(Y == 1).

% ~ Expr	logical NOT
test(sat_x_and_not_x_fail, [fail]) :-
  sat(X * ~X).

% ?- taut(X * ~X, T).
% T = 0,
% sat(X=:=X).
test(taut_x_and_not_x_fail) :-
  taut(X * ~X, T),
  assertion(T == 0).

% Expr + Expr	logical OR
% Var ^ Expr	existential quantification
test(sat_x_or_y_with_x_as_1) :-
  sat(X^Y^(X+Y)),
  X=1.

test(sat_x_or_y_with_y_as_1) :-
  sat(X^Y^(X+Y)),
  Y=1.

test(sat_x_or_y_with_x_as_0_y_as_1) :-
  sat(X^Y^(X+Y)),
  X=0,Y=1.

test(sat_x_or_y_with_x_as_1_y_as_0) :-
  sat(X^Y^(X+Y)),
  X=1,Y=0.

test(sat_x_or_y_with_x_as_1_y_as_1) :-
  sat(X^Y^(X+Y)),
  X=1,Y=1.

% ?- sat(X^Y^(X+Y)),X=0,Y=0.
% X = Y, Y = 0.
test(sat_x_or_y_with_x_as_0_y_as_0) :-
  sat(X^Y^(X+Y)),
  X=0,Y=0,
% not really sure why this is
  assertion(X == Y),
  assertion(Y == 0).

% ?- sat(X*Y + X*Z),labeling([X,Y,Z]).
% X = Z, Z = 1,
% Y = 0 ;
% X = Y, Y = 1,
% Z = 0 ;
% X = Y, Y = Z, Z = 1.
test(sat_x_and_y_or_y_and_z_labeled_x_equal_z) :-
  sat(X*Y + X*Z),once(labeling([X,Y,Z])),
  X=Z,
  assertion(Z == 1),
  assertion(Y == 0).

test(sat_x_and_y_or_y_and_z_labeled_x_equal_y, all(Y = [1])) :-
  sat(X*Y + X*Z),labeling([X,Y,Z]),
  X=Y,Y=\=Z,
  assertion(Y == 1),
  assertion(Z == 0).

test(sat_x_and_y_or_y_and_z_labeled_x_equal_y, all(Z = [1])) :-
  sat(X*Y + X*Z),labeling([X,Y,Z]),
  X=Y,Y=Z,
  assertion(Z == 1).

% Expr =< Expr	less or equal (implication)
% ?- sat(X =< Y),sat(Y =< Z),taut(X =< Z, T).
% T = 1,
% sat(X=:=X*Y),
% sat(Y=:=Y*Z).
test(sat_x_lessthan_y_sat_y_lessthan_z) :-
  sat(X =< Y),sat(Y =< Z),taut(X =< Z, T),
  assertion(T == 1).

% Expr # Expr	exclusive OR
% ?- sat(1#X#a#b).
% sat(X=:=a#b),
% sat(_=:=a).
test(sat_1_xor_x_xor_a_b) :-
% not sure what to do with this example or how it is used
  sat(1#X#a#b),sat(X=:=a#b).

:- end_tests(clpb_example).
:- run_tests.
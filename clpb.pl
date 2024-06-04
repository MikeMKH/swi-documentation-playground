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

% ?- set_prolog_flag(clpb_residuals, bdd).
% true.

% ?- sat(X#Y).
% clpb:'$clpb_bdd'([node(3)-(v(X, 0)->node(2);node(1)), node(1)-(v(Y, 1)->true;false), node(2)-(v(Y, 1)->false;true)]).

% ?- sat(~(X+Y)).
% X = Y, Y = 0.

% ?- sat(+[1,Y,X]), sat(X#Y).
% clpb:'$clpb_bdd'([node(3)-(v(Y, 0)->node(2);node(1)), node(1)-(v(X, 1)->true;false), node(2)-(v(X, 1)->false;true)]).

test(sat_x_equal_1_andthen_x_equal_1_plus_0_fail, [fail]) :-
  sat(X=:=1),X=1+0.

test(x_equal_1_plus_0_sat_x_equal_1_andthen_x_equal_1_plus_0) :-
  X=1+0,sat(X=:=1),X=1+0,
  assertion(X=:=1+0).

test(sat_x_equal_1_andthen_x_is_1_plus_0) :-
  sat(X=:=1),X is 1+0,
  assertion(X=:=1).

% ?- set_prolog_flag(clpb_monotonic, true).
% true.

% all variables now require to be wrapped with the functor v/1
% ?- sat(v(X)=:=1#1).
% X = 0.

:- use_module(library(clpfd)).

pigeon(I, J, Rows, Cs) :-
  length(Rows, I), length(Row, J),
  maplist(same_length(Row), Rows),
  transpose(Rows, TRows),
  phrase((all_cards(Rows,[1]),all_cards(TRows,[0,1])), Cs).

all_cards([], _) --> [].
all_cards([Ls|Lss], Cs) --> [card(Cs,Ls)], all_cards(Lss, Cs).

% ?- pigeon(9, 8, Rows, Cs), sat(*(Cs)).
% Correct to: "plunit_clpb_example:pigeon(9,8,Rows,Cs)"? yes
% false.

% ?- pigeon(2, 3, Rows, Cs), sat(*(Cs)),
% |    append(Rows, Vs), labeling(Vs),
% |    maplist(portray_clause, Rows).
% Correct to: "plunit_clpb_example:pigeon(2,3,Rows,Cs)"? yes
% [0, 0, 1].
% [0, 1, 0].
% Rows = [[0, 0, 1], [0, 1, 0]],
% Cs = [card([1], [0, 0, 1]), card([1], [0, 1, 0]), card([0, 1], [0, 0]), card([0, 1], [0, 1]), card([0, 1], [1, 0])],
% Vs = [0, 0, 1, 0, 1, 0] ;
% [0, 0, 1].
% [1, 0, 0].
% Rows = [[0, 0, 1], [1, 0, 0]],
% Cs = [card([1], [0, 0, 1]), card([1], [1, 0, 0]), card([0, 1], [0, 1]), card([0, 1], [0, 0]), card([0, 1], [1, 0])],
% Vs = [0, 0, 1, 1, 0, 0] ;
% [0, 1, 0].
% [0, 0, 1].
% Rows = [[0, 1, 0], [0, 0, 1]],
% Cs = [card([1], [0, 1, 0]), card([1], [0, 0, 1]), card([0, 1], [0, 0]), card([0, 1], [1, 0]), card([0, 1], [0, 1])],
% Vs = [0, 1, 0, 0, 0, 1] ;
% [0, 1, 0].
% [1, 0, 0].
% Rows = [[0, 1, 0], [1, 0, 0]],
% Cs = [card([1], [0, 1, 0]), card([1], [1, 0, 0]), card([0, 1], [0, 1]), card([0, 1], [1, 0]), card([0, 1], [0, 0])],
% Vs = [0, 1, 0, 1, 0, 0] ;
% [1, 0, 0].
% [0, 0, 1].
% Rows = [[1, 0, 0], [0, 0, 1]],
% Cs = [card([1], [1, 0, 0]), card([1], [0, 0, 1]), card([0, 1], [1, 0]), card([0, 1], [0, 0]), card([0, 1], [0, 1])],
% Vs = [1, 0, 0, 0, 0, 1] ;
% [1, 0, 0].
% [0, 1, 0].
% Rows = [[1, 0, 0], [0, 1, 0]],
% Cs = [card([1], [1, 0, 0]), card([1], [0, 1, 0]), card([0, 1], [1, 0]), card([0, 1], [0, 1]), card([0, 1], [0, 0])],
% Vs = [1, 0, 0, 0, 1, 0].

nand_gate(X, Y, Z) :- sat(Z =:= ~(X*Y)).

xor(X, Y, Z) :-
  nand_gate(X, Y, T1),
  nand_gate(X, T1, T2),
  nand_gate(Y, T1, T3),
  nand_gate(T2, T3, Z).

test(xor_1_0) :-
  xor(X, Y, 0#1),
  assertion(X = 1),
  assertion(Y = 0).

test(xor_0_1) :-
  xor(X, Y, 1#0),
  assertion(X = 1),
  assertion(Y = 0).

test(xor_1_1) :-
  xor(X, Y, 1#1),
  assertion(X = 1),
  assertion(Y = 1).

test(xor_0_0) :-
  xor(X, Y, 0#0),
  assertion(X = 0),
  assertion(Y = 0).

:- end_tests(clpb_example).
:- run_tests.
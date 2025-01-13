:- module(day7, []).
:- use_module(library(dcg/basics), [integer//1, eol//0]).
:- use_module(library(dcg/high_order), [sequence//2, sequence//3]).

equation(eq(R, Coefs)) --> integer(R), ": ", sequence(integer, " ", Coefs), eol.
equations(Eqs) --> sequence(equation, Eqs).

eval(0, X, Y, R) :- R #= X + Y.
eval(1, X, Y, R) :- R #= X * Y.
eval(2, X, Y, R) :-
  number_string(X, S1),
  number_string(Y, S2),
  string_concat(S2, S1, S),
  number_string(R, S).

valid_equation(TestVal, Limit, Cfs) :-
  length(Cfs, Len),
  OpLen #= Len - 1,
  length(BitL, OpLen),
  BitL ins 0..Limit,
  foldl(eval, [0|BitL], Cfs, 0, TestVal).
valid_equations(Eqs, Limit, TestVal) :-
  member(eq(TestVal, Cfs), Eqs),
  once(valid_equation(TestVal, Limit, Cfs)).

part1(Input, Result) :-
  string_codes(Input, Cs),
  once(phrase(equations(Eqs), Cs)),
  aggregate(sum(R), valid_equations(Eqs, 1, R), Result).

part2(Input, Result) :-
  string_codes(Input, Cs),
  once(phrase(equations(Eqs), Cs)),
  aggregate(sum(R), valid_equations(Eqs, 2, R), Result).

:- begin_tests(day7).
:- use_module(test_utils).
small_input("\c
190: 10 19\n\c
3267: 81 40 27\n\c
83: 17 5\n\c
156: 15 6\n\c
7290: 6 8 6 15\n\c
161011: 16 10 13\n\c
192: 17 8 14\n\c
21037: 9 7 18 13\n\c
292: 11 6 16 20\n\c
").
test(part11) :- small_input(In), test_part(value, day7:part1, In, 3749).
test(part12) :- test_part(file, day7:part1, 'resources/day7.txt', 12940396350192).
test(part21) :- small_input(In), test_part(value, day7:part2, In, 11387).
test(part22) :- test_part(file, day7:part2, 'resources/day7.txt', 106016735664498).
:- end_tests(day7).

:- module(day7, []).
:- use_module(library(dcg/basics), [integer//1, eol//0]).
:- use_module(library(dcg/high_order), [sequence//2, sequence//3]).

equation(eq(R, Coefs)) --> integer(R), ": ", sequence(integer, " ", Coefs), eol.
equations(Eqs) --> sequence(equation, Eqs).

valid_equation(_, Target, [Target]).
valid_equation(Mode, Target0, [C|Cfs]) :-
  ( divmod(Target0, C, Target, 0),
    valid_equation(Mode, Target, Cfs)
  ; Target0 > C,
    Target #= Target0 - C,
    valid_equation(Mode, Target, Cfs)
  ; Mode = concat,
    Divisor is 10 ** (1 + floor(log10(C))),
    divmod(Target0, Divisor, Target, C),
    valid_equation(concat, Target, Cfs)
  ).
valid_equations(Eqs, Mode, TestVal) :-
  member(eq(TestVal, Cfs), Eqs),
  reverse(Cfs, RCfs),
  once(valid_equation(Mode, TestVal, RCfs)).

input_result(Input, Mode, Result) :-
  string_codes(Input, Cs),
  once(phrase(equations(Eqs), Cs)),
  aggregate(sum(R), valid_equations(Eqs, Mode, R), Result).

part1(Input, Result) :- input_result(Input, default, Result).
part2(Input, Result) :- input_result(Input, concat, Result).

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

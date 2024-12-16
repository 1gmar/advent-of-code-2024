:- module(day3, []).
:- use_module(library(dcg/basics), [integer//1]).
:- use_module(library(clpfd), [(#=)/2, op(_, _, #=)]).

mul(X, Y) --> "mul(", integer(X), ",", integer(Y), ")".
mul(S) --> mul(X, Y), { S #= S0 + X * Y }, mul(S0).
mul(S) --> [_], mul(S).
mul(0) --> [].

conditional_mul(S, _) --> "do()", conditional_mul(S, on).
conditional_mul(S, _) --> "don't()", conditional_mul(S, off).
conditional_mul(S, on) --> mul(X, Y), { S #= S0 + X * Y }, conditional_mul(S0, on).
conditional_mul(S, F) --> [_], conditional_mul(S, F).
conditional_mul(0, _) --> [].

part1(Input, Result) :-
  string_codes(Input, Codes),
  once(phrase(mul(Result), Codes, _)).
  
part2(Input, Result) :-
  string_codes(Input, Codes),
  once(phrase(conditional_mul(Result, on), Codes, _)).

:- begin_tests(day3).
:- use_module(test_utils).
smallInput("xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))").
test(part1) :- smallInput(In), test_part(value, day3:part1, In, 161).
test(part1) :- test_part(file, day3:part1, 'resources/day3.txt', 183380722).
smallInput2("xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))").
test(part2) :- smallInput2(In), test_part(value, day3:part2, In, 48).
test(part2) :- test_part(file, day3:part2, 'resources/day3.txt', 82733683).
:- end_tests(day3).

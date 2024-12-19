:- module(day1, []).

line_numbers(Line, NumPair) :-
  split_string(Line, " ", " ", SS),
  maplist(number_string, NumPair, SS).
input_data(String, Data) :-
  string_lines(String, Lines),
  maplist(line_numbers, Lines, NumLines),
  transpose(NumLines, Data).

item_score(List, Item, Score) :-
  occurrences_of_term(Item, List, C),
  Score is Item * C.

part1(Input, Result) :-
  input_data(Input, Data),
  maplist(msort, Data, SD),
  [L1, L2] = SD,
  maplist([X, Y, abs(X - Y)]>>true, L1, L2, Diffs),
  sum_list(Diffs, Result).

part2(Input, Result) :-
  input_data(Input, Data),
  [L1, L2] = Data,
  maplist(item_score(L2), L1, Scores),
  sum_list(Scores, Result).

:- begin_tests(day1).
:- use_module(test_utils).
smallInput("\c
3   4\n\c
4   3\n\c
2   5\n\c
1   3\n\c
3   9\n\c
3   3\n").

test(part1) :- smallInput(X), test_part(value, day1:part1, X, 11).
test(part1) :- test_part(file, day1:part1, 'resources/day1.txt', 1660292).
test(part2) :- smallInput(X), test_part(value, day1:part2, X, 31).
test(part2) :- test_part(file, day1:part2, 'resources/day1.txt', 22776016).
:- end_tests(day1).

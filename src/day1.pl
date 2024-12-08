:- module(day1, [part1/2, part2/2]).
:- use_module(library(clpfd), [transpose/2]).

line_numbers(Line, NumPair) :-
  split_string(Line, " ", " ", SS),
  maplist([Str, Num]>>number_string(Num, Str), SS, NumPair).
input_data(String, Data) :-
  string_lines(String, Lines),
  maplist(line_numbers, Lines, NumLines),
  transpose(NumLines, Data).

item_score(Item, List, Score) :-
  include(==(Item), List, FL),
  length(FL, C),
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
  maplist([X, S]>>item_score(X, L2, S), L1, Scores),
  sum_list(Scores, Result).

:- begin_tests(day1).
:- use_module(library(readutil), [read_file_to_string/3]).
smallInput("\c
3   4\n\c
4   3\n\c
2   5\n\c
1   3\n\c
3   9\n\c
3   3\n").

test(part1) :- smallInput(X), part1(X, Y), assertion(Y == 11).
test(part1) :- 
  read_file_to_string('resources/day1.txt', X, []),
  part1(X, Y),
  assertion(Y == 1660292).

test(part2) :- smallInput(X), part2(X, Y), assertion(Y == 31).
test(part2) :- 
  read_file_to_string('resources/day1.txt', X, []),
  part2(X, Y),
  assertion(Y == 22776016).

:- end_tests(day1).

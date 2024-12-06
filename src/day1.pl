:- module(day1, [part1/2, part2/2]).
:- use_module(library(clpfd), [transpose/2]).

line_numbers(Line, NumPair) :-
  split_string(Line, " ", " ", SS),
  maplist([Str, Num]>>number_string(Num, Str), SS, NumPair).
input_data(String, Data) :-
  string_lines(String, Lines),
  maplist(line_numbers, Lines, NumLines),
  transpose(NumLines, Data).

part1(Input, Result) :-
  input_data(Input, Data),
  maplist(msort, Data, SD),
  [L1, L2] = SD,
  foldl([X, Y, S0, S]>>(abs(X - Y, AV), S is S0 + AV), L1, L2, 0, Result).

part2(Input, Result) :-
  input_data(Input, Data),
  [L1, L2] = Data,
  foldl([X, S0, Sum]>>(occurrences_of_term(X, L2, C), Sum is S0 + X * C), L1, 0, Result).

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

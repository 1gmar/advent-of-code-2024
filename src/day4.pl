:- module(day4, []).
:- use_module(library(clpfd), [transpose/2, (#=)/2, op(_, _, #=), (#>)/2, op(_, _, #>)]).

count_down_list(_, _, 0, []).
count_down_list(E, Step, N, [E|L]) :-
  N #> 0,
  N0 #= N - 1,
  E0 #= E + Step,
  count_down_list(E0, Step, N0, L).

list_window(List, N, Win) :-
  length(Win, N),
  append([_, Win, _], List).

word_list_match(Word, List) :-
  string_chars(Word, CWord),
  reverse(CWord, RWord),
  ( append([_, CWord, _], List)
  ; append([_, RWord, _], List)
  ).
row_xmas_count(Row, Count) :-
  aggregate_all(count, word_list_match("XMAS", Row), Count).

n_col_win_diagonals(N, ColWin, Diags) :-
  count_down_list(N, -1, N, LIndices),
  maplist(nth1, LIndices, ColWin, Diag1),
  count_down_list(0, 1, N, RIndices),
  maplist(nth0, RIndices, ColWin, Diag2),
  Diags = [Diag1, Diag2].

matrix_xmas_diagonal_match(Mat) :-
  list_window(Mat, 4, RowWin),
  transpose(RowWin, TRW),
  list_window(TRW, 4, ColWin),
  n_col_win_diagonals(4, ColWin, Diags),
  select(D, Diags, _),
  word_list_match("XMAS", D).

matrix_xmas_count(Mat, Count) :-
  maplist(row_xmas_count, Mat, RowCounts),
  transpose(Mat, TMat),
  maplist(row_xmas_count, TMat, ColCounts),
  aggregate_all(count, matrix_xmas_diagonal_match(Mat), DiagCount),
  sum_list(RowCounts, RowCount),
  sum_list(ColCounts, ColCount),
  Count is RowCount + ColCount + DiagCount.

matrix_x_mas_match(Mat) :-
  list_window(Mat, 3, RowWin),
  transpose(RowWin, TRW),
  list_window(TRW, 3, ColWin),
  n_col_win_diagonals(3, ColWin, Diags),
  maplist(word_list_match("MAS"), Diags).

part1(Input, Result) :-
  string_lines(Input, Lines),
  maplist(string_chars, Lines, Cs),
  matrix_xmas_count(Cs, Result).

part2(Input, Result) :-
  string_lines(Input, Lines),
  maplist(string_chars, Lines, Mat),
  aggregate_all(count, matrix_x_mas_match(Mat), Result).

:- begin_tests(day4).
:- use_module(test_utils).
small_input("\c
MMMSXXMASM\n\c
MSAMXMSMSA\n\c
AMXSXMAAMM\n\c
MSAMASMSMX\n\c
XMASAMXAMM\n\c
XXAMMXXAMA\n\c
SMSMSASXSS\n\c
SAXAMASAAA\n\c
MAMMMXMMMM\n\c
MXMXAXMASX\n\c
").
test(part1) :- small_input(In), test_part(value, day4:part1, In, 18).
test(part1) :- test_part(file, day4:part1, 'resources/day4.txt', 2370).
test(part2) :- small_input(In), test_part(value, day4:part2, In, 9).
test(part2) :- test_part(file, day4:part2, 'resources/day4.txt', 1908).
:- end_tests(day4).

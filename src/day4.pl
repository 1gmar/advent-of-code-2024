:- module(day4, []).

list_window(List, N, Win) :-
  length(Win, N),
  append([_, Win, _], List).

word_list_match(Word, List) :-
  string_chars(Word, CWord),
  reverse(CWord, RWord),
  (CWord = List ; RWord = List).

matrix_n_rotation(Mat, 0, Mat).
matrix_n_rotation(Mat, N, Rot) :-
  N #> 0,
  N0 #= N - 1,
  maplist(reverse, Mat, RMat),
  transpose(RMat, TRMat),
  matrix_n_rotation(TRMat, N0, Rot).

n_col_win_diagonals(N, ColWin, Diags) :-
  range_list(N, -1, N, LIndices),
  maplist(nth1, LIndices, ColWin, Diag1),
  range_list(0, 1, N, RIndices),
  maplist(nth0, RIndices, ColWin, Diag2),
  Diags = [Diag1, Diag2].

matrix_xmas_row_col_match(Mat) :-
  string_chars("XMAS", XMAS),
  N in 0..3,
  matrix_n_rotation(Mat, N, Rot),
  select(Row, Rot, _),
  append([_, XMAS, _], Row).

matrix_xmas_diagonal_match(Mat) :-
  list_window(Mat, 4, RowWin),
  transpose(RowWin, TRW),
  list_window(TRW, 4, ColWin),
  n_col_win_diagonals(4, ColWin, Diags),
  select(D, Diags, _),
  word_list_match("XMAS", D).

matrix_xmas_count(Mat, Count) :-
  aggregate_all(count, matrix_xmas_row_col_match(Mat), RowColCount),
  aggregate_all(count, matrix_xmas_diagonal_match(Mat), DiagCount),
  Count is RowColCount + DiagCount.

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

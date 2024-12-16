:- module(day4, []).
:- use_module(library(clpfd), [transpose/2, (#=)/2, op(_, _, #=), (#>)/2, op(_, _, #>)]).

take(N, List, Pref) :-
  length(Pref, N),
  prefix(Pref, List).

count_down_list(_, 0, []).
count_down_list(E, N, [E|L]) :-
  N #> 0,
  N0 #= N - 1,
  E0 #= E - 1,
  count_down_list(E0, N0, L).

list_window(List, N, Win) :-
  length(Win, N),
  append([_, Win, _], List).

rotate(List, N, Result) :-
  reverse(List, RL),
  length(List, Len),
  (N < 0 -> RotN is (Len + N) mod Len ; RotN is N mod Len),
  length(RPref, RotN),
  append(RPref, RSuff, RL),
  reverse(RPref, Pref),
  reverse(RSuff, Suff),
  append(Pref, Suff, Result).

xmas_match(Row) :-
  string_chars("XMAS", XMAS),
  append([_, XMAS, _], Row).
row_xmas_count(Row, Count) :-
  aggregate_all(count, xmas_match(Row), FCount),
  reverse(Row, RRow),
  aggregate_all(count, xmas_match(RRow), BCount),
  Count is FCount + BCount.

matrix_diagonal_xmas_count(Mat, Count) :-
  matrix_right_diagonals_xmas_count(Mat, C1),
  maplist(reverse, Mat, RMat),
  matrix_right_diagonals_xmas_count(RMat, C2),
  Count is C1 + C2.
matrix_right_diagonals_xmas_count(Mat, Count) :-
  matrix_row_diagonals(Mat, RDiags),
  row_diagonals_xmas_count(RDiags, Count1),
  transpose(Mat, TMat),
  matrix_row_diagonals(TMat, LDiags),
  [_|LDiagsT] = LDiags,
  row_diagonals_xmas_count(LDiagsT, Count2),
  Count is Count1 + Count2.
matrix_row_diagonals(Mat, RDiags) :-
  length(Mat, Len),
  count_down_list(0, Len, Rots),
  maplist(rotate, Mat, Rots, CD),
  transpose(CD, RDiags).
row_diagonals_xmas_count(RDiags, Count) :-
  length(RDiags, Len),
  count_down_list(Len, Len, TakeArgs),
  maplist(take, TakeArgs, RDiags, Diags),
  maplist(row_xmas_count, Diags, Counts),
  sum_list(Counts, Count).

matrix_xmas_count(Mat, Count) :-
  maplist(row_xmas_count, Mat, RowCounts),
  transpose(Mat, TMat),
  maplist(row_xmas_count, TMat, ColCounts),
  matrix_diagonal_xmas_count(Mat, DiagCount),
  sum_list(RowCounts, RowCount),
  sum_list(ColCounts, ColCount),
  Count is RowCount + ColCount + DiagCount.

x_mas_match(Diag) :-
  string_chars("MAS", XMAS),
  (XMAS = Diag ; reverse(Diag, XMAS)).
three_col_win_diagonal(right, ColWin, Diag) :- three_col_win_diagonal_(ColWin, [0, 1, 2], 2, Diag).
three_col_win_diagonal(left, ColWin, Diag) :- three_col_win_diagonal_(ColWin, [0, -1, -2], 0, Diag).
three_col_win_diagonal_(ColWin, Rots, Index, Diag) :-
  maplist(rotate, ColWin, Rots, RW),
  transpose(RW, TRW),
  nth0(Index, TRW, Diag).
matrix_x_mas_match(Mat) :-
  list_window(Mat, 3, RowWin),
  transpose(RowWin, TRW),
  list_window(TRW, 3, ColWin),
  three_col_win_diagonal(left, ColWin, Diag1),
  three_col_win_diagonal(right, ColWin, Diag2),
  x_mas_match(Diag1),
  x_mas_match(Diag2).

part1(Input, Result) :-
  string_lines(Input, Lines),
  maplist(string_chars, Lines, Cs),
  once(matrix_xmas_count(Cs, Result)).

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

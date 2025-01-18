:- module(day8, []).

antenna_map(Pts0, c(X0, Y), Pts) --> ".", { X #= X0 + 1 }, antenna_map(Pts0, c(X, Y), Pts).
antenna_map(Pts0, c(_, Y0), Pts) --> 
  [C],
  { code_type(C, end_of_line), Y #= Y0 + 1 },
  antenna_map(Pts0, c(0, Y), Pts).
antenna_map(Pts0, c(X0, Y), Pts) --> 
  [K],
  { code_type(K, alnum),
    (  get_assoc(K, Pts0, Vs)
    -> put_assoc(K, Pts0, [p(X0, Y)|Vs], Pts1)
    ;  put_assoc(K, Pts0, [p(X0, Y)], Pts1)
    ),
    X #= X0 + 1
  },
  antenna_map(Pts1, c(X, Y), Pts).
antenna_map(Pts, _, Pts) --> [].

antenna_line(As, line(P1, P2)) :- perm_n(2, As, [P1, P2]).
perm_n(N, L, P) :- comb(N, L, C), permutation(C, P).

comb(0, _, []) :- !.
comb(_, [], []) :- !.
comb(N, [X|Xs], C) :-
  length(C, N),
  ( N0 #= N - 1,
    comb(N0, Xs, C0),
    C = [X|C0]
  ; comb(N, Xs, C)
  ).

distance_antinode_position(line(p(X0, Y0), p(X1, Y1)), p(X, Y)) :-
  SqD #= (Y1 - Y0) ^ 2 + (X1 - X0) ^ 2,
  (Y1 - Y0) * (X - X1) #= (Y - Y1) * (X1 - X0),
  SqD #= (Y0 - Y) ^ 2 + (X0 - X) ^ 2,
  4 * SqD #= (Y1 - Y) ^ 2 + (X1 - X) ^ 2.

line_antinode_position(line(p(X0, Y0), p(X1, Y1)), p(X, Y)) :-
  (Y1 - Y0) * (X - X1) #= (Y - Y1) * (X1 - X0),
  label([X, Y]).

antennas_antinode(AN_Position, As_by_Freq, Size, p(X, Y)) :-
  member(As, As_by_Freq),
  antenna_line(As, Line),
  [X, Y] ins 0..Size,
  call(AN_Position, Line, p(X, Y)).
  
input_antinode_criteria_result(Input, Criteria, Result) :-
  string_codes(Input, Cs),
  string_lines(Input, Ls),
  length(Ls, Len),
  Size is Len - 1,
  empty_assoc(Pts0),
  once(phrase(antenna_map(Pts0, c(0, 0), Pts), Cs)),
  assoc_to_values(Pts, As_by_Freq),
  setof(AN, antennas_antinode(Criteria, As_by_Freq, Size, AN), ANs),
  length(ANs, Result).

part1(Input, Result) :- input_antinode_criteria_result(Input, distance_antinode_position, Result).
part2(Input, Result) :- input_antinode_criteria_result(Input, line_antinode_position, Result).

:- begin_tests(day8).
:- use_module(test_utils).
small_input("\c
............\n\c
........0...\n\c
.....0......\n\c
.......0....\n\c
....0.......\n\c
......A.....\n\c
............\n\c
............\n\c
........A...\n\c
.........A..\n\c
............\n\c
............\n\c
").
test(part11) :- small_input(In), test_part(value, day8:part1, In, 14).
test(part12) :- test_part(file, day8:part1, 'resources/day8.txt', 376).
test(part21) :- small_input(In), test_part(value, day8:part2, In, 34).
test(part22) :- test_part(file, day8:part2, 'resources/day8.txt', 1352).
:- end_tests(day8).

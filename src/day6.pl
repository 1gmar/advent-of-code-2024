:- module(day6, []).

string_map([p(X0, Y0)|Obs], GR, c(X0, Y0)) --> "#", { X #= X0 + 1 }, string_map(Obs, GR, c(X, Y0)).
string_map(Obs, [p(X0, Y0)|GR], c(X0, Y0)) --> "^", { X #= X0 + 1 }, string_map(Obs, GR, c(X, Y0)).
string_map(Obs, GR, c(X0, Y0)) --> ".", { X #= X0 + 1 }, string_map(Obs, GR, c(X, Y0)).
string_map(Obs, GR, c(_, Y0)) --> "\n", { Y #= Y0 + 1 }, string_map(Obs, GR, c(0, Y)).
string_map([], [], _) --> [].
 
turn_right(n, e).
turn_right(e, s).
turn_right(s, w).
turn_right(w, n).

select_distance_(n, Obs, p(X, Y), ObY, Size, Diff) :-
  (  get_assoc(p(X, ObY), Obs, v)
  -> Diff #= abs(Y - ObY)
  ;  ObY1 #= ObY - 1,
     ObY1 #>= 0,
     select_distance_(n, Obs, p(X, Y), ObY1, Size, Diff)
  ).
select_distance_(s, Obs, p(X, Y), ObY, Size, Diff) :-
  (  get_assoc(p(X, ObY), Obs, v)
  -> Diff #= abs(Y - ObY)
  ;  ObY1 #= ObY + 1,
     ObY1 #< Size,
     select_distance_(s, Obs, p(X, Y), ObY1, Size, Diff)
  ).
select_distance_(w, Obs, p(X, Y), ObX, Size, Diff) :-
  (  get_assoc(p(ObX, Y), Obs, v)
  -> Diff #= abs(X - ObX)
  ;  ObX1 #= ObX - 1,
     ObX1 #>= 0,
     select_distance_(w, Obs, p(X, Y), ObX1, Size, Diff)
  ).
select_distance_(e, Obs, p(X, Y), ObX, Size, Diff) :-
  (  get_assoc(p(ObX, Y), Obs, v)
  -> Diff #= abs(X - ObX)
  ;  ObX1 #= ObX + 1,
     ObX1 #< Size,
     select_distance_(e, Obs, p(X, Y), ObX1, Size, Diff)
  ).

select_distance(n, Obs, p(X, Y), Size, Diff) :- select_distance_(n, Obs, p(X, Y), Y, Size, Diff).
select_distance(s, Obs, p(X, Y), Size, Diff) :- select_distance_(s, Obs, p(X, Y), Y, Size, Diff).
select_distance(w, Obs, p(X, Y), Size, Diff) :- select_distance_(w, Obs, p(X, Y), X, Size, Diff).
select_distance(e, Obs, p(X, Y), Size, Diff) :- select_distance_(e, Obs, p(X, Y), X, Size, Diff).

aggregate_route(n, p(X, Y), Diff, Route, p(X, GY)) :-
  range_list(Y, -1, Diff, L),
  GY #= Y - Diff + 1,
  bagof(p(X, Y1, n), T^select(Y1, L, T), Route).
aggregate_route(s, p(X, Y), Diff, Route, p(X, GY)) :-
  range_list(Y, 1, Diff, L),
  GY #= Y + Diff - 1,
  bagof(p(X, Y1, s), T^select(Y1, L, T), Route).
aggregate_route(w, p(X, Y), Diff, Route, p(GX, Y)) :-
  range_list(X, -1, Diff, L),
  GX #= X - Diff + 1,
  bagof(p(X1, Y, w), T^select(X1, L, T), Route).
aggregate_route(e, p(X, Y), Diff, Route, p(GX, Y)) :-
  range_list(X, 1, Diff, L),
  GX #= X + Diff - 1,
  bagof(p(X1, Y, e), T^select(X1, L, T), Route).

move_to_obstacle(Dir, Obs, GPos, Size, Route, NGPos) :-
  select_distance(Dir, Obs, GPos, Size, Diff),
  aggregate_route(Dir, GPos, Diff, Route, NGPos).

next_guard_pos_(n, p(X, Y), Diff, p(X, GY)) :- GY #= Y - Diff + 1.
next_guard_pos_(s, p(X, Y), Diff, p(X, GY)) :- GY #= Y + Diff - 1.
next_guard_pos_(w, p(X, Y), Diff, p(GX, Y)) :- GX #= X - Diff + 1.
next_guard_pos_(e, p(X, Y), Diff, p(GX, Y)) :- GX #= X + Diff - 1.

next_guard_pos(Dir, Obs, GPos, Size, NGPos) :-
  select_distance(Dir, Obs, GPos, Size, Diff),
  next_guard_pos_(Dir, GPos, Diff, NGPos).

exit_distance(w, p(X, _), _, X).
exit_distance(n, p(_, Y), _, Y).
exit_distance(e, p(X, _), Len, Diff) :- Diff #= Len - X.
exit_distance(s, p(_, Y), Len, Diff) :- Diff #= Len - Y.

simulate_guard_patrol(p(X, Y, Dir), Obs, Len, Route, Result) :-
  (  move_to_obstacle(Dir, Obs, p(X, Y), Len, R1, p(X1, Y1))
  -> append(Route, R1, R2),
     turn_right(Dir, NDir),
     simulate_guard_patrol(p(X1, Y1, NDir), Obs, Len, R2, Result)
  ;  exit_distance(Dir, p(X, Y), Len, Diff),
     aggregate_route(Dir, p(X, Y), Diff, R1, _),
     append(Route, R1, Result)
  ).

find_patrol_loop(Dir, GPos, Obs, Size, GPosTrace) :-
  next_guard_pos(Dir, Obs, GPos, Size, NPos),
  (  get_assoc(t(Dir, NPos), GPosTrace, v)
  -> true
  ;  put_assoc(t(Dir, NPos), GPosTrace, v, Trace1),
     turn_right(Dir, NDir),
     find_patrol_loop(NDir, NPos, Obs, Size, Trace1)
  ).

find_patrol_loops(Obs, [GP|Tail], Size, Count) :-
  empty_assoc(BST),
  once(append([Pref, [_]], [GP|Tail])),
  foldl(fold_patrol_loops, Pref, Tail, r(Size, Obs, BST, 0), r(_, _, _, Count)).

fold_patrol_loops(p(GX, GY, Dir), p(X, Y, _), r(Size, Obs, Trace, Count), r(Size, Obs, Trace1, Count1)) :-
  (  get_assoc(p(X, Y), Trace, v)
  -> Trace1 = Trace, Count1 = Count
  ;  put_assoc(p(X, Y), Obs, v, NObs),
     put_assoc(p(X, Y), Trace, v, Trace1),
     empty_assoc(BST),
     (  find_patrol_loop(Dir, p(GX, GY), NObs, Size, BST)
     -> Count1 is Count + 1
     ;  Count1 = Count
     )
  ).

string_parsed_map(Input, Len, BST, GP) :-
  string_lines(Input, Lines),
  length(Lines, Len),
  string_codes(Input, Codes),
  once(phrase(string_map(Obs, [GP], c(0, 0)), Codes)),
  maplist([P, P-v]>>true, Obs, Ps),
  list_to_assoc(Ps, BST).

part1(Input, Result) :-
  string_parsed_map(Input, Len, Obs, p(X, Y)),
  simulate_guard_patrol(p(X, Y, n), Obs, Len, [], Route),
  maplist([p(R, C, _), p(R, C)]>>true, Route, R1),
  list_to_ord_set(R1, Set),
  length(Set, Result).

part2(Input, Result) :-
  string_parsed_map(Input, Len, Obs, p(X, Y)),
  simulate_guard_patrol(p(X, Y, n), Obs, Len, [], Route),
  find_patrol_loops(Obs, Route, Len, Result).

:- begin_tests(day6).
:- use_module(test_utils).
small_input("\c
....#.....\n\c
.........#\n\c
..........\n\c
..#.......\n\c
.......#..\n\c
..........\n\c
.#..^.....\n\c
........#.\n\c
#.........\n\c
......#...\n\c
").
test(part11) :- small_input(In), test_part(value, day6:part1, In, 41).
test(part12) :- test_part(file, day6:part1, 'resources/day6.txt', 5305).
test(part21) :- small_input(In), test_part(value, day6:part2, In, 6).
test(part22) :- test_part(file, day6:part2, 'resources/day6.txt', 2143).
:- end_tests(day6).

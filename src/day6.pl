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
  aggregate(set(p(X, Y1)), T^select(Y1, L, T), Route).
aggregate_route(s, p(X, Y), Diff, Route, p(X, GY)) :-
  range_list(Y, 1, Diff, L),
  GY #= Y + Diff - 1,
  aggregate(set(p(X, Y1)), T^select(Y1, L, T), Route).
aggregate_route(w, p(X, Y), Diff, Route, p(GX, Y)) :-
  range_list(X, -1, Diff, L),
  GX #= X - Diff + 1,
  aggregate(set(p(X1, Y)), T^select(X1, L, T), Route).
aggregate_route(e, p(X, Y), Diff, Route, p(GX, Y)) :-
  range_list(X, 1, Diff, L),
  GX #= X + Diff - 1,
  aggregate(set(p(X1, Y)), T^select(X1, L, T), Route).

move_to_obstacle(Dir, Obs, GPos, Size, Route, NGPos) :-
  select_distance(Dir, Obs, GPos, Size, Diff),
  once(aggregate_route(Dir, GPos, Diff, Route, NGPos)).

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

simulate_guard_patrol(Dir, GPos, Obs, Len, Route, Result) :-
  (  move_to_obstacle(Dir, Obs, GPos, Len, R1, NGPos)
  -> ord_union(Route, R1, R2),
     turn_right(Dir, NDir),
     simulate_guard_patrol(NDir, NGPos, Obs, Len, R2, Result)
  ;  exit_distance(Dir, GPos, Len, Diff),
     aggregate_route(Dir, GPos, Diff, R1, _),
     ord_union(Route, R1, Result)
  ).

find_patrol_loop(Dir, GPos, Obs, Size, GPosTrace) :-
  next_guard_pos(Dir, Obs, GPos, Size, NPos),
  (  get_assoc(t(Dir, NPos), GPosTrace, v)
  -> true
  ;  put_assoc(t(Dir, NPos), GPosTrace, v, Trace1),
     turn_right(Dir, NDir),
     find_patrol_loop(NDir, NPos, Obs, Size, Trace1)
  ).

find_patrol_loops(GPos, Obs, Route, Size) :-
  empty_assoc(BST),
  ord_del_element(Route, GPos, NPosLs),
  member(NPos, NPosLs),
  put_assoc(NPos, Obs, v, NObs),
  find_patrol_loop(n, GPos, NObs, Size, BST).
    
string_parsed_map(Input, Len, BST, GP) :-
  string_lines(Input, Lines),
  length(Lines, Len),
  string_codes(Input, Codes),
  once(phrase(string_map(Obs, [GP], c(0, 0)), Codes)),
  maplist([P, P-v]>>true, Obs, Ps),
  list_to_assoc(Ps, BST).

part1(Input, Result) :-
  string_parsed_map(Input, Len, Obs, GP),
  once(simulate_guard_patrol(n, GP, Obs, Len, [], Route)),
  length(Route, Result).

part2(Input, Result) :-
  string_parsed_map(Input, Len, Obs, GP),
  once(simulate_guard_patrol(n, GP, Obs, Len, [], Route)),
  aggregate(count, find_patrol_loops(GP, Obs, Route, Len), Result).

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

:- module(day2, [part1/2, part2/2]).
:- use_module(library(clpfd), [ins/2, op(_, _, ins), op(_, _, ..)]).

line_numbers(Line, NumPair) :-
  split_string(Line, " ", " ", SS),
  maplist([Str, Num]>>number_string(Num, Str), SS, NumPair).
input_data(String, Data) :-
  string_lines(String, Lines),
  maplist(line_numbers, Lines, Data).

safe_report(Report) :-
  [_|RepTail] = Report,
  length(RepTail, Len),
  length(RepPref, Len),
  prefix(RepPref, Report),
  maplist([X, Y, Diff]>>(Diff is X - Y), RepPref, RepTail, Diffs),
  (Diffs ins 1..3 ; Diffs ins (-3)..(-1)).

safe_report_dampened(Report) :-
  safe_report(Report), !
  ; select(_, Report, SubRep),
    safe_report(SubRep), !.
  
part1(Input, Result) :-
  input_data(Input, Data),
  include(safe_report, Data, SafeReports),
  length(SafeReports, Result).

part2(Input, Result) :-
  input_data(Input, Data),
  include(safe_report_dampened, Data, SafeReports),
  length(SafeReports, Result).

:- begin_tests(day2).
:- use_module(library(readutil), [read_file_to_string/3]).
smallInput("\c
7 6 4 2 1\n\c
1 2 7 8 9\n\c
9 7 6 2 1\n\c
1 3 2 4 5\n\c
8 6 4 4 1\n\c
1 3 6 7 9\n\c
").

test_part(Part, In, ExpOut) :- call(Part, In, Out), assertion(Out == ExpOut).

test(part1) :- smallInput(In), test_part(part1, In, 2).
test(part1) :-
  read_file_to_string('resources/day2.txt', In, []),
  test_part(part1, In, 371).

test(part2) :- smallInput(In), test_part(part2, In, 4).
test(part2) :-
  read_file_to_string('resources/day2.txt', In, []),
  test_part(part2, In, 426).

:- end_tests(day2).

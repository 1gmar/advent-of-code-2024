:- module(test_utils, [test_part/4]).
:- use_module(library(readutil), [read_file_to_string/3]).

assert_part_result(Part, Input, ExpResult) :-
  call(Part, Input, Result),
  assertion(Result == ExpResult).

test_part(value, Part, Input, ExpResult) :-
  assert_part_result(Part, Input, ExpResult).
test_part(file, Part, File, ExpResult) :-
  read_file_to_string(File, Input, []),
  assert_part_result(Part, Input, ExpResult).

:- use_module(library(clpfd)).

:- set_prolog_flag(double_quotes, chars).
:- set_prolog_flag(back_quotes, string).
:- set_prolog_flag(answer_write_options, [max_depth(0)]).
:- set_prolog_flag(debugger_write_options, [max_depth(0)]).

range_list(_, _, 0, []).
range_list(E, Step, N, [E|L]) :-
  N #> 0,
  N0 #= N - 1,
  E0 #= E + Step,
  range_list(E0, Step, N0, L).


:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- set_prolog_flag(double_quotes, chars).
:- set_prolog_flag(back_quotes, string).
:- set_prolog_flag(answer_write_options, [max_depth(0)]).
:- set_prolog_flag(debugger_write_options, [max_depth(0)]).

range_list(E, Step, N, L) :-
  zcompare(C, N, 0),
  range_list_(C, E, Step, N, L).

range_list_(=, _, _, _, []).
range_list_(>, E0, Step, N0, [E0|L]) :-
  N #= N0 - 1,
  E #= E0 + Step,
  range_list(E, Step, N, L).


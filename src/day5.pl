:- module(day5, []).

order_rules([rule(X, Y)|Rs]) --> integer(X), "|", integer(Y), "\n", order_rules(Rs).
order_rules([]) --> "\n".

updates([update(Ints)|Us]) --> sequence(integer, ",", Ints), "\n", updates(Us).
updates([]) --> [].

update_quick_sorted([N|Nums], Rules) -->
  { partition(check_rule(Rules, N), Nums, Lesser, Greater) },
  update_quick_sorted(Greater, Rules),
  [N],
  update_quick_sorted(Lesser, Rules).
update_quick_sorted([], _) --> [].

check_rule(Rules, N, Next) :-
  member(rule(N, Next), Rules).
num_obeys_rule(Rules, N-Nums) :-
  maplist(check_rule(Rules, N), Nums).
valid_order(Rules, Nums, Result) :-
  bagof(N-T, H^append([H, [N], T], Nums), Pairs),
  maplist(num_obeys_rule(Rules), Pairs),
  Result = Nums.
  
invalid_update_check(Rules, Nums) :-
  append([_, [N], T], Nums),
  select(Next, T, _),
  member(rule(Next, N), Rules).
invalid_order_sorted(Rules, Nums, Result) :-
  once(invalid_update_check(Rules, Nums)),
  phrase(update_quick_sorted(Nums, Rules), Result).

select_midnum_from_updates_with(Criteria, Updates, MidNum) :-
  select(update(Nums), Updates, _),
  call(Criteria, Nums, Result),
  length(Result, Len),
  MidIdx #= Len // 2,
  nth0(MidIdx, Result, MidNum).

string_rules_and_updates(Input, Rules, Updates) :-
  string_codes(Input, Codes),
  once(phrase((order_rules(Rules), updates(Updates)), Codes)).

part1(Input, Result) :-
  string_rules_and_updates(Input, Rules, Updates),
  aggregate(
    sum(MidNum),
    select_midnum_from_updates_with(valid_order(Rules), Updates, MidNum),
    Result).

part2(Input, Result) :-
  string_rules_and_updates(Input, Rules, Updates),
  aggregate(
    sum(MidNum),
    select_midnum_from_updates_with(invalid_order_sorted(Rules), Updates, MidNum),
    Result).

:- begin_tests(day5).
:- use_module(test_utils).
small_input("\c
47|53\n\c
97|13\n\c
97|61\n\c
97|47\n\c
75|29\n\c
61|13\n\c
75|53\n\c
29|13\n\c
97|29\n\c
53|29\n\c
61|53\n\c
97|53\n\c
61|29\n\c
47|13\n\c
75|47\n\c
97|75\n\c
47|61\n\c
75|61\n\c
47|29\n\c
75|13\n\c
53|13\n\c
\n\c
75,47,61,53,29\n\c
97,61,53,29,13\n\c
75,29,13\n\c
75,97,47,61,53\n\c
61,13,29\n\c
97,13,75,29,47\n\c
").
test(part1) :- small_input(In), test_part(value, day5:part1, In, 143).
test(part1) :- test_part(file, day5:part1, 'resources/day5.txt', 6612).
test(part2) :- small_input(In), test_part(value, day5:part2, In, 123).
test(part2) :- test_part(file, day5:part2, 'resources/day5.txt', 4944).
:- end_tests(day5).

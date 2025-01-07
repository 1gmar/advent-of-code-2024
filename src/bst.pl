:- module(bst, [bst_search/2, bst_insert/3]).

height(nil, 0).
height(node(_, _, _, H), H).
height_updated(L, R, H) :-
  height(L, LH),
  height(R, RH),
  H #= 1 + max(LH, RH).

balance(nil, 0).
balance(node(_, L, R, _), B) :- 
  height(L, LH),
  height(R, RH),
  B #= LH - RH.

balance_compare(ll, B, Val, node(_, node(LV, _, _, _), _, _)) :- B #> 1, Val @< LV.
balance_compare(rr, B, Val, node(_, _, node(RV, _, _, _), _)) :- B #< -1, Val @> RV.
balance_compare(lr, B, Val, node(_, node(LV, _, _, _), _, _)) :- B #> 1, Val @> LV. 
balance_compare(rl, B, Val, node(_, _, node(RV, _, _, _), _)) :- B #< -1, Val @< RV.
balance_compare(b, B, _, _) :- B in -1..1.

bst_rotate(right, node(V, node(LV, LL, LR, _), R, _), node(LV, LL, node(V, LR, R, RH), H)) :-
  height_updated(LR, R, RH),
  height_updated(LL, node(V, LR, R, RH), H).
bst_rotate(left, node(V, L, node(RV, RL, RR, _), _), node(RV, node(V, L, RL, LH), RR, H)) :-
  height_updated(L, RL, LH),
  height_updated(node(V, L, RL, LH), RR, H).

bst_balanced_tree(b, T, T).
bst_balanced_tree(ll, T, BT) :- bst_rotate(right, T, BT).
bst_balanced_tree(rr, T, BT) :- bst_rotate(left, T, BT).
bst_balanced_tree(lr, node(V, L, R, H), BT) :-
  bst_rotate(left, L, BL),
  bst_rotate(right, node(V, BL, R, H), BT).
bst_balanced_tree(rl, node(V, L, R, H), BT) :-
  bst_rotate(right, R, BR),
  bst_rotate(left, node(V, L, BR, H), BT).

bst_insert(nil, Val, node(Val, nil, nil, 1)).
bst_insert(node(V, L0, R0, H0), Val, BST) :-
  compare(C, Val, V),
  bst_insert_(C, node(V, L0, R0, H0), Val, node(_, L, R, _)),
  height_updated(L, R, H),
  balance(node(V, L, R, H), B),
  once(balance_compare(BC, B, Val, node(V, L, R, H))),
  bst_balanced_tree(BC, node(V, L, R, H), BST).

bst_insert_(<, node(V, L0, R, H), Val, node(V, L, R, H)) :- bst_insert(L0, Val, L).
bst_insert_(>, node(V, L, R0, H), Val, node(V, L, R, H)) :- bst_insert(R0, Val, R).

bst_search(T, node(V, L, R, _)) :-
  compare(C, T, V),
  bst_search_(C, T, L-R).

bst_search_(=, _, _).
bst_search_(<, T, L-_) :- bst_search(T, L).
bst_search_(>, T, _-R) :- bst_search(T, R).
 

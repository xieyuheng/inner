:- module(cicada, [add/3,
                   append/3]).

add(0, N, N).
add(succ(M), N, succ(O)):-
  add(M, N, O).

append([], List2, List2).
append([Head1|Tail1], List2, [Head1|Tail2]):-
  append(Tail1, List2, Tail2).

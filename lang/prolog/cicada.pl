:- module(cicada, [add/3,
                   append/3]).

add(0, N, N).
add(succ(M), N, succ(O)):-
  add(M, N, O).

append([], List2, List2).
append([Head1|Tail1], List2, [Head1|Tail2]):-
  append(Tail1, List2, Tail2).

prefix(P, L):-
  append(P, _, L).

suffix(S, L):-
  append(_, S, L).

sublist(Sub, L):-
  suffix(S, L),
  prefix(Sub, S).

%% reverse([], []).
%% reverse([Head|Tail], R):-
%%   append(R1, [Head], R),
%%   reverse(Tail, R1).

reverse(L, R):-
  reverse(L, R, []).
reverse([], R, R).
reverse([Head|Tail], R, Ac):-
  reverse(Tail, R, [Head|Ac]).

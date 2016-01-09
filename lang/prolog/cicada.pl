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


f(a).
f(b).
f(c).

g(a).
g(b).

h(X):-
  f(X),
  g(X).

k(X):-
  f(X),
  g(X),
  h(X).

%% k(Y).

%% X = f(X).
%% X = f(X), Y = f(Y).
%% X = f(Y), Y = g(X).
%% X1 = f(Y1), Y1 = g(X1), X2 = f(Y2), Y2 = g(X2).
%% X1 = f(Y1), Y1 = g(X1), X2 = f(g(X2)).

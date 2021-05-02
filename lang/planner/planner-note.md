# planner note

## info

- https://en.wikipedia.org/wiki/Planner_(programming_language)
-  planner--a-language-for-manipulating-models-and-proving-theorems-in-a-robot

## the procedural use of logic proposition

P implies Q

- f: P -> Q

Forward chaining (antecedently)
If assert P, assert Q
If assert not Q, assert not P

- f: P -> Q
  given x: P we can construct element of Q by f(x).

Backward chaining (consequently)
If goal Q, goal P
If goal not P, goal not Q

- f: P -> Q
  to search for element of Q,
  it is sufficient to search for element of P -- x,
  and apply f to x, to get element of Q -- f(x).

## for AI

PLANNER 是为 AI 而设计的，
假设我们有一个可以形式化 deduction 的语言，
那么如果给它增加新的功能，
我们就可以模仿人类解决问题时的认知过程：

（1）人所相信的命题是可变的。
因此我们应该做一个定理的数据库，并且可以更新其中的命题与定理。
数据库中的命题带有时间信息，如果更新一个命题，
导致依赖它的命题不成立了，我们可以不删除不成立的命题，
而是标记出是什么时候的什么修改导致了它不成立。

（2）人在解题是有很多启发式的（heuristic）程序。
我们应该设计 plugin 机制，
使得 proof search 的过程可以被用户的启发式程序自定义。

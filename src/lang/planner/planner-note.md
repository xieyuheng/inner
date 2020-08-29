# planner note

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

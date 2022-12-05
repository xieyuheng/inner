---
title: Constraint logic programming: A survey
authors: [Joxan Jaffar, Michael J. Maher]
year: 1994
---

# 1. INTRODUCTION

Constraint Logic Programming (CLP)
began as a natural merger
of two declarative paradigms:
constraint solving and logic programming.

The key features in CLP:

- Constraints are used to specify the query as well as the answers.
- During execution, new variables and constraints are created.
- The collection of constraints in every state is tested as a whole
  for satisfiability before execution proceeds further.

In summary, constraints are: used for input/output, dynamically generated,
and globally tested in order to control execution.

- **Xie:** We call the state "solution".

## 1.1. Constraint Languages

Considerable work on constraint programming languages
preceded logic programming and constraint logic programming.

We now briefly survey some important works,
with a view toward the following features.

- Are constraints used for input/output?
- Can new variables and/or constraints be dynamically generated?
- Are constraints used for control?
- What is the constraint solving algorithm,
- And to what extent is it complete?

## 1.2. Logic Programming

Next, we consider conventional logic programming (LP),
and argue by example that the power of CLP
cannot be obtained by making simple changes to LP systems.

The question at hand is whether predicates in a logic program
can be meaningfully regarded as constraints.

That is, is a predicate with the same declarative semantics as a constraint
a sufficient implementation of the constraint as per CLP?

Consider, for example, the logic program:

```
add(O, N, N).
add(s(N), M, s(K)) :- add(N, M, K).
```

The query `add(N, M, K), add(N, M, s(K))`,
which is clearly unsatisfiable,
runs forever in a conventional LP system.

The important point here is that
a global test for the satisfiability
of the two add constraints is not done
by the underlying LP machinery.

More concretely, the second subgoal of the query above
is not given a representation of the fact that `N + M = K`.

Since LP is an instance of CLP,
in which constraints are equations over terms,
its solver also requires a representation of accumulated constraints.

It happens, however, that there is no need for an explicit representation,
such as the extra arguments discussed above.
This is because the accumulated constraints
can be represented by a most general unifier, and this, of course,
is globally available via a simple binding mechanism.

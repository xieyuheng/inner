---
title: Constraint handling rules
author: Thom Frühwirth
year: 2009
---

# Preface

Rules have a double nature, they can express monotonic static causal
relations on the basis of logic, but also nonmonotonic dynamic
behavior by describing state changes.

# 1 Getting started

## 1.1 How CHR works

**Example 1.1.1 (Weather)** Everybody talks about the weather, and we
do as well.

These rules are so-called **propagation rules**, recognizable by
`==>`. These kind of rules do not remove any constraints, they just
add new ones.

```prolog
rain ==> wet.
rain ==> umbrella.

% ?- rain.
% rain, wet, umbrella.
```

Computation of a CHR program is initiated by posing a query. The
rules of the program will be applied to the query until exhaustion,
i.e. until no more change happens. The rule applications will
manipulate the query by removing CHR constraints and by adding
constraints. The result of the computation is called the answer, it
simply consists of the remaining constraints.

If we pose the query `rain`, the answer will be `rain, wet, umbrella`
(not necessarily in that order).

If we write instead the two **simplification rules**

```prolog
rain <=> wet.
rain <=> umbrella.

% ?- rain.
% wet.
```

then the answer to rain will be just `wet`.
The first rule is applied and removes `rain`.

Rules are tried in textual order, in a top-down fashion, and so only
the first rule will ever be applied in our example. In general,
whenever more than one rule application is possible, one rule
application is chosen. A rule application cannot be undone (unlike
Prolog). We thus say that CHR is a _committed-choice language_.

- **Xie:** Deterministic by applying only the first matching rule.
  Thus the order of rule matters.

With propagation rules, we can draw conclusions from existing
information. With simplification rules, we can simplify things, as we
will see. Simplification rules can also express _state change_,
i.e. _dynamic behavior_ through updates.

**Example 1.1.2 (Walk)**

```prolog
left, right <=> true.
forward, backward <=> true.

% ?- left, forward, right, right, forward, forward, backward, left, left.
% left, forward, forward.
```

**Example 1.1.3 (Men and women)**

```prolog
male(X), female(Y) <=> pair(X, Y).

% ?- male(bob), female(alice), male(tom).
% male(tom), pair(bob, alice).

If we replace the simplification rule by a propagation rule, we can
compute all possible pairings, since the male and female constraints
are kept.

```prolog
male(X), female(Y) ==> pair(X, Y).

% ?- male(bob), female(alice), male(tom).
% male(tom),
% male(bob),
% female(alice),
% pair(tom, alice),
% pair(bob, alice).
```

So called **simpagation rule**.

In this type of CHR rule, the constraints left of the backslash `\`
are kept but the remaining constraints of the head, right of the
backslash, are removed.

```prolog
male(X) \ female(Y) ==> pair(X, Y).

% ?- male(bob), female(alice), female(coral), male(tom).
% male(tom),
% male(bob),
% pair(bob, coral),
% pair(bob, alice).
```

- **Xie:** Like linear logic `!`.

  ```
  male(X) \ female(Y)
  male(X)!, female(Y)
  ```

**Example 1.1.4 (Family relationships I)** The following propagation
rule named `mm` expresses that the mother of a mother is a
grandmother. The constraint `grandmother(joe, sue)` reads as “The
grandmother of Joe is Sue”.  The use of variables in the rule `mm`
should be obvious.

```prolog
mm @ mother(X, Y), mother(Y, Z) ==> grandmother(X, Z).

% ?- mother(joe, ann), mother(ann, sue).
% mother(ann, sue),
% mother(joe, ann),
% grandmother(joe, sue).
```

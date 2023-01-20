---
title: Essentials of Constraint Programming
authors: [Thom Frühwirth, Slim Abdennadher]
year: 2003
---

# 4. Logic Programming

## 4.1 LP Calculus

### 4.1.2 Operational Semantics

Calculus is defined by state transition.

The state is one solution of
a goal (a conjunction of goals)
and a substitution.

Thus non-deterministic is not part of the calculus,
which requires a queue of solutions.

In our implementation we have `Solver` which serve as the state:

```
class Solver {
  solutions: Array<Solution>
  partialSolutions: Array<Solution>
}
```

and `pursue` function:

```
pursue(Mod, Env, Solution, goal: Goal): Array<Solution>
```

which serve as the transition function,
and handles non-deterministic by returning a array of solutions.

**Non-determinism:**

Given a state `<A and G, θ>`, there are two degrees of non-determinism
in the calculus when we want to reduce it to another state:

- Any atom in the conjunction `A and G` can be chosen as the atom `A`
  according to the congruence defined on states.

- Any clause `(B <- H)` in `P` for which `B` and `A θ` are unifiable
  can be chosen.

This two kinds of non-determinisms are different,

- We need to try all choices of clause to keep the search complete.

- We do not need to try all choices of atom,
  because different choices only effect the performance of the search
  and will give the same result (if not going to infinite loop).

- **Xie:** Whenever we want to try all choices of a non-determinism,
  we need to keep a queue of all branches of states,
  and different strategy for selecting next state from the queue
  gives different search strategy,
  for examples depth-first and breadth-first.

## 4.2 Declarative Semantics

A list of clauses understand in first-order logic,
is the universal closure of the conjunction of the clauses.

For example:

```
clause Append([], l, l)
clause Append([a | d], l, [a | r]) -- { Append(d, l, r) }
```

Should be understand as:

```
conj([
  forall (l) Append([], l, l) <- Succeed(),
  forall (a, d, l, r) Append([a | d], l, [a | r]) <- Append(d, l, r),
])
```

The semantics (meaning) of "negation as failure"
can be expressed in first-order logic easily.

## 4.3 Soundness and Completeness

**Soundness:**

Everything that is derivable (in the sense of the operational semantics),
should also logically follow (in the sense of first-order logic) from the program (the clauses).

**Completeness:**

Everything logically follow (in the sense of first-order logic) from the program (the clauses),
should also be derivable (in the sense of the operational semantics).

**Theorem 4.3.1** (Soundness and Completeness of successful derivations).

Let `P` be a logic program, `C` be a goal, and `θ` be a substitution.

- **Soundness:** If `θ` is a computed answer of `G`, then

  ```
  P |= forall (...) G θ
  ```

- **Completeness:**

  If

  ```
  P |= forall (...) G θ
  ```

  then a computed answer `a` of `G` exists, such that `0 = a b`.

  The relationship `0 = a b` means that the computed answer can be
  more general. This is because an answer need not instantiate all
  variables and any instance of it also follows from the logical
  reading of the program.

**Definition 4.3.1** A derivation is fair if it either fails or if
each atom appearing in a derivation is selected after finitely many
reductions.

In other words, given a state, there is no atom that is ignored
forever, i.e., never selected.

**Theorem 4.3.2** (Soundness and Completeness offailed derivations).

Let `P` be a logic program and `G` be a goal. Any fair derivation starting with
`<G, empty>` fails finitely if and only if

```
P and Clark's completion |= not exists (...) G
```

Such a result on failed derivations would not exist without Clark's
completion of a program. Note that SLD resolution does not admit fair
derivations in any case, since always the left most atom of a state is
chosen for `Unfold`.

# 5. Constraint Logic Programming

CLP languages combine the advantages of LP languages
(declarative, for arbitrary predicates, non-deterministic)
with those of constraint solvers
(declarative, efficient for special predicates, deterministic).

- **Xie:** Note that a constraint solver is deterministic.

The solution state is the remaining goals and the constraints,
substitution is merged into constraints.

- **Xie:** Maybe we should not
  merged substitution into constraints
  in practical implementation.

- **Xie:** Arbitrary first-order theory can be used as the constraint solver,
  does this mean that we can use guarded hypergraph rewriting
  to implement any first-order theory?

# 6. Concurrent Constraint Logic Programming

In CCLP, concurrently executing processes communicate via a common
constraint store. The processes are defined by predicates. Constraints
take the role of (partial) messages and variables take the role of
communication channels.

**Xie:** Maybe we can implement this using couchdb-like revison.

**Xie:** Since constraints can only increase,
maybe we can use the propagator model of Sussman.

# 7. Constraint Handling Rules

**Xie:** We let rules guarded by predicates instead of built-in constraints.

**Xie:** Is it really necessary to distinguish
simplification rules from propagation rules?

- Maybe by distinguishing them we can view constraints as linear resource,
  instead of dedup multiple occurance of the same constraint.

CHR is not hypergraph rewriting,
because the state consists of goals and constraints,
and a rule can only match atoms in goals but not in constraints.

- Is there a transition that move atoms from constraints back to goals?

The `Propagate` transition is like the `Simplify` transition, except
that it keeps the constraints `E` in the state. Trivial
non-termination is avoided by not applying a rule a second time to the
same constraints.

- **Xie:** Not that a `Propagate` rule is a linear resource that can only be applied once,
  but that it can only be applied to the same constraints once.

- **Xie:** Also note that there is no dedup on the constraints store,
  viewed a labelled hypergraph, this means
  multiple edges of the same labelled is allowed.

# 8. Constraint Systems and Constraint Solvers

# 9. Boolean Algebra B

# 10. Rational Trees RT

# 11. Linear Polynomial Equations R

# 12. Finite Domains FD

# 13. Non-linear Equations I

# A. Foundations from Logic

## A.1 First-Order Logic: Syntax and Semantics

A first-order language can be viewed as a class and its instances.

The signature of the language is the class,
an interpretation of the language is an instance of the class.

Take `Arithmetic` for example.

```
class Arithmetic {
  Element: Type

  zero: Element
  one: Element

  add(Element, Element): Element
  mul(Element, Element): Element

  Eq(Element, Element): Proposition
  Gt(Element, Element): Proposition
  Lt(Element, Element): Proposition
}
```

Logic symbols are function over `Proposition`.

```
Truth: Proposition
Absurd: Proposition
Not(Proposition): Proposition
And(Proposition, Proposition): Proposition
Imply(Proposition, Proposition): Proposition
```

`forall` and `exists` are syntax keywords
that introduce bound variables.

(We do not use "proposition as type" here.)

```
forall (x: Element, y: Element) Eq(add(x, y), add(y, x)) : Proposition
exists (x: Element) Eq(x, add(one)) : Proposition
```

`Proposition` is a subtype of `Type` with additional axioms
-- primitive functions without computational meaning.

TODO

`I, n |= F`

`I |= F`

Maybe we should not understand first-order language
by "proposition as type" at all.

An interpretation is actually an interpreter
in the sense of EOPL's "writing an interpreter".

If we only use one `env`,
we get not only first-order language,
but also higher-order language,
and in normal interpreter,
we are not limited to one universe.

But when "writing an interpreter",
we do not have `forall` and `exists` in our language.

## A.2 Basic Calculi and Normal Forms

A _calculus_ is a deduction system defined by a list of inference rules.

If we want to have a term-syntax to record proof,
we will inevitably reach for function as term of `forall`
and pair as term of `exists`.

The _resolution calculus_ is a deduction system,
in which no term-syntax is required.

But maybe we still can provide a term-syntax for resolution calculus,
just to record the steps, not to use the reducion of term as a computational model.

But maybe the satisfiability can be verified without the proof (steps of deduction).

- But the `find` query can only handle `exists`, not `forall`.

---
title: Constraint logic programming: A survey
authors: [Joxan Jaffar, Michael J. Maher]
year: 1994
---

<question>
  Horn clauses only handle part of first-order logic,
  can full first-order logic provide a framework
  to talk about constraints?

  <answer>
    TODO
  </answer>
</question>

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

- **Xie:** We call the state "solution" in our implementation.

  Constraint logic programming seems to me
  much like how a person solve a problem
  by working on partial solutions until some of them are complete.

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

## 1.3. CLP Languages

Viewing the subject rather broadly,
constraint logic programming can be said to involve
the incorporation of constraints and constraint "solving" methods
in a logic-based language.

This characterization suggests the possibility of many interesting languages,
based on different constraints and different logics.

However, to this point, work on CLP
has almost exclusively been devoted to
languages based on Horn clauses.

# 2. CONSTRAINT DOMAINS

- **Xie:** My understanding of the terminology:

  | paper                  | programming                      |
  |------------------------|----------------------------------|
  | signature              | class                            |
  | domain                 | instance                         |
  | formulas (constraints) | first-order formulas about class |

We assume:

- Formulas come from first-order language.

- A signature has `=` predicate.

- The class of constraints is closed under

  - variable renaming,
  - conjunction,
  - existential quantification.

    - **Xie:** No need universal quantification, why?

## Example 2.1

```cicada
class Arithmetic {
  Element: Type

  zero: Element
  one: Element
  add: (Element, Element) -> Element
  mul: (Element, Element) -> Element

  // We use the following syntax for predicates (relations).
  Eq: [Element, Element]
  Lt: [Element, Element]
  LtEq: [Element, Element]
}

let realArithmetic = new Arithmetic {
  Element: RealNumber,
  ...,
}

let rationalArithmetic = new Arithmetic {
  Element: RationalNumber,
  ...,
}

class LinearArithmetic {
  Element: Type

  zero: Element
  one: Element
  add: (Element, Element) -> Element
  // mul: (Element, Element) -> Element

  Eq: [Element, Element]
  Lt: [Element, Element]
  LtEq: [Element, Element]
}
```

Example constraints:

```whereabouts
exists (y) {
  Lt [add(mul(5, x), y), 3]
  Lt [x, sub(y, 1)]
}

// with `!=`

conj {
  LtEq [add(mul(2, x), y), 0]
  x != y
}
```

Prolog and standard logic programming
can be viewed as constraint logic programming
over the constraint domain of finite trees.

https://en.wikipedia.org/wiki/Herbrand_structure

# 3. LOGICAL SEMANTICS

# 4. FIXEDPOINT SEMANTICS

# 5. TOP-DOWN EXECUTION

# 6. SOUNDNESS AND COMPLETENESS RESULTS

# 7. BOTTOM-UP EXECUTION

# 8. CONCURRENT CONSTRAINT LOGIC PROGRAMMING

# 9. LINGUISTIC EXTENSIONS

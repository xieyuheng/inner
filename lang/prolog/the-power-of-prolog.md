The Power of Prolog

- project: https://github.com/triska/the-power-of-prolog
- book: https://www.metalevel.at/prolog
- videos: https://www.metalevel.at/prolog/videos
- channel: https://www.youtube.com/channel/UCFFeNyzCEQDS4KCecugmotg

# What is logic?

video: https://www.youtube.com/watch?v=nlTZQ0FF2Eo&ab_channel=ThePowerofProlog

Logic is one kind of study of formal languages.

Logic is concerned with the properties of, and relations between:

- syntax -- formulae, sentences, ...
- semantics -- truth, validity, semantic consequence, ...
- inferences -- proofs, theorems, soundness, completeness, ...
  - probabilistic -- about uncertainty and chance
  - inductive -- generalize what we have observed
  - deductive -- draw conclusions by virtue of the syntactic structure of statements.

meta properties:

- strong soundness -- every theorem can be derived from inference rules is a semantic consequence of the axioms.
- strong completeness -- every semantic consequence can be derived by syntactic inferences as a theorem.

task of logician:

- to devise suitable language,
  to express sensible inference rules,
  to reason about statements.

- use inductive reasoning to design deductive rules.

different logics:

- propositional logic -- propositions are atomic
  - can express NP-complete problems -- what does is mean?

- predicate logic -- a family of logics, which can be categorized by their order:

  - first-order predicate logic
    - can (only) quantify over individuals (or over domain elements)
    - can describe how turing machine works -- how about lambda calculus?
      - for any given turing machine,
        we can construct a first-order formula,
        that describe what the machine does, in the sense that
        every state of the machine corresponds to a semantic consequence of the description.
      - for any given turing machine,
        we can construct a first-order formula that is valid if and only if the turing machine halts.
      - thus in first-order logic, validity and satisfiability are in general not decidable.

  - second-order and higher-order predicate logic
    - second-order can quantify over relations between domain elements
    - higher-order can quantify over relations between relations
    - propositional logic can be viewed as zero-order logic, for it does not support quantifiers

  - a general principle -- increased expressiveness has a price

classical v.s. non-classical logic (another way to categorize logics):

- classical (found by Frege):
  - law of excluded middle: (A or (not A))
  - commutativity of conjunction: (A and B) -> (B and A)
  - law of non-contradiction: (not (A and (not A)))
  - ...

- Xie: The author talked about in the domain of mathematical proofs,
  we should not use the law of excluded middle,
  but his arguments are not as convincing as Errett Bishop's arguments.

- intuitionistic logic

- modal logic
  - temporal logic -- about things that change over time
  - epistemic logic -- about knowledge (can be used in legal situation)
  - many value logic
    - lukasiewicz logic
    - fuzzy logic
  - substructural logic
    - linear logic -- about resources
    - relevance logic
  - ...

- we use different logic in different situations,
  logic to programmers is like mathematics for physics.

prolog is a programming language based on a subset of predicate logic:

- mostly first-order logic
- few higher-order and meta-logical features

prolog's execution mechanism is a specific form of theorem proving

- incomplete -- not all logical consequences of a program can be derived
  - the reason of this trade off is for efficient programming
  - but prolog can be used to implement theorem provers

# Predicate Logic

video: https://www.metalevel.at/prolog/videos/predicate_logic

syntax: formula and statement

semantics: assign truth value to statement

An **interpretation**:

- domain: D
- function: (D^n) -> D
- predicate: (D^n) -> boolean

A formula is **satisfiabe** if there is an interpretation under which the formula is true,
such interpretation is called a **model** of the formula.

A formula is **valid** if it is true under all interpretations.

If every interpretation that make A true also make B true,
then B is a **semantic consequences** of A, witten `A |= B`.

- Xie: In general programming language,
  we can implement the relation between theory and model
  by class and object, or by abstract class and class.

  Take group theory for example:

``` typescript
export function imply(x: boolean, y: boolean): boolean {
  return !x || y
}

export function equ(x: boolean, y: boolean): boolean {
  return imply(x, y) && imply(y, x)
}

export abstract class Equivalence<T> {
  abstract eq(a: T, b: T): boolean

  reflexive(x: T): boolean {
    return this.eq(x, x)
  }

  symmetric(a: T, b: T): boolean {
    return equ(this.eq(a, b), this.eq(b, a))
  }

  transitive(a: T, b: T, c: T): boolean {
    return imply(this.eq(a, b) && this.eq(b, c), this.eq(a, c))
  }
}

export abstract class Group<G> {
  abstract equivalence: Equivalence<G>

  eq(a: G, b: G): boolean {
    return this.equivalence.eq(a, b)
  }

  abstract mul(a: G, b: G): G

  mul_associative(a: G, b: G, c: G): boolean {
    return this.eq(this.mul(this.mul(a, b), c), this.mul(a, this.mul(b, c)))
  }

  abstract id: G

  identity_of_mul(a: G): boolean {
    return this.eq(this.mul(this.id, a), a) && this.eq(this.mul(a, this.id), a)
  }

  abstract inv(a: G): G

  inverse_of_mul(a: G): boolean {
    return (
      this.eq(this.mul(this.inv(a), a), a) &&
      this.eq(this.mul(a, this.inv(a)), a)
    )
  }
}
```

- Xie: We can change the underlaying logic by changing `boolean` to other algebras.

  What is the relation between algebra and logic?

- Xie: In type theory however, we define logic within type theory.

  We explain proposition by assigning type of evidence to the proposition.
  When defining inference rules, we always have the syntax with us, to record the evidence, which will form proofs.

If semantics is to be explained by the interpretation of theory in a model,
how should we define a model?

To define a set theory, we have to use the notation of set,
because "the notation of set" is used in
the definition of "the interpretation of theory in a model".

Deductive system:

Main goal: to prove semantic consequences only by syntactic structure.
- without any interpretation.

In this syntactic view, a **proof** of a theorem,
is finite sequence of applications of inference rules,
starting from a set of axioms, ending with the theorem.

A is a **syntactic consequences** of a set of formulas ctx, written `ctx |- A`,
if there is a proof of A from ctx.

Deductive systems for predicate logic:

- Hilbert-style systems (many axioms, few rules) -- good for comparing different logics
- Gentzen-style systems (few axioms, many rules) -- good for understanding (maybe)
  - natural deduction (type system of lambda calculus)
  - sequent calculus
- resolution -- good for automated theorem proving
- ... any other systems? I want to know more.

**Soundness**:     `ctx |- A  =>  ctx |= A`
**Completeness**:  `ctx |= A  =>  ctx |- A`

Deduction theorem (which means we can do conditional proofs):

  `ctx, A |- B  =>  ctx |- A -> B`
       `A |- B  =>      |- A -> B`

- Xie: This "theorem" makes me feel bad,
it makes me feel that we should only use `->` but not `|-`.

TODO Prove validity is not decidable by constructing turing machine in predicate logic.

Notable properties of first-order logic:

- **monotonicity** of entailment: `P |= C  =>  P, A |= C`
  - which means adding things to context is ok.

- **completeness** -- Goedel's completeness theorem.
  - There are many deductive systems for first-order logic which are both sound and complete.

The completeness above is about semantic completeness.

Goedel's incompleteness theorem is about syntactic completeness

A formal system S is **syntacticly complete**
iff for each sentence A of the system,
either `S |- A` or `S |- not(A)`.

Goedel's incompleteness theorem: a consistent formal system which can do arithmetic is incomplete.

- **compactness**: A set S of first-order sentences has a model iff every finite subset of S has a model.
  - compactness is actually equivalent to completeness

TODO I do not understand well the theorems about cardinality at the end of the lecture.

TODO learn about the least Herbrand model.

# Horn clauses

video: https://www.metalevel.at/prolog/videos/horn_clauses

questions:

- the resolvent of two horn clauses is itself a horn clause (meaning?)
- and the resolvent of a goal clause and a definite clause is a goal clause (meaning?)
- info: https://en.wikipedia.org/wiki/Resolution_(logic)

**literal**:
- positive literal -- predicate
- negative literal -- negation of predicate

**clause**: disjunction of literals

**Horn clause**: a clause with at most one positive literal

- definite clause: one positive literal, one or more negation literals
  - one conclusion, many premises
- unit clause: one positive literal, no negation literal
- goal clause: no positive literal, only negation literals

Horn clause in prolog:

- rule:   `H :- G1, G2, ..., Gn.` -- head and goals
- fact:   `H.`
- query:  `  ?- G1, G2, ..., Gn.`

We write horn clauses, and ask for their logical consequences -- programming as theory building.

Horn clauses are a turing complete subset of first-order predicate logic.

Construct a turing machine by Horn clauses:

A turing machine can be expressed by a conjunction of definite Horn clauses,
each definite Horn clause represent a transition,
from one state (one premise) to next state (the conclusion).

The machine reach its final state (it halts) iff `exists x, y: T(final, x, y)`,
this is derivable from the definite calculus iff the turing machine halts.

Consider its negation, we can also be express "not halts" by an unit Horn clause:
`forall x, y: not(T(final, x, y))` -- `?- T(final, x, y)`,
thus the entire clause is unsatisfiable iff the turing machine does halts.

- unsatisfiability of first-order horn clause is only semi-decidable.
- satisfiability of first-order horn clause is undecidable.

TODO Why unsatisfiability is different from satisfiability?

# Predicates

video: https://www.metalevel.at/prolog/videos/predicates

We define predicate by writing down Horn clauses.

A predicate describe relation between its arguments.

Predicate can be defined by function and vice versa,
however predicate is good at expressing relations,
because it make no distinction between input and output.

We can use predicate to express computation
by defining relation between states.

# Clauses, rules and facts

video: https://www.youtube.com/watch?v=x_APqarir-k&feature=youtu.be&ab_channel=ThePowerofProlog

In logic, a clause a disjunction or a conjunction of literals.

A Horn clause is a special *disjunction clause*
where there is at most one positive literal,
which can be written as *implication*
from a body (premises) to a head (conclusion),
where the head is an arbitrary literal of the clause
and the body is the conjunction of the *negations* of the other literals,
the body of each rule is a Prolog *goal*.

A **goal** is a Prolog term that denotes a predicate and its arguments.

We can also read the body as a *constraint* of the clause,
each goal *narrows down* the set of solutions.

A clause is either a rule or a fact.
The clauses that constitute a predicate denote logical alternatives:
If any clause is true, then the whole predicate is true.

# Queries and Answers

video: https://www.metalevel.at/prolog/videos/queries

We put terms in, and we get terms out.

**Queries and Answers are semanticly equivalent.**

- They are just in different forms.
- This is a very important general principle of prolog.

Horn clauses let us state what holds, but not what does not hold.
Thus we can not distinguish intended failure between failure due to imcomplete database.
- `color(purple)` will fail, if we did not yet stated it as a fact.

Prolog's `dif` can find the least different subterm.

``` prolog
?- dif(f(g(X)), f(g(Y))).
dif(Y, X).
```

Such answers are called residual goal, for example:

``` prolog
?- X + Y - 3 #> Y + 8.
X in 12..sup,
Y in inf..sup.
```

# Terms

video: https://www.youtube.com/watch?v=TUjQqvCTwjQ&ab_channel=ThePowerofProlog

In Prolog, all data are represented by Prolog terms.

Each term is either a variable, an atomic term or a compound term.

We do not have to declare term, we just use them.
And they have no inherent meaning.

A term is called *ground* if it contains no variables.
A compound term is called *partially instantiated* if one of its subterms is a variable.

# Operators

video: https://www.youtube.com/watch?v=DepPPfDVSpw&ab_channel=ThePowerofProlog

About infix notations.

Useful predicate to query about operators.

``` prolog
?- current_op(Pri, Fix, Op).
?- write_canonical(A = B).
```

- Xie: Note that, Prolog toplevel is designed to be used as end user -- instead of programmers.

# Lists

video: https://www.youtube.com/watch?v=9JzwUnMSCdA&ab_channel=ThePowerofProlog

Prolog lists are a special case of terms.

- Xie: Good old lisp cons list.

# Pairs

video: https://www.youtube.com/watch?v=EPTad1ysIRE&ab_channel=ThePowerofProlog

A key-value pair is written as: `K - V`

Why hot using list to represent pair? -- like we do in lisp.
- Because it is easier to read. -- nominal v.s. structural -- nominal is easier to read?

# Strings

video: https://www.youtube.com/watch?v=plvBFNi0cVw&ab_channel=ThePowerofProlog

Use list of chars to handle string.

# Type tests

video: https://www.youtube.com/watch?v=ZIv0G4b1xBQ&ab_channel=ThePowerofProlog

The standard predicates for type testing (`atom/1`, `integer/1`, `compound/1` etc.)
are logically flawed because they are not monotonic:
If you use these predicates, then generalizing a query may lead to fewer solutions,
preventing declarative debugging based on logical properties.

For example, `atom/1` fails for the most general query, even though it succeeds for more specific queries:

``` prolog
?- atom(X).
false.

?- X = a, atom(X).
X = a.
```

It is better to raise error then to fail (return false),
thus we have (`atom_si/1`, `integer_si/1` etc.),
`_si` means "sufficiently instantiated",
which can raise instantiation errors on variable.

# Clean vs. Defaulty Representations

video: https://www.youtube.com/watch?v=KJ8uZiWpomg&ab_channel=ThePowerofProlog

When representing data with Prolog terms, ask yourself the following question:

> Can I distinguish the kind of each component from its principal functor?

If this holds, your representation is called **clean**.
If you cannot distinguish the elements by their principal functor,
your representation is called **defaulty**,
a wordplay combining "default" and "faulty".

This is because reasoning about your data will need a "default case",
which is applied if everything else fails.

- Xie: This is like using disjoint union in algebraic data type's sum type.

# Naming Prolog Predicates

video: https://www.youtube.com/watch?v=Uska9DgJEoo&ab_channel=ThePowerofProlog

在 Prolog 中命名 predicate 时，最好把参数相关的信息直接表达咋 predicate name 中。
但是这其实是由于 Prolog 是 untyped language，而在 name 中所表达的信息，其实就是类型信息。

可以 enable 人们使用较简洁的命名的语言功能是：
- 类型系统
- record type

``` prolog
parent_child(P, C)
author_reviewer(P1, P2)
```

``` typescript
parent({ parent: v`p`, child: v`c` })
review({ author: v`p1`, reviewer: v`p2` })
```

对比 minikanren 中的 naming convention，
即直接用 function 的 name，并且在后面加 `o`。

``` prolog
list_without(L1, E, L2)
```

``` typescript
remove({ target: v`l1`, element: v`e`, result: v`l2`})
```

# Prolog Integer Arithmetic

video: https://www.youtube.com/watch?v=sHo6-hk21L8&ab_channel=ThePowerofProlog

CLP -- Constraint Logic Programming
CLP(FD) -- Constraint Logic Programming over finite domain
CLP(Z)  -- Constraint Logic Programming over integer

``` prolog
?- 1 #\= X.
clpz:(X in inf..0\/2..sup).
```

**Constraint propagation** has remove 1 from the domain of X.

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


# Horn clauses

video: https://www.metalevel.at/prolog/videos/horn_clauses

questions:

- the resolvent of two horn clauses is itself a horn clause (meaning?)
- and the resolvent of a goal clause and a definite clause is a goal clause (meaning?)

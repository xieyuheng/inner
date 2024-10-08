---
title: Checking Dependent Types with Normalization by Evaluation: A Tutorial
author: David Thrane Christiansen
source: "https://davidchristiansen.dk/tutorials/nbe"
---

# 1 Evaluating Untyped lambda-Calculus

- Writing an evaluator requires the following steps:
  - Identify the values that are to be the result of evaluation
  - Figure out which expressions become values immediately,
    and which require computation
  - Implement `classes` for the values,
    and use function for computation

## 1.2 The Evaluator

- A closure packages an expression
  that has not yet been evaluated

  with the run-time environment
  in which the expression was created.

  Here, closures always represent expressions with a variable
  that will be instantiated with a value,
  so these closures additionally have the `name` field.

- The evaluator consists of two procedures

  - `exp.eval (env)`
    evaluates an expression in a run-time environment that
    provides values for its free variables,

  - `closure.apply (arg)`
    for applying the value of a function
    to the value of its argument

# 2 Generating Fresh Names

- Normalization requires generating fresh names
  to avoid conflicting variable names.

# 3 Normalizing Untyped λ-Calculus

## 3.1 Normal Forms

- <exp> ::= <id> | (lambda (<id>) <exp>) | (<exp> <exp>)

- equivalence relation of lambda terms.

  - alpha-equivalence:
    consistently renaming bound variables
    doesn’t change the meaning of an expression

  - beta-equivalence:
    applying a lambda-expression to an argument
    is equal to the result of the application

- both rules are equations, which means that
  they can be applied anywhere in an expression
  and that they can be read both from left to right
  and from right to left.

- When we have a collection of equations over syntax,
  the syntax can be seen as divided into various "buckets"
  where each expression in a bucket
  is αβ-equivalent to all the others in its bucket.

- One way to check whether two expressions are in the same bucket
  is to assign each bucket a representative expression
  and provide a way to find the bucket representative
  for any given expression.

- Then, if two expressions are in the same bucket,
  they will have the same representative.
  This canonical representative is referred to as a `normal form`.

- Here, we adopt the convention that normal forms are those
  that contain no reducible expressions, or redexes,
  which is to say that there are no lambda-expressions
  directly applied to an argument.

- Because α-equivalence is easier to check than β-equivalence,
  most people consider normal forms with respect to the β-rule only,
  and then use α-equivalence when comparing β-normal forms.

## 3.2 Finding Normal Forms

- When reducing under lambda,
  there will also be variables that
  do not have a value in the environment.
  To handle these cases,
  we need values that represent `neutral expressions`.

- A neutral expression is an expression that
  we are not yet able to reduce to a value,
  because information such as
  the value of an argument to a function is not yet known.

- In this language, there are two neutral expressions:
  variables that do not yet have a value,
  and applications where the function position is neutral.

- <norm> ::= <neu> | (lambda (<id>) <norm>)
  <neu> ::= <id> | (<neu> <norm>)

# 5 Bidirectional Type Checking

- Bidirectional type checking is a technique
  for making type systems syntax-directed
  that adds only a minimal annotation burden.

- Typically, only the top level of an expression
  or any explicit redexes need to be annotated.

- Additionally, bidirectional type checking provides guidance
  for the appropriate places to insert checks
  of type equality or subsumption.

## 5.2 Checking Types

- When writing a bidirectional type checker,
  the first step is to classify the expressions
  into introduction and elimination forms.

- The introduction forms, also called constructors,
  allow members of a type to be created,
  while the eliminators expose the information
  inside of the constructors to computation.

- In this section,
  the constructor of the `->` type is `lambda`
  and the constructors of `Nat` are `zero` and `add1`.
  The eliminators are function application and `rec`.

- Under bidirectional type checking,
  the type system is split into two modes:
  in checking mode, an expression is
  analyzed against a known type to see if it fits,
  while in synthesis mode,
  a type is derived directly from an expression.

- Each expression for which a type can be synthesized
  can be checked against a given type
  by performing the synthesis
  and then comparing the synthesized type to the desired type.

- This is where subsumption
  or some other nontrivial type equality check can be inserted.

- Additionally, type annotations (here, written e∈A)
  allow an expression that can be checked
  to be used where synthesis is required.

- Usually, introduction forms have checking rules,
  while elimination forms admit synthesis.

# 6 Typed Normalization by Evaluation

## 6.2 The Evaluator

- Just as in Normalizing Untyped λ-Calculus,
  normalization consists of evaluation followed by reading back.

  Here, introduction and elimination rules for natural numbers are included.

- The evaluator works in essentially the same way
  as the evaluator for untyped normalization.

  - Constructor expressions become values,

  - while eliminators delegate to helpers that

    - either compute the right answer
      when the target is a value,

    - or construct larger neutral terms
      when the target is neutral.

## 6.3 Typed Read-Back

- In untyped normalization by evaluation,
  values were examined to determine how to read them back.

  In typed NbE, however,
  each type can specify its own notion of equality,
  and thus the syntax of its normal forms.

  Therefore, reading back is now recursive
  on the structure of the type
  rather than the structure of the value.

# 7 A Tiny Piece of Pie

- Tartlet contains
  functions,
  pairs,
  the natural numbers,
  atoms,
  and the unit
  and empty types.

- Also, the Tartlet type of types, U, is a U.
  This makes it inconsistent as a logic,
  but it is still safe as a programming language.

- x -
  how to show that Tartlet is inconsistent as a logic

## 7.1 The Language

```
<expr> ::=
  <id>
  | ( Pi ( ( <id> <expr> ) ) <expr> )
  | ( lambda ( <id> ) <expr> )
  | ( <expr> <expr> )
  | ( Sigma ( ( <id> <expr> ) ) <expr> )
  | ( cons <expr> <expr> )
  | ( car <expr> )
  | ( cdr <expr> )
  | Nat
  | zero
  | ( add1 <expr> )
  | ( ind-Nat <expr> <expr> <expr> <expr> )
  | ( = <expr> <expr> <expr> )
  | same
  | ( replace <expr> <expr> <expr> )
  | Trivial
  | sole
  | Absurd
  | ind-Absurd
  | Atom
  | ( quote <id> )
  | U
  | ( the <expr> <expr> )
```

### 7.1.1 Identifiers

### 7.1.2 Program α-equivalence

## 7.2 Values and Normalization

- x - about constructor and eliminator
  there are two kinds of `exp_t` -- constructor and eliminator
  - a constructor correspond to a `value_t`
  - a eliminator correspond to a `neutral_t`

- x - about `the_neutral_t`
  a `neutral_t` is not a `value_t`,
  when wish to use a `neutral_t` as `value_t`,
  we must also provide its type,
  thus the structure of `the_neutral_t`.
  a `the_neutral_t` is a `value_t`.

- x - about `the_value_t`
  when a `neutral_t` needs a field contain `value_t`,
  we must also provide the type,
  thus the structure of `the_value_t` (so called normal form),
  but a `the_value_t` is not a `value_t`,
  since it will only occur in fields of `neutral_t`.

- x - summary
  ```typescript
  abstract class value_t {}
  class the_neutral_t extends value_t { t: value_t, neutral: neutral_t }
  abstract class neutral_t { <target>: neutral_t, <field>: the_value_t }
  class the_value_t { t: value_t, value: value_t }
  ```

### 7.2.1 The Values

- Following the recipe for normalization by evaluation,
  we need to define value representations
  of each constructor and type constructor in the language.

- The values represent the introduction forms and the type constructors.
  Because they do not contain any potential computation other than neutral expressions,
  they represent only the values of Tartlet.

### 7.2.2 Neutral Expressions

- At the basis of recursive definition of `neutral_t` is `neutral_var_t`

- Each eliminator in the language (including function application),
  must be able to recognize neutral targets
  and construct a representation of itself as a neutral expression.

### 7.2.3 Normal Forms

- The internal representation of normal forms,
  constructed with "THE",
  pairs a type value with a value classified by the type.

## 7.3 Definitions and Dependent Types

- The simply-typed language
  of Typed Normalization by Evaluation
  could store definitions separately
  from the context and the environment,
  constructing each as needed for type checking or evaluation.

  In a dependently-typed language, however,
  type checking can invoke evaluation.
  This means that the context needs to distinguish between

  - free variables that result from from binding forms
    such as λ, Π, and Σ,
    for which a value is not available during type checking,

  - and free variables that result from from definitions,
    which do have values during type checking.

### 7.3.1 The Evaluator

### 7.3.2 Eliminators

- Each eliminator is realized by a native procedure.
  This procedure checks whether the target of elimination is neutral,
  and if so, it produces a new neutral expression.
  Otherwise, it finds the resulting value.

### 7.3.3 Reading Back

- Just as in "Typed Normalization by Evaluation",
  reading back from values into syntax is accomplished via
  two mutually-recursive procedures: read-back-norm and read-back-neutral.

- Convert a neutral expression into its representation as syntax.
  The only case that is not immediately the same as the others is that for ind-Absurd;
  it adds a type annotation to its target that,
  together with a special case in α-equiv?,
  causes all neutral inhabitants of Absurd to be identified with one another.

## 7.4 Type Checking

### 7.4.1 The Type Checker

- Like Pie and many other implementations of type theory,
  the Tartlet type checker is an elaborating type checker.
  This means that, instead of simply indicating that an expression is well-typed,
  it returns a version of the expression with more details inserted.
  In other words, the language accepted by the type checker
  contains structures that are not understood by the normalizer,
  and the type checker emits expressions in that simpler language.

- In an elaborating bidirectional type checker,
  checking emits an elaborated equivalent of the input expression,
  while synthesis emits both an elaborated expression and its type,
  with its type being in the core language of the normalizer.
  Many elaborating type checkers also re-check the expression in the simpler language;
  to keep these notes shorter, Tartlet dispenses with that step.

- When examining types, looking for specific type constructors,
  the type checker matches against their values.
  This ensures that the type checker never forgets to normalize before checking,
  which could lead to types that contain unrealized computation not being properly matched.
  For instance, the typing rules for ind-Nat
  might give rise to the type ((λ (k) Atom) zero) for the base.
  Attempting to use that expression as the type for 'sandwich would be incorrect without first reducing it.
  Using values, which cannot even represent redexes,
  removes the need to worry about normalization prior to inspection.

### 7.4.2 Type Checking with Definitions

- If input is an expression, check and normalize it.
  If input is a definition, check it and add it to the context.

## 7.5 Projects

## 7.6 Putting It Together

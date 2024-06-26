---
title: the little typer
---

# 1. The More Things Change, the More They Stay the Same

```scheme
'ratatouille
(the Atom 'ratatouille)
(the Atom (the Atom 'ratatouille))

(the (Pair Atom Atom)
  (cons 'ratatouille 'baguette))
```

- The Four Forms of Judgment
  1. ___ is a ___.
  2. ___ is the same ___ as ___.
  3. ___ is a type.
  4. ___ and ___ are the same type.

- The normal form of an expression
  is the most direct way of writing it.
  Any two expressions that are the same have identical normal forms,
  and any two expressions with identical normal forms are the same.

  Given a type, every expression described by that type has a normal form.

```scheme
0 1
(the Nat 0)
(the Nat 1)
(the Nat 1729)

(the Nat zero)
(the Nat (add1 zero))
```

- x -
  Remember Bishop's set
  "To define a set we prescribe, at least implicitly,
  1. what we (the constructing intelligence) must do
     in order to construct an element of the set,
  2. and what we must do to show that
     two elements of the set are equal."

  We used a stronger way to define a type,
  instead of asking for a equivalent relation,
  we are asking for a normal form.
  Thus a program to eval expressions of a type to normal form,
  give us a predicate for equivalent relation.

  We are also explicit about constructing.
  To define a type, we explicit specify its constructors.
  - `Nat` has `zero` and `add1` as constructors
  - primitive type `Atom` has infinitely many constructors

- In Pie,
  expressions do not refer to some external notion of meaning,
  there is nothing but expressions
  and what we judge about them.

  While in Lisp, values are distinct from expressions,
  and the result of evaluation is a value.

- expression
  value (top is constructor)
  normal form (might be neutral)

- x -
  I should do the same in my design.
  - Note that,
    "expression" is about semantics,
    and not about concrete syntax.

# 2. Doin' What Comes Naturally

- Constructors build values,
  and eliminators takeapart values built by constructors.

- `lambda` is a constructor,
  because every expression that looks like
  `(lambda (x0 x ...) <body>)` is a value.

  Applying a function to arguments
  is the eliminator for functions.

- x -
  "Applying a function" seems not like eliminators like
  `car` and `cdr`, which are functions.

  "Applying a function" is builtin to the language.
  `car` and `cdr` can also be builtin as
  "taking fields of record".

```scheme
(check-same (-> Atom Atom (Pair Atom Atom))
  (lambda (a d) (cons a d))
  (lambda (d a) (cons d a)))

(check-same (-> Nat Nat)
  (lambda (y) (car (the (Pair Nat Nat) (cons y y))))
  (lambda (x) x))
```

- x -
- expression
  value (top is constructor)
  normal form (might be neutral)

  strategy
  strategy with explicit next step
  strategy with every steps (might be neutral)

- *Neutral Expressions*
  Expressions that are not values
  and cannot yet be evaluated due to a variable
  are called neutral.

  if `x` is a variable,
  which we only know its type is `(Pair Nat Nat)`,
  but not know its value.
  then `(car x)` or `(cdr x)` are neutral

- Neutral expressions make it necessary to expand our view on
  what it means to be the same.
  Each variable is the same as itself,
  no matter what type it has.

  This is because variables are only replaced consistently,
  so two occurrences of a variable
  cannot be replaced by values that are not the same.

- If two expressions have identical eliminators at the top
  and all arguments to the eliminators are the same,
  then the expressions are the same.

  such as `(car x)` and `(car x)`

- Neutral expressions that are written identically
  are the same, no matter their type.

- *The Second Commandment of cons*
  If p is a (Pair A D),
  then it is the same (Pair A D)
  as (cons (car p) (cdr p)).

```scheme
(which-Nat zero
  'naught
  (lambda (n) 'more))

(check-same Atom
  (the Atom 'naught)
  (which-Nat zero
    'naught
    (lambda (n) 'mor)))

(check-same Atom
  (the Atom 'more)
  (which-Nat (add1 (add1 (add1 zero)))
    'naught
    (lambda (n) 'more)))
```

- cicada-like:

``` typescript
which_Nat: (
  [implicit]: { X: type }
  target: Nat,
  base: X,
  step: (prev: Nat) -> X,
) -> X = {
  case (target) {
    zero => base
    succ => step (target.prev)
  }
}
```

- RECURSION IS NOT AN OPTION
- RECURSION IS NOT AN OPTION
- RECURSION IS NOT AN OPTION

- Recursion is not an option
  because every expression must have a value.

  Some recursive definitions make it possible
  to write expressions that do not have values.

- *Every U Is a Type*
  Every expression described by U is a type,
  but not every type is described by U.
  - such as U and (Pair U U)

# Recess: A Forkful of Pie

- Using Pie is very much like a conversation:
  it accepts claims, definitions, and expressions
  and it replies with feedback.

- For claims and definitions,
  the feedback is whether they are meaningful.

  For expressions,
  the feedback is also the expression's type and normal form.

- When an expression is a type, but does not have a type,
  Pie replies with just its normal form.
  - such as U, (Pair U U), (Pair Atom U), (-> U U)

```scheme
(the (Pair Atom (Pair Atom Atom))
  (cons 'spinach
        (the (Pair Atom Atom)
          (cons 'kale 'cauliflower))))

(the (Pair Atom (Pair Atom Atom))
  (cons 'spinach
        (cons 'kale 'cauliflower)))

(car (the (Pair Atom Nat)
       (cons 'brussels-sprout 4)))
```

# 3. Eliminate All Natural Numbers!

- "same as" chart
  - x -
    base of the format of the game of equivalent

```scheme
| (gauss (add1 zero))
| (add1 (gauss zero))
| (add1 zero)
```

- *Total Function*
  A function that always assigns a value
  to every possible argument is called a total function.

- x -
  | strategy | winning strategy |
  | function | total function   |

- cicada-like:

``` typescript
iter_Nat: (
  [implicit]: { X: type }
  target: Nat,
  base: X,
  step: (almost: X) -> X,
) -> X = {
  case (target) {
    zero => base
    succ => step (iter_Nat (target.prev, base, step))
  }
}

rec_Nat: (
  [implicit]: { X: type }
  target: Nat,
  base: X,
  step: (prev: Nat, almost: X) -> X,
) -> X = {
  case (target) {
    zero => base
    succ => step (
      prev = target.prev,
      almost = rec_Nat (target.prev, base, step),
    )
  }
}

step_add: (almost: Nat) -> Nat =
  succ (almost)
add: (x: Nat, y: Nat) -> Nat =
  iter_Nat (x, y, step_add)

step_gauss: (prev: Nat, almost: Nat) -> Nat =
  add (succ (prev), almost)
gauss: (n: Nat) -> Nat =
  rec_Nat (n, zero, step_gauss)
```

# 4. Easy as Pie

``` typescript
elim_Pair: (
  A: type,
  D: type,
  X: type,
  p: Pair (A, D),
  f: (A, D) -> X,
) -> X = f (p.car, p.cdr)
```

# 5. Lists, Lists, and More Lists

``` typescript
rec_List: (
  [implicit]: { E: type, X: type }
  target: List (E),
  base: X,
  step: (car: E, cdr: List (E), almost: X) -> X,
) -> X = {
  case (target) {
    nil => base
    cons => step (
      car = target.car,
      cdr = target.cdr,
      almost = rec_List (target.cdr, base, step),
    )
  }
}
```

# 6. Precisely How Many?

```scheme
(the (Vec Atom 0) vecnil)
(the (Vec Atom 1) (vec:: 'oyster vecnil))

(the Atom
  (head (the (Vec Atom 1) (vec:: 'oyster vecnil))))
(the (Vec Atom 0)
  (tail (the (Vec Atom 1) (vec:: 'oyster vecnil))))
```

- We avoid attempting to define a non-total function
  by using a more specific type
  to rule out unwanted arguments.

- *Use a More Specific Type*
  Make a function total
  by using a more specific type
  to rule out unwanted arguments.

- (-> Y X) is sugar for (Pi ([y Y]) X)
  when `y` does not occur in `Y`

# 7. It All Depends On the Motive

``` typescript
ind_Nat: (
  target: Nat,
  motive: (target: Nat) -> type,
  base: motive (zero),
  step: (
    prev: Nat,
    almost: motive (prev),
  ) -> motive (add1 (prev)),
) -> motive (target) = {
  case (target) {
    zero => base
    succ => step (
      prev = target.prev,
      almost = ind_Nat (
        target.prev,
        motive,
        base,
        step,
      ),
    )
  }
}
```

- *Readable Expressions*
  Getting the right answer is worthless
  if we do not know that it is correct.
  Understanding the answer is at least
  as important as having the correct answer.

# Recess: One Piece at a Time

```scheme
(claim peas
  (Pi ([n Nat])
    (Vec Atom n)))
(define peas
  (lambda (n)
    (ind-Nat n
      (lambda (k)
        (Vec Atom k))
      TODO
      TODO)))
```

# 8. Pick a Number, Any Number

- Sameness is indeed a judgment.
  But, with a new type constructor,
  types can express a new idea called equality.

  - to write
    "incr and (+ 1) always find the same answer."
    as a type.

- Creating expressions that capture the ideas
  behind a form of judgment is sometimes called
  internalizing the form of judgment.

```scheme
(= Atom 'kale 'blackberries)
```

- A more precise way to define neutral expressions
  is to start with the simplest neutral expressions
  and build from there.

- *Neutral Expressions*
  Variables are neutral,
  unless they refer to definitions,
  because a defined name is the same as its definition.

  Also, if the target of an eliminator expression is neutral,
  then the entire expression is neutral.

``` typescript
cong: (
  [implicit]: {
    X: type,
    Y: type,
    from: X,
    to: X,
  }
  target: eqv_t (X, from, to),
  f: (X) -> Y,
) -> eqv_t (Y, f (from), f (to)) = {
  case (target) {
    same => same (f (target.value))
  }
}
```

# 9. Double Your Money, Get Twice as Much

``` typescript
replace: (
  [implicit]: {
    X: type,
    from: X,
    to: X,
  }
  target: eqv_t (X, from, to),
  motive: (X) -> U,
  base: motive (from),
) -> motive (to) = {
  // TODO
  // What is the Commandment of `replace`?
}
```

- `replace` is used when the type of something nearly fits,
  and the part that doesn't is equal to something
  that would make it fit.

- `replace` is useful because
  by writing an appropriate motive,
  it can have any type.

- *Solve Easy Problems First*
  If two functions produce equal results,
  then use the easier one when defining a dependent function,
  and then use `replace` to give it the desired type.

# 10. It Also Depends On the List

- *Use a Specific Type for Correctness*
  Specific types can rule out foolish definitions.

  - x -
    while specific type express intention

``` typescript
ind_List: (
  [implicit]: { E: type },
  target: List (E),
  motive: (target: List (E)) -> type,
  base: motive (nil),
  step: (
    e: E,
    es: List (E)
    almost: motive (es),
  ) -> motive (list_cons (e, es)),
) -> motive (target) = {
  case (target) {
    nil => base
    list_cons => step (
      e = target.e,
      es = target.es,
      almost = ind_List (
        target.es,
        motive,
        base,
        step,
      ),
    )
  }
}
```

# 11. All Lists Are Created Equal

- These two varieties of arguments to a type constructor,
  that either vary or do not vary, have special names.

  - Those that do not vary,
    such as the entry type in Vec and List,
    are called *parameters*,

  - and those that do vary are called *indices*.

- Whenever a type constructor has an index,
  the index shows up in the motive for its eliminator,
  and therefore also in the step.

  - A family of types whose argument is an index
    is sometimes called "an indexed family."

``` typescript
ind_Vec: (
  [implicit]: { E: type },
  n: Nat,
  target: Vec (E, n),
  motive: (
    k: Nat,
    target: Vec (E, k),
  ) -> type,
  base: motive (zero, vec_nil),
  step: (
    k: Nat,
    head: E,
    tail: Vec (E, k),
    almost: motive (k, tail),
  ) -> motive (succ (k), vec_cons (head, tail)),
) -> motive (n, target) = {
  case (target) {
    vec_nil => base
    vec_cons => step (
      k = n.prev,
      head = target.head,
      tail = target.tail,
      almost = ind_Vec (
        n.prev,
        target.tail,
        motive,
        base,
        step,
      ),
    )
  }
}
```

- When writing a Curried motive, base, or step,
  it pays to carefully consider the order of arguments.

- Thus far, we have used more specific types
  to rule out foolish definitions.
  Another way to rule out foolish definitions
  is to prove that they are not foolish.

  - Sometimes,
    using a more specific type is called an *intrinsic* proof.
    Similarly, using a separate proof is called *extrinsic*.

- x -
  when proving things like `step-list->vec->list=`
  the insight lies in the `same-as` chart of equivalence,
  (such as, "observation about `+`"
  and "observation about `list->vec`")
  while the prover often does not provide such features.

  - although a prover can be interactive,
    but since the interaction is not recorded,
    the whole point of formalization is missed.

  - in a better prover, we must
    not mimic the obscure variable names that mathematician use,
    but capture the essence of reasoning of mathematician.

  - for example,
    when proving equivalence, we could make it more intuitive
    to reduce the equivalent to smaller one.

```scheme
(-> (= (List E)
      es
      (vec->list E (length E es) (list->vec E es)))
    (= (List E)
      (:: e es)
      (vec->list E (length E (:: e es)) (list->vec E (:: e es)))))

;; working from the succedent of the step
(= (List E)
  (:: e es)
  (vec->list E (length E (:: e es)) (list->vec E (:: e es))))
;; ==>
(= (List E)
  (:: e es)
  (vec->list E (add1 (length E es)) (vec:: e (list->vec E es))))
;; ==>
(= (List E)
  (:: e es)
  (vec->list E (add1 (length E es)) (vec:: e (list->vec E es))))

;; we will need the above to equal
(= (List E)
  (:: e es)
  (:: e (vec->list E (length E es) (list->vec E es))))
```

- *When in Doubt, Evaluate*
  Gain insight by finding the values of expressions in types
  and working out examples in "same-as" charts.

- Note that,
  `step-list->vec->list=` does not uniquely determine
  the definitions of `list->vec` and `vec->list`.

# 12. Even Numbers Can Be Odd

- Although two functions always return the same answer,
  sometimes one of them is easier to use
  because it more quickly becomes a value.

  In particular, `+` and thus `twice`
  leave an `add1` on the second argument,
  while `double` puts both `add1`s at the top immediately.

- *Carefully Choose Definitions*
  Carefully-chosen definitions can greatly simplify later proofs.

# 13. Even Haf a Baker's Dozen

# 14. There's Safety in Numbers

# 15. Imagine That ..

# 16. If It's All the Same to You

# A. The Way Forward

## A Universe Hierarchy

## Inductive Datatypes

## Recursive Functions with Pattern Matching

- The basic principle of eliminators is that
  for each constructor, we need to explain
  what must be done to satisfy the motive
  using the information inside the constructor.

## Implicit Arguments

## Proof Tactics

# B. Rules Are Made to Be Spoken

## Intro

- When implementing dependent types,
  there are two questions to be answered:
  when to check for sameness,
  and how to check for sameness.

- Our implementation of Pie
  uses bidirectional type checking
  (described in the section "Forms of Judgment")
  to decide when,
  and normalization by evaluation
  (described in the section "Normalization")
  as the technique for checking sameness.

## Forms of Judgment

- x -
  "Forms of Judgment (with output after `~>`)"
  are implemented as functions.

  Forms specify the type of functions,
  and inference rules specify the body of functions.

  an inference rule is
  a piece of function body written reversely,

  which might be viewed as
  an abstract method of an abstract class
  implemented by many concrete methods of concrete classes.

- x -
  "Forms of Judgment (without output)"
  are implemented as predicates which can success or fail.

  in a practical implementation,
  we must return error report when fail.

- When reading the rules as an algorithm,
  each form of judgment should be implemented as a function.

  |            | Input        | Output       |
  |------------+--------------+--------------|
  | Conclusion | Pattern      | Construction |
  | Premise    | Construction | Pattern      |

  - When an expression occurs in input position
    in the conclusion of an inference rule,
    it should be read as a pattern to be matched against the input.

  - When it is in output position,
    it should be read as
    constructing the result of the algorithm.

  - When an expression occurs in an input position in a premise,
    it represents input being constructed for a recursive call,

  - and when it occurs in the output position in a premise,
    it represents a pattern to be matched
    against the result returned from the recursive call.

- When matching against a concrete expression in a rule,
  the algorithm must reduce the expression enough
  so that if it doesn't match,
  further reduction cannot make it match.

  Finding a neutral expression
  or a value that is the same as
  the expression being examined is sufficient.

  - x -
    by "the same as" we mean definitional equality.

## Normalization

- The process of checking whether the judgments
  ctx :- c1 = c2 type
  ctx :- c1 = c2 : ct
  are believable, is called *conversion checking*.

- To check for conversion,
  the Pie implementation uses a technique called
  normalization by evaluation or NbE for short.

- The essence of NbE is to define a notion of value
  that represents only the normal forms of the language,
  and then write an interpreter
  from Core Pie syntax into these values.

- Then, the value's type is analyzed to determine
  what the normal form should look like,
  and the value itself is converted back into syntax.

  Converting a value into its normal form
  is called *reading back* the normal form from the value.

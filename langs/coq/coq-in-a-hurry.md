---
title: Coq in a Hurry
author: Yves Bertot
year: 2015
---

# 1 Expressions and logical formulas

## 1.1 Writing correct formulas

### `Check`

```coq
Check True.
Check False.
Check 1 + 2.
Check 3 = 5 /\ True.
Check (3, 5).
Check nat.
Check nat -> Prop.
Check 3.
Check True: Prop.
Check (3, 3 = 5): nat * Prop.
```

### `fun`

```coq
Check fun x: nat => x = 3.
Check (fun x: nat => x = 3) 1.

Check
  forall x: nat,
    x < 3 \/ exists y: nat, x = y + 3.
```

```cicada
check forall (x: Nat) Either(
  LessThen(x, three),
  exists (y: Nat) Equal(Nat, x, add(y, 3))
)
```

### `let`

```coq
Check
  let f :=
    fun x => (x * 3, x)
  in f 3.
```

### `Locate`

To find the function hidden behind a notation.

```coq
Locate "_ -> _".
Locate "_ \/ _".
Locate "_ <= _".
Locate "_ + _".

Locate and.
Locate plus.
Locate sum.
Locate nat.
```

## 1.2 Evaluating expressions

### `Eval`

```coq
Compute
  let f := fun x => (x * 3, x)
  in f 3.

Check
  fun x: nat =>
  fun y: nat =>
    plus x y.

Compute
  let add: nat -> nat -> nat :=
    fun x: nat =>
    fun y: nat =>
      plus x y
  in add 4 3.
```

# 2 Programming in Coq

## 2.1 Defining new constants

### `Definition`

```coq
Definition example1(x : nat) := x*x + 2*x + 1.

Check example1.
Compute example1 100.

Reset example1.

Definition example1 := fun x : nat => x*x + 2*x + 1.

Print example1.
```

## 2.2 boolean conditional expressions

```coq
Compute
  if true
  then 3
  else 5.

Check true.

Check True.
```

### `Search`

To search definitions related to a given type.

The command `SearchPattern` takes a pattern as argument
and returns any symbol whose type finishes with that pattern.

The command `Search` takes a list of patterns as arguments
and returns any symbol whose type contains all the patterns in the list.

```coq
Search bool.
Search Prop.
Search bool Prop.

SearchPattern bool.
SearchPattern Prop.
```

# 2.3 Computing with natural numbers

```coq
Compute S (S (S 0)).

Definition is_zero(n: nat): bool :=
  match n with
  | 0 => true
  | S p => false
  end.

Compute is_zero 1.
Compute is_zero 0.

Definition nat_sub1(n: nat): nat :=
  match n with
  | 0 => 0
  | S p => p
  end.

Compute nat_sub1 1.
Compute nat_sub1 0.

Print pred.
Print Nat.pred.

Compute pred 1.
Compute pred 0.
```

### `Fixpoint`

Coq is very sensitive to recursive definitions,
and we have to use `Fixpoint` keyword to express recursion.

When describing a recursive function,
we have to respect a constraint called _structural recursion_.

The recursive call can only be made on a subterm of the initial argument.
More precisely the argument of the recursive call must be a variable,
and this variable must have been obtained from the
initial argument through pattern matching.

```coq
Fixpoint sum_n(n: nat): nat :=
  match n with
  | 0 => 0
  | S p => p + sum_n p
  end.

Compute sum_n 10.

Fixpoint evenb(n: nat): bool :=
  match n with
  | 0 => true
  | 1 => false
  | S (S p) => evenb p
  end.

Compute evenb 100.
Compute evenb 101.
```

## 2.4 Computing with lists

```coq
Require Import List.

Check 1 :: 2 :: 3 :: nil.
Check nil.
Check nil : list nat.

Compute map (fun x => x + 3) (1 :: 3 :: 2 :: nil).
Compute map (fun (x: nat) => x + 3) (1 :: 3 :: 2 :: nil).

Compute map S (1 :: 2 :: 3 :: nil).

Compute
  let l := 1 :: 2 :: 3 :: nil
  in l ++ map (fun x => x + 3) l.
```

## 2.5 Finding more functions

```coq
Require Import List.

Fixpoint sum_list(l: list nat) :=
  match l with
  | nil => 0
  | n :: tl => n + sum_list tl
  end.

Compute sum_list (1 :: 2 :: 3 :: nil).

SearchPattern (nat -> nat -> bool).

Compute Nat.eqb 1 1.

Fixpoint gteq(x y: nat): bool :=
  match x with
  | 0 =>
      match y with
      | 0 => true
      | S k2 => false
      end
  | S k1 =>
      match y with
      | 0 => true
      | S k2 => gteq k1 k2
      end
  end.

Fixpoint insert(n: nat)(l: list nat): list nat :=
  match l with
  | nil => n :: nil
  | a :: tl =>
      if gteq a n
      then n :: l
      else a :: insert n tl
  end.

Fixpoint sort(l: list nat): list nat :=
  match l with
  | nil => nil
  | a :: tl => insert a (sort tl)
  end.

Compute sort (1 :: 4 :: 3 :: 22 :: 5 :: 16 :: 7 :: nil).

Fixpoint is_sorted(l: list nat): bool :=
  match l with
  | nil => true
  | a :: nil => true
  | a1 :: a2 :: nil => gteq a2 a1
  | a1 :: a2 :: tail =>
      if gteq a2 a1
      then
        match l with
        | nil => true
        | a1 :: tail => is_sorted tail
        end
      else false
  end.

Compute is_sorted (1 :: 2 :: 3 :: nil).
Compute is_sorted (1 :: 4 :: 3 :: nil).

Fixpoint count_list(n: nat)(l: list nat): nat :=
  match l with
  | nil => 0
  | a :: tail =>
      if Nat.eqb a n
      then 1 + count_list n tail
      else count_list n tail
  end.

Compute count_list 1 (1 :: 2 :: 1 :: nil).
Compute count_list 3 (1 :: 2 :: 1 :: nil).
```

# 3 Propositions and proofs

The semantices of `x: A`

1. `x` is proof of logical formula `A`
2. `x` is of the type `A`

## 3.1 Finding existing proofs

```coq
Search True.

SearchPattern True.
SearchPattern (_ <= _).
```

## 3.2 Constructing new proofs

The usual approach to construct proofs is known as _goal directed proof_,
with the following type of scenario:

1. the user enters a statement that they want to prove, using the command `Theorem`
   or `Lemma`, at the same time giving a name for later reference,
2. the Coq system displays the formula as a formula to be proved, possibly giving a
   context of local facts that can be used for this proof (the context is displayed above
   a horizontal line written `=====`, the goal is displayed under the horizontal line),
3. the user enters a command to decompose the goal into simpler ones,
4. the Coq system displays a list of formulas that still need to be proved,
5. back to step 3.

The commands used at step 3 are called _tactics_.

When there are no more goals the proof is complete.

There is a large collection of tactics in the Coq system,
each of which is adapted to a shape of goal.

- **Xie**: Unlike function which takes many premises and return a conclusion,
  a tactic take a _goal_ and returns more goals,
  where a goal has two parts, a context and a conclusion.

- **Xie**: How to understand the space of goals?

  - Is it different from the space of types?

    Think about sequent calculus and a stack of goals.

  - Can we express a goal as a type and view a tactic as a function?

    We will need record types, because in the context,
    the order of hypotheses does not matter.

```coq
Lemma and_comm:
  forall A B: Prop,
    A /\ B -> B /\ A.
Proof.
  intros A B.
  intros both.
  split.
  destruct both as [first second].
  exact second.
  destruct both as [first second].
  exact first.
Qed.
```

Use `elim` and `case` instead of `destruct both as [first second]`.

```coq
Lemma and_comm:
  forall A B: Prop,
    A /\ B -> B /\ A.
Proof.
  intros A B.
  intros both.
  split.
  elim both.
  intros first second.
  exact second.
  elim both.
  intros first second.
  exact first.
Qed.
```

Without tactics.

```coq
SearchPattern (_ /\ _).

(* conj: forall [A B : Prop], A -> B -> A /\ B *)

Search (_ /\ _).

(* proj2: forall [A B : Prop], A /\ B -> B *)
(* proj1: forall [A B : Prop], A /\ B -> A *)

Definition and_comm(A B: Prop): A /\ B -> B /\ A :=
  fun both =>
    conj
      (proj2 both)
      (proj1 both).
```

- **Xie**: I think the problem of Coq's tactics,
  is that they are not connected with functions
  in simple and explicit ways.

- **Xie**: While the following polynomial equalities seems requiring proof search.

  But maybe not, if we can reduce symbolic polynomial to normal form and so on.

```coq
Require Import Ring.
Require Import Arith.

Theorem simple_poly:
  forall (x : nat),
    (x + 1) * (x + 2) = x * x + 3 * x + 2.
Proof.
  intros. ring.
Qed.

Compute simple_poly 10.

Fixpoint sumn(n : nat): nat :=
  match n with
  | 0 => 0
  | (S n') => n + (sumn n')
  end.

Theorem sum_formula:
  forall n,
    2 * (sumn n) = (n + 1) * n.
Proof.
  intros n.
  induction n.
  - reflexivity. (* 0 = 0 base case *)
  - simpl.
    ring [IHn]. (* induction step *)
Qed.
```

### `intros x y`

Introduce hypotheses into context by lambda.

```
fun x y => ...
```

- **Xie**:

  其实证明定理就像是在有向图中行走，
  看能走到哪里就算证明到了哪里。
  而当我证明了一个带有全称量词的定理的时候，
  就相当于我熟悉了这个有向图中的某种模式的道路。
  当我再次遇到这个种道路的时候，
  我就可以直接到达道路的那一头，
  而不用再一步一步地走了。

### `destruct both as [first second]`

```
let first = proj1 both
let second = proj2 both
...
```

### `exact v`

Lookup the type of `v` in the context.

### `intuition`

Let Coq search the proof.

## 3.3 More examples using tactics

```coq
Lemma or_comm:
  forall A B: Prop,
    A \/ B -> B \/ A.
Proof.
  intros A B.
  intros either.
  destruct either as [left | right].
  right.
  exact left.
  left.
  exact right.
Qed.
```

Use `elim` and `case` instead of `destruct either as [left | right]`.

```coq
Lemma or_comm:
  forall A B: Prop,
    A \/ B -> B \/ A.
Proof.
  intros A B.
  intros either.
  elim either.
  intro left.
  right.
  exact left.
  intro right.
  left.
  exact right.
Qed.
```

Without tactics.

```coq
SearchPattern (_ \/ _).

(* or_intror: forall [A B : Prop], B -> A \/ B *)
(* or_introl: forall [A B : Prop], A -> A \/ B *)

Search (_ \/ _).

(* or_ind: forall [A B P : Prop], (A -> P) -> (B -> P) -> A \/ B -> P *)

Definition or_comm(A B: Prop): A \/ B -> B \/ A :=
  fun either =>
    or_ind
      (fun left => or_intror left)
      (fun right => or_introl right)
      either.
```

### `destruct either as [left | right]`

First, in the function body of the left case,
then, in the function body of the right case.

### `assumption`

Like `exact v`, but let Coq decides which variable to use.

### 3.3.1 Examples using `apply`

The `apply` tactic allows us to work top-down,
instead of bottom-up during normal function application.

- Arguments is found after the function during proving (like typed hole).

```coq
Check le_n.
(* le_n: forall n : nat, n <= n *)

Check le_S.
(* le_S: forall n m : nat, n <= m -> n <= S m *)

Lemma example4:
  3 <= 5.
Proof.
  apply le_S.
  apply le_S.
  apply le_n.
Qed.

Definition example4_fn: 3 <= 5 :=
  le_S 3 4 (le_S 3 3 (le_n 3)).
```

Transitivity theorem
for the order "less than or equal to"
on natural numbers.

```coq
Require Import Arith.

Check le_trans.
(* Lemma le_trans : forall n m p : nat, n <= m -> m <= p -> n <= p. *)

Lemma example5_1:
  1 <= 2 -> 2 <= 3 -> 1 <= 3.
Proof.
  apply le_trans.
Qed.

Definition example5_1_fn:
  1 <= 2 -> 2 <= 3 -> 1 <= 3 :=
  le_trans 1 2 3.

Lemma example5:
  forall x y,
    x <= 10 -> 10 <= y -> x <= y.
Proof.
  intros x y.
  intros x10 y10.
  apply (le_trans x 10).
  (* NOTE Using named implicit argument (like the following)
     will make it part of the public API. *)
  (* apply le_trans with (m := 10). *)
  exact x10.
  exact y10.
Qed.

Definition example5_fn x y:
  x <= 10 -> 10 <= y -> x <= y :=
  le_trans x 10 y.
```

### 3.3.2 Examples using `rewrite`

```coq
Require Import Arith.

Lemma example6:
  forall x y,
    (x + y) * (x + y) = (x * x) + (2 * x * y) + (y * y).
Proof.
  intros x y.
  SearchRewrite (_ * (_ + _)).
  rewrite mult_plus_distr_l.
  SearchRewrite ((_ + _) * _).
  rewrite mult_plus_distr_r.
  rewrite mult_plus_distr_r.
  SearchRewrite (_ + (_ + _)).
  rewrite plus_assoc.
  rewrite <- plus_assoc with (n := x * x).
  SearchPattern (?x *?y =?y *?x).
  rewrite mult_comm with (n:= y) (m:=x).
  SearchRewrite (S _ * _).
  pattern (x * y) at 1.
  rewrite <- mult_1_l.
  rewrite <- mult_succ_l.
  SearchRewrite (_ * (_ * _)).
  rewrite mult_assoc.
  reflexivity.
Qed.
```

Using `Ring`.

```coq
Require Import Ring.
Require Import Arith.

Lemma example6:
  forall x y,
    (x + y) * (x + y) = x*x + 2*x*y + y*y.
Proof.
  intros.
  ring.
Qed.
```

### 3.3.3 Examples using `unfold`

To _unfold_ definitions.

```coq
Lemma pred_S_eq:
  forall x y,
    x = S y -> Nat.pred x = y.
Proof.
  intros x y.
  intros q.
  unfold Nat.pred.
  rewrite q.
  reflexivity.
Qed.
```

## 3.4 More advanced tactics

| tactic           | usage                   |
| ---------------- | ----------------------- |
| intuition, tauto | propositional logic     |
| firstorder       | first-order logic       |
| ring             | equality of polynomials |
| omega            | linear inequations      |

# 4 Proving properties of programs on numbers

## 4.1 A proof by induction

## 4.2 Stronger statements in induction

# 5 Reasoning on conditional statements

# 6 Proving properties of programs on lists

# 7 Defining new datatypes

## 7.1 Defining inductive types

## 7.2 Pattern matching

## 7.3 Recursive function definition

## 7.4 Proof by cases

## 7.5 Proof by induction

## 7.6 An example using injection

# 8 Numbers in the Coq system

# 9 Inductive properties

## 9.1 Defining an inductive predicate

## 9.2 Proofs by induction on inductive predicates

## 9.3 The inversion tactic

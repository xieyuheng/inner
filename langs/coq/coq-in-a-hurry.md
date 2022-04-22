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

- Xie: Unlike function which takes many premises and return a conclusion,
  a tactic take a _goal_ and returns more goals,
  where a goal has two parts, a context and a conclusion.

- Xie: How to understand the space of goals?

  - Is it different from the space of types?

  - Can we express a goal as a type and view a tactic as a function?

    We will need record types, because in the context,
    the order of hypotheses does not matter.

```coq
Lemma and_comm:
  forall A B: Prop,
    A /\ B -> B /\ A.
Proof.
  intros a b.
  intros both.
  split.
  destruct both as [fst snd].
  exact snd.
  destruct both as [fst snd].
  exact fst.
Qed.
```

We should learn to proof without tactics first.

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

**Tactic: `intros x y`**

```
fun x y => ...
```

- Introduce hypotheses into context by lambda.

**Tactic: `destruct both as [fst snd]`**

```
let fst = proj1 both
let snd = proj2 both
...
```

**Tactic: `exact h`**

- Lookup the type of `h` in the context.

**Tactic: `intuition`**

- Let Coq search the proof.

Another example.

```coq
Lemma or_comm:
  forall A B: Prop,
    A \/ B -> B \/ A.
Proof.
  intros A B.
  intros either.
  destruct either as [x | y].
  right.
  exact x.
  left.
  exact y.
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

**Tactic: `destruct either as [left | right]`**

First, in the function body of the left case,
then, in the function body of the right case.

**Tactic: `assumption`**

Like `exact h`, but let Coq decides which variable to use.

**Tactic: `apply`**

- TODO 用来处理 context 中的 universal-quantification with implication:

```coq
Theorem kkk
        forall x1 x2 x3 ,
          (P1 x1 x2 x3 ->
           (P2 x1 x2 x3 ->
            (P3 x1 x2 x3 ->
             (P4 x1 x2 x3 -> C x1 x2 x3)))).
```

apply try to match
<premise> -> <conclusion>
with the pattern provided by a Theorem
and try to form new goal accordingly:

```coq
Theorem lll C a1 a2 a3.
Proof.
  apply kkk.
  (* replaces the current goal with 4 goals *)
  (* whose statements are: *)
  (* A1 a1 a2 a3.  *)
  (* A2 a1 a2 a3.  *)
  (* A3 a1 a2 a3.  *)
  (* A4 a1 a2 a3.  *)
...
```

其实证明定理就像是在有向图中行走
看能走到哪里就算证明到了哪里
而当我证明了一个带有全称量词的定理的时候
就相当于我熟悉了这个有向图中的某种模式的道路
当我再次遇到这个种道路的时候
我就可以直接到达道路的那一头
而不用再一步一步地走了

```coq
Check le_n.
(* le_n: forall n : nat, n <= n *)
Check le_S.
(* le_S: forall n m : nat, n <= m -> n <= S m *)
Lemma example4 : 3 <= 5.
Proof.

  (* 下面apply处理context中的 *)
  (* universal-quantification with implication *)
  apply le_S.
  apply le_S.

  (* 下面apply处理context中的 *)
  (* universal-quantification without implication: *)
  (* 这时就有可能完成证明了 *)
  apply le_n.

Qed.
```

transitivity theorem for the order
``less than or equal to'' on natural numbers

```coq
Require Import Arith.

Check le_trans.
(* Lemma le_trans : forall n m p : nat, n <= m -> m <= p -> n <= p. *)

Lemma example5_1 :  1 <= 2 -> 2 <= 3 -> 1 <= 3.
Proof.
  apply le_trans.
Qed.

Lemma example5 : forall x y, x <= 10 -> 10 <= y -> x <= y.
Proof.

  intros x y x10 y10.
  apply le_trans with (m := 10).
  (* 可以理解到如果没有with (m := 10)为什么就会失败 *)
  (* 因为apply想要利用定理le_trans给出sub-goal的时候 *)
  (* 会发现当把全称量词中的约束变元作为pattern-variable时 *)
  (* 有的pattern-variable(这里的m)没有绑定到任何值 *)

  (* 之后就简单了 *)

  (* 最精确的: *)
  (* exact x10. *)
  (* exact y10. *)

  (* 模糊一点 让coq帮忙查找: *)
  (* assumption. *)
  (* assumption. *)

  (* 最模糊的: *)
  intuition.
  intuition.

Qed.
```

#### rewrite

many theorems have a conclusion that is an equality
the most practical tactic to use these theorem is rewrite
即 rewrite 是用来给证明等式的
rewrite 所使用的定理(rewrite-rule)
pattern-matching 被证定理的等号左边
然后将被证的等式恒等变形为另一个等式

```coq
Require Import Arith.

Lemma example6 : forall x y, (x + y) * (x + y) = x*x + 2*x*y + y*y.
Proof.

  intros x y.
  (* 约束变元的类型被推导出来了 *)
  (* 下面查一下(左)分配律的重写规则 *)
  SearchRewrite (_ * (_ + _)).
  rewrite mult_plus_distr_l.
  (* 下面查一下(右)分配律的重写规则 *)
  SearchRewrite ((_ + _) * _).

  (* rewrite mult_plus_distr_r. *)

  (* 可以用with来指定一个上面所查找到的的定理中的 *)
  (* 约束变元所应该在模式匹配中被绑定到的项 *)
  (* 否则coq会选择前面的一个 *)
  rewrite mult_plus_distr_r with (p:=y).
  rewrite mult_plus_distr_r.

  (* intuition在这里不能用 *)
  (* 看来它是专门处理一阶逻辑中的显然步骤的 *)

  (* 那么继续找加法结合律 *)
  SearchRewrite (_ + (_ + _)).
  (* plus_assoc: forall n m p : nat, n + (m + p) = n + m + p *)
  rewrite plus_assoc.

  (* 下面反着利用rewrite-rule *)
  (* 而匹配的还是被证明项的等号左边 *)
  rewrite <- plus_assoc with (n := x * x).

  (* next : commutativity for multiplication *)
  SearchPattern (?x *?y =?y *?x).
  (* mult_comm: forall n m : nat, n * m = m * n *)
  rewrite mult_comm with (n:= y) (m:=x).

  (* 找定理的时候要小部分小部分地找 *)
  (* 汉语形成副词的方式是通过重复:小部分小部分地 *)
  SearchRewrite ((S _) * _).
  SearchRewrite (S _ * _).
  (* mult_succ_l: forall n m : nat, S n * m = n * m + m *)
  (* mult_1_l: forall n : nat, 1 * n = n *)


  (* using a tactic called pattern *)
  (* to limit the place where rewriting occurs *)
  pattern (x * y) at 1.
  rewrite <- mult_1_l.
  rewrite <- mult_succ_l.

  (* 然后是乘法结合律 *)
  SearchRewrite (_ * (_ * _)).
  rewrite mult_assoc.

  reflexivity.
  (* reflexivity用来引入基本等词 *)

Qed.
```

上面的证法很笨很笨
对等价关系所形成的表达式之间的无向路
应该有更好的处理方式

确实 coq 提供了 ring 这个函数

> < 但是如何使用呢?
> 下面的用法是不行的

```coq
Lemma example6 : forall x y, (x + y) * (x + y) = x*x + 2*x*y + y*y.
Require Import Ring.
Proof.
  intros.
  ring.
Qed.
```

```coq
Require Import Omega.

Lemma omega_example :
  forall f x y, 0 < x ->
           0 < f x ->
           3 * f x <= 2 * y ->
           f x <= y.
Proof.
  intros.
  omega.
Qed.
```

# >< proving properties of programs on numbers

# >< proving properties of programs on lists

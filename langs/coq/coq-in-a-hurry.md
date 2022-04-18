---
title: Coq in a Hurry
author: Yves Bertot
year: 2015
---

# Check

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

# fun

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

# let

```coq
Check
  let f := fun x => (x * 3, x)
  in f 3.
```

# Locate

To find the function hidden behind a notation.

```coq
Locate "_ <= _".
Locate "_ \/ _".

Check and.
Check (and True False).
Check (and True).
Locate and.

Locate plus.
Locate sum.
Locate nat.
Locate "_ + _".
```

# command : Eval

some symbolic computation is performed on this formula
and there are several strategies to perform this symbolic computation
one strategy is called ``compute''

```coq
Eval compute in
    let f := fun x => (x * 3, x)
    in f 3.

Check fun x1:nat => fun x2:nat => (plus x1 x2).

Eval compute in
    let f := fun x1:nat => fun x2:nat => (plus x1 x2)
    in f 4 3.
```

# command : Definition

with ``Definition'' one could write programs
programs are usually represented by functions

simple programs can be executed in the coq system itself
more complex coq programs can be transformed into programs
in more conventional languages and executed outside coq

```coq
Definition example1 (x : nat) := x*x+2*x+1.
(* one can't use Definition
   to define something more than once *)

Check example1.
Eval compute in
    example1 100.

(* to see the free a binding of an name *)
Reset example1.

(* explicit version: *)
Definition example1 := fun x : nat => x*x+2*x+1.

(* to see the definition of an name *)
Print example1.
```

# type : bool

observing the difference between bool and Prop

```coq
Require Import Bool.

Eval compute in
    if true
    then 3
    else 5.

Check true.

Check True.
```

# command : Search and SearchAbout

knowing what functions are provided by a datatype

```coq
Search bool.
SearchAbout bool.

Check false : bool.
Check true : bool.
Check xorb : bool -> bool -> bool.
Check orb : bool -> bool -> bool.
Check negb : bool -> bool.
Check implb : bool -> bool -> bool.
Check andb : bool -> bool -> bool.


Search Prop.
SearchAbout Prop.
```

# type : nat

```coq
Require Import Arith.

Eval compute in
    S (S (S 1)).

Definition is_zero :=
  (fun n:nat =>
     match n with
       | 0 => true
       | S p => false
     end).

Eval compute in
    is_zero 1.

Eval compute in
    is_zero 0.

Definition nat_sub1 :=
  fun n:nat =>
    (match n with
       | 0 => 0
       | S p => p
     end).

Eval compute in
    nat_sub1 1.

Eval compute in
    nat_sub1 0.

Print pred.
```

# command : Fixpoint

is it means that the recursion is implemented by ``Y''???

> < but way one can't use `Fixpoint'' as `Definition''???

```coq
Fixpoint sum_n n :=
  match n with
    | 0 => 0
    | S p => p + sum_n p
  end.

Fixpoint sum_n2 n s :=
  match n with
    | 0 => s
    | S p => sum_n2 p (p + s)
  end.

Eval compute in
    sum_n2 100 0.

Eval compute in
    sum_n2 100 0.


Fixpoint evenb n :=
  match n with
    | 0 => true
    | 1 => false
    | S (S p) => evenb p
  end.

Eval compute in
    evenb 100.

Eval compute in
    evenb 101.
```

structural-recursion-constraint:
the recursive call can only be made
on a subterm of the initial argument
it can't ensure that every computation terminates at all
so this kind of funking constraint is always bad for user

# type : list

list of data must be of the same type

```coq
Require Import List.


Check 1::2::3::nil.

Check nil.
(* have no type *)

Check (nil : list nat).

Eval compute in
    map (fun x => x + 3) (1::3::2::nil).

Eval compute in
    map S (1::22::3::nil).

Eval compute in
    let l := (1::2::3::nil)
    in l ++ map (fun x => x + 3) l.


Fixpoint evenb n :=
  match n with
    | 0 => true
    | 1 => false
    | S (S p) => evenb p
  end.

Definition head_evb :=
  fun l =>
    match l with
      | nil => false
      | a::tl => evenb a
    end.

Eval compute in
    head_evb (2::1::nil).

Fixpoint sum_list l :=
  match l with
    | nil => 0
    | n::tl => n + sum_list tl
  end.

Eval compute in
    sum_list (2::1::nil).

Fixpoint 大于等于 n1 n2 :=
  match n1 with
    | 0 => match n2 with
        | 0 => true
        | S k2 => false
      end
    | S k1 =>
      match n2 with
        | 0 => true
        | S k2 => 大于等于 k1 k2
      end
    end.


Fixpoint insert n l :=
  match l with
    | nil => n::nil
    | a::tl => if 大于等于 a n
               then n::l
               else a::insert n tl
  end.
Fixpoint sort l :=
  match l with
    | nil => nil
    | a::tl => insert a (sort tl)
  end.

Eval compute in
    sort (1::4::3::22::5::16::7::nil).

Fixpoint is_sorted l :=
  match l with
    | nil => true
    | a::nil => true
    | a1::a2::nil => 大于等于 a2 a1
    | a1::a2::tail => if 大于等于 a2 a1
                      then
                        match l with
                          | nil => true
                          | a1::tail => is_sorted tail
                        end
                      else false
  end.
Eval compute in
    is_sorted (1::2::3::nil).
Eval compute in
    is_sorted (1::4::3::nil).
```

# propositions and proofs

the semantices of x:A

1. x is proof of logical formula A
2. x is of the type A

# command : Search and SearchPattern

to find already existing proofs of facts
its argument should always be an identifier

some axiom joint of the directed-graph

```coq
Search True.

(* Search le. *)

(* SearchPattern (_ + _ <= _ + _). *)

(* SearchRewrite (_ + (_ - _)). *)

SearchAbout True.
```

# command : Theorem and Lemma

## note

_curry–howard isomorphism_
_propositions-as-types_

这是通过语法的相似性而被发现的
当发现语法相似的时候就是应该融合形式语言的时候
尽管语义不同

Qed. quod erat demonstrandum
w.z.b.w. was zu beweisen war

A -> B == ¬A ∨ B

## tactics 是写在 Proof.于 Qed.之间的 context&conclusion-processing function

每个 tactics 只能处理某些特定 patten 的 context&conclusion

### goal == context&conclusion

so one can say ``goal-processing function''

it looks like:

```
<context>
=======================
<conclusion>
```

- > < 其中<context>是前面证明过的定理和局部的假设???

and initially it is:

```
<>
=======================
<statements>
```

就下面的在一般数学文本中出现的对推理规则的表达而言
Γ,x:σ ͱ M:τ
------------------- (->introduction)
Γ ͱ (λx.M):(σ->τ)
coq 中的双横线`=============''对应于这里的`ͱ''
而这里的单横线`-------------''对应于coq中的`tactics''
可以看出在一般数学文本中
语义上`ͱ''与`-------------''是相似的
只不过它们的层次不同

### 被处理的 context&conclusion 作为数据结构是什么样的?

即是问 context&conclusion 和 context&conclusion 之间的关系是什么
这些关系是如何实现的
有向图吗???
其实就是被隐蔽起来的有向图处理
onescontext&conclusion 是有向图的节点
tactics 用来指明在回溯过程中下一步往那个方向走

### tactics for the basic logical connectives

#### intros h1 h2 ...

introduce
用来处理 conclusion 中的

1. 全称量词(universal quantification)
   - 量词后面的是约束变元 所以可以随便用什么名字
2. 蕴含式的假设(implication)
3. 否定式
   把表达式引入 context 的同时消减了 conclusion 中的东西
   即从 conclusion 中提取出可以在局部假设成立得到假设
   intros 后面跟标示符用来给提取出来的局部成立的假设命名

```coq
Lemma example2 : forall a b : Prop, a /\ b -> b /\ a.
Proof.
  intros a b.
  intros H.
  split.
  destruct H as [H1 H2].
  exact H2.
  intuition.
  (* intuition as: *)
  (* destruct H as [H1 H2]. *)
  (* exact H1. *)
Qed.
```

#### destruct H as [H1 H2]

用来处理 context 中的 b /\ a
这将会在一个 goal 中把 H 分开为两句

#### destruct H as [H1 | H2]

用来处理 context 中的 b \/ a 中的
这将会把一个 goal 分开为两个 goal
即是分情况证明

```coq
Lemma example3 : forall A B, A \/ B -> B \/ A.
Proof.
  intros A B H.
  destruct H as [H1 | H2].
  right.
  assumption.
  left.
  assumption.
Qed.
```

#### exact H

simply expresses that we want to prove
a statement that is present in the context

#### assumption

to look for one hypothesis whose
statement is the same as the conclusion

#### intuition

automatic tactic
让 coq 帮忙来完成一些步骤

#### apply

用来处理 context 中的
universal-quantification with implication:

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

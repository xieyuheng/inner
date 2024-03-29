---
title: Logical Foundations
---

# bool

```coq
Inductive bool: Type :=
  | true: bool
  | false: bool.

Definition negb(b: bool): bool :=
  match b with
  | true  => false
  | false => true
  end.

Definition andb(b1 b2: bool): bool :=
  match b1 with
  | true => b2
  | false => false
  end.

Definition orb(b1 b2: bool): bool :=
  match b1 with
  | true => true
  | false => b2
  end.
```

# nat

```coq
Require Export bool.

Inductive nat : Type
  :=
  | O : nat
  | S : nat -> nat.


Fixpoint beq_nat
         (n : nat)
         (m : nat) : bool
  :=
    match n, m with
    | O   , O    => true
    | O   , S m' => false
    | S n', O    => false
    | S n', S m' => beq_nat n' m'
    end.

Fixpoint ble_nat
         (n : nat)
         (m : nat) : bool
  :=
    match n, m with
    | O   , _    => true
    | S n', O    => false
    | S n', S m' => ble_nat n' m'
    end.


Fixpoint evenb
         (n : nat) : bool
  :=
    match n with
    | O           =>  true
    | (S O)       =>  false
    | (S (S n'))  =>  (evenb n')
    end.


Definition oddb
           (n : nat) : bool
  :=
    (negb (evenb n)).


Definition pred
           (n : nat) : nat
  :=
    match n with
    | O  =>  O
    | (S n')  =>  n'
    end.


Fixpoint plus
         (n : nat)
         (m : nat) : nat
  :=
    match n with
    | O       =>  m
    | (S n')  =>  (S (plus n' m))
    end.


Fixpoint mult
         (n : nat)
         (m : nat) : nat
  :=
    match n with
    | O       =>  O
    | (S n')  =>  (plus m (mult n' m))
    end.


Fixpoint minus
         (n : nat)
         (m : nat) : nat
  :=
    match n, m with
    | O , _   =>  O
    | _ , O   =>  n
    | (S n'), (S m')  =>  (minus n' m')
    end.


Fixpoint exp
         (base  : nat)
         (power : nat) : nat
  :=
    match power with
    | O      =>  (S O)
    | (S p)  =>  (mult base (exp base p))
    end.


Fixpoint factorial
         (n : nat) : nat
  :=
    match n with
    | O  =>  O
    | (S O)  =>  (S O)
    | (S n')  =>  (mult n (factorial n'))
    end.
```

# simpl

如上對 nat 的基本函數 的遞歸定義
其定義中 並沒有展示出 對稱性 和 結合性
這些運算運算律是需要在之後證明的

何以至此
能在定義中就展示其 對稱性 與 結合性 邪

當使用自然數的不同的編碼方式時
情況會不同

```coq
Require Export nat.


Theorem plus_O_n :
  forall n : nat,
    (plus O n) = n.
Proof.
  intros n.  destruct n as [ | n' ].
  (* n = O *)
  simpl.  reflexivity.
  (* n = S n' *)
  simpl.
  (* 從下面的證明可以看出
     在用基本等詞判斷表達式是否相等的時候
     表達式中是可以存在約束變元的
     注意每個約束變元也是有類型的 *)
  reflexivity.
Qed.


Theorem plus_n_O :
  forall n : nat,
    (plus n O) = n.
Proof.
  intros n.  simpl.
  (* Doesn't do anything!
     so we can not just reflexivity *)
  destruct n as [ | n' ].
  simpl.
  reflexivity.
  (* 發現如果按上面的方式定義加法 就沒法證明這個定理 *)
Abort.


(* 注意 (plus 1 n) 與 (S n) 的語義差別 *)
Theorem plus_1_l :
  forall n : nat,
    (plus (S O) n) = (S n).
Proof.
  intros n.  simpl.  reflexivity.
Qed.


Theorem mult_O_l :
  forall n : nat,
    (mult O n) = O.
Proof.
  intros n.  simpl.  reflexivity.
Qed.
```

# rewrite

```coq
Require Export simpl.


Theorem plus_id_example :
  forall n m : nat,
    n = m
    -> (plus n n) = (plus m m).
Proof.
  intros n.
  intros m.

  (* move hypothesis into context *)
  intros h.

  (* rewrite the goal using hypothesis *)
  (* apply the rewrite from left to right *)
  (* from left of  n = m  to right of it *)
  rewrite -> h.
  (* from right to left is also ok *)
  (* from right of  n = m  to left of it *)
  rewrite <- h.

  reflexivity.
Qed.


Theorem plus_id_exercise :
  forall n m o : nat,
    n = m
    -> m = o
    -> (plus n m) = (plus m o).
Proof.
  intros n m o.
  intros h1.
  intros h2.
  rewrite -> h1.
  rewrite <- h2.
  reflexivity.
Qed.


Theorem mult_O_plus :
  forall n m : nat,
    (mult (plus O n) m) = (mult n m).
Proof.
  intros n m.
  rewrite -> plus_O_n.
  reflexivity.
Qed.


Theorem mult_S_1 :
  forall n m : nat,
    m = (S n)
    -> (mult m (plus (S O) n)) = (mult m m).
Proof.
  intros n m.
  intros h.
  (* 類型就是命題
     h : m = S n
     即 h 屬 m = S n 類型
     所以 intros 對上面看似不同的對象的處理方式是一致的 *)
  rewrite -> plus_1_l.
  rewrite <- h.
  reflexivity.
Qed.
```

# destruct

coq 的設計失誤
當使用 destruct 而形成了 subgoal
並沒有對 subgoal 的命名機制

```coq
Require Export rewrite.


Theorem plus_1_neq_O :
  forall n : nat,
    (beq_nat (plus n (S O)) O) = false.
Proof.
  intros n.
  simpl.
  (* does nothing!
     the first argument to [+]
     is the unknown number [n]
     and the argument to [beq_nat]
     is the compound expression [n + 1]
     neither can be simplified
     這表明了
     simpl 是跟定義函數的時候
     匹配參數時的順序有關的
     simpl 每次之能處理一個參數 *)
  destruct n as [ | n'].
  (* The [destruct] tactic
     can be used with any inductively defined datatype

     [as] is used to
     bind subcases of the [destruct]
     to variables  *)
  simpl.
  reflexivity.
  simpl.
  reflexivity.
Qed.

(* negation is its own inverse *)
Theorem negb_involutive :
  forall b : bool,
    (negb (negb b)) = b.
Proof.
  intros b.
  destruct b.
  reflexivity.
  reflexivity.
Qed.

Theorem zero_nbeq_plus_1 :
  forall n : nat,
    (beq_nat O (plus n (S O))) = false.
Proof.
  intros n.  destruct n as [ | n'].
  simpl.  reflexivity.
  simpl.  reflexivity.
Qed.


Theorem andb_eq_orb :
  forall (b c : bool),
    (andb b c = orb b c)
    -> b = c.
Proof.
  intros b c.
  destruct b .
  destruct c.
  simpl.
  intros h.
  reflexivity.
  simpl.
  intros h.
  (* 每個表達式之間的等式都可以被用來作 rewrite
     即使是看似錯誤的 true = false *)
  rewrite <- h.
  reflexivity.
  destruct c.
  simpl .
  intros h.
  rewrite <- h.
  reflexivity.
  simpl.
  intros h.
  reflexivity.
Qed.
```

# induction

其特點是 需要證明相等的兩個表達式中
有約束變元是函數作用的參數

1. 如果 函數的定義中 沒有匹配這個位置的參數
   那麼 可能就不需要歸納法

2. 如果 函數的定義中 匹配了這個位置的參數
   那麼 可能就需要歸納法
   因爲
   如果 函數根本就沒有匹配某個約束變元
   那麼
   這個約束變元在表達式改寫中
   被改寫的方式 就是平凡的
   注意
   形式上 與 destruct 相比
   induction 向環境中多引入了一個條件

```coq
Require Export destruct.


Theorem plus_n_O :
  forall n : nat,
    (plus n O) = n.
Proof.
  intros n. induction n as [ | n' ].
  (* n = 0 *)
  reflexivity.
  (* n = S n'. *)
  simpl. rewrite -> IHn'. reflexivity.
Qed.


Theorem minus_diag :
  forall n : nat,
    (minus n n) = O.
Proof.
  intros n. induction n as [ | n'].
  (* n = 0 *)
  simpl. reflexivity.
  (* n = S n' *)
  simpl. rewrite -> IHn'. reflexivity.
Qed.


Theorem mult_O_r :
  forall n : nat,
    (mult n O) = O.
Proof.
  intros n. induction n as [ | n' ].
  (* n = O *)
  simpl.  reflexivity.
  (* n = S n' *)
  simpl. rewrite -> IHn'.  reflexivity.
Qed.


Theorem plus_n_Sm :
  forall n m : nat,
    (S (plus n m)) = (plus n (S m)).
Proof.
  intros n m.  induction n as  [ | n' ].
  (* n = O *)
  simpl.  reflexivity.
  (* n = S n' *)
  simpl.  rewrite -> IHn'.  reflexivity.
Qed.


Theorem plus_comm :
  forall n m : nat,
    (plus n m) = (plus m n).
Proof.
  intros n m.
  induction n as [ | n' ].
  (* n = O *)
  rewrite -> plus_O_n.
  rewrite -> plus_n_O. reflexivity.
  (* n = S n' *)
  simpl.  rewrite -> IHn'.
  rewrite -> plus_n_Sm.
  reflexivity.
Qed.


Theorem plus_assoc :
  forall n m p : nat,
    (plus n (plus m p)) = (plus (plus n m) p).
Proof.
  intros n m p.  induction n as [ | n' ].
  (* n = O *)
  simpl.  reflexivity.
  (* n = S n' *)
  simpl.  rewrite -> IHn'.
  reflexivity.
Qed.


Fixpoint double
         (n : nat) : nat
  :=
    match n with
    | O => O
    | S n' => S (S (double n'))
    end.


Lemma double_plus :
  forall n : nat,
    (double n) = (plus n n) .
Proof.
  intros n.  induction n as [ | n' ].
  (* n = O *)
  simpl.  reflexivity.
  (* n = S n' *)
  simpl.  rewrite -> IHn'.
  rewrite -> plus_n_Sm. reflexivity.
Qed.
```

# assert

coq 的設計失誤
沒有方便的語法
來指明某次 rewrite 作用的位置

在 assert 中出現的局部變元
並不是約束變元
而是在局部環境中被引入了的
使用它們時
所能比配到的子表達式是更具體的
這樣就能用通過使用 assert
來補救上面的設計失誤

可以發現
對於有結合律和交換律的二元函數而言
用前綴表達式或者後綴表達式時
我們就難以觀察到
應該使用那些運算律來對表達式進行變換
可能因爲

1. 我們不熟悉這些運算律
   在非中綴表達式中的樣子

2. 中綴表達式對於體現這些運算律而言
   是本質重要的

```coq
Require Export induction.

Theorem mult_O_plus' :
  forall n m : nat,
    (mult (plus O n) m) = (mult n m).
Proof.
  intros n m.

  assert (h: (plus O n) = n).
  reflexivity.

  rewrite -> h.  reflexivity.
Qed.



Theorem plus_rearrange :
  forall n m p q : nat,
    (plus (plus n m) (plus p q))
    = (plus (plus m n) (plus p q)).
Proof.
  intros n m p q.

  rewrite -> plus_comm.
  (* doesn't work
     for coq rewrote the wrong plus *)
  rewrite -> plus_comm.

  assert (h: (plus n m) = (plus m n)).
  (* lemma *)
  rewrite -> plus_comm. reflexivity.

  rewrite -> h. reflexivity.
Qed.



Theorem plus_swap :
  forall n m p : nat,
    (plus n (plus m p))
    = (plus m (plus n p)).
Proof.
  intros n m p.

  assert (l1 : (plus n (plus m p)) = (plus (plus n m) p)).
  rewrite -> plus_assoc. reflexivity.

  assert (l2 : (plus m (plus n p)) = (plus (plus m n) p)).
  rewrite -> plus_assoc. reflexivity.

  assert (l3 : (plus m n) =  (plus n m)).
  rewrite -> plus_comm. reflexivity.

  rewrite -> l1.
  rewrite -> l2.
  rewrite -> l3.
  reflexivity.
Qed.



Theorem left_mult_step_distribution :
  forall n m : nat,
    (plus m (mult m n)) = (mult m (S n)).
Proof.
  intros n m.

  induction m as [ | m' ].

  (* m = O *)
  simpl.
  reflexivity.

  (* m = S m' *)
  simpl.
  rewrite ->  plus_swap.
  rewrite ->  IHm'.
  reflexivity.
Qed.



Theorem mult_comm :
  forall n m : nat,
    (mult n m) = (mult m n).
Proof.
  intros n m.

  induction n as [ | n' ].
  (* n = O *)
  simpl.  rewrite -> mult_O_r.
  reflexivity.

  (* n = S n' *)
  simpl.
  rewrite -> IHn'.
  rewrite -> left_mult_step_distribution.
  reflexivity.
Qed.



Theorem evenb_n__oddb_Sn :
  forall n : nat,
    (evenb n) = (negb (evenb (S n))).
Proof.
  intros n.

  induction n as [ | n' ].

  (* n = O *)
  simpl. reflexivity.

  (* n = S n' *)
  simpl.
Abort.



Theorem ble_nat_refl :
  forall n : nat,
    true = (ble_nat n n).
Proof.
  intros n.
  induction n as [ | n' ].
  (* n = true *)
  simpl. reflexivity.
  (* n = S n' *)
  simpl.  rewrite <- IHn'. reflexivity.
Qed.


Theorem zero_nbeq_S :
  forall n : nat,
    (beq_nat O (S n)) = false.
Proof.
  intros n.
  simpl. reflexivity.
Qed.


Theorem andb_false_r :
  forall b : bool,
    (andb b false) = false.
Proof.
  intros b.
  destruct b as [ | ].
  (* b = true *)
  simpl. reflexivity.
  (* b = false *)
  simpl. reflexivity.
Qed.

Theorem plus_ble_compat_l :
  forall n m p : nat,
    (ble_nat n m) = true
    -> (ble_nat (plus p n) (plus p m)) = true.
Proof.
  intros n m p.
  intros h.
  induction p as [ | p'].
  (* p = O *)
  simpl. rewrite -> h. reflexivity.
  (* p = S p' *)
  simpl. rewrite -> IHp'. reflexivity.
Qed.


Theorem S_nbeq_0 :
  forall n : nat,
    (beq_nat (S n) O) = false.
Proof.
  intros n.
  simpl. reflexivity.
Qed.


Theorem mult_1_l :
  forall n : nat,
    (mult (S O) n) = n.
Proof.
  intros n.
  simpl. rewrite -> plus_n_O. reflexivity.
Qed.


Theorem all3_spec :
  forall b c : bool,
    (orb
       (andb b c)
       (orb (negb b)
            (negb c)))
    = true.
Proof.
  intros b c.
  destruct b as [ | ].
  destruct c as [ | ].
  (* b = true *)
  (* c = true *)
  simpl. reflexivity.
  (* c = false *)
  simpl. reflexivity.
  (* b = false *)
  simpl. reflexivity.
Qed.


Theorem mult_plus_distr_r :
  forall n m p : nat,
    (mult (plus n m) p)
    = (plus (mult n p) (mult m p)).
Proof.
  intros n m p.
  induction n as [ | n' ].
  (* n = O *)
  simpl. reflexivity.
  (* n = S n' *)
  simpl.
  rewrite -> IHn'.
  rewrite -> plus_assoc.
  reflexivity.
Qed.


Theorem mult_assoc :
  forall n m p : nat,
    (mult n (mult m p))
    = (mult (mult n m) p).
Proof.
  intros n m p.
  induction n as [ | n' ].
  (* n = O *)
  simpl. reflexivity.
  (* n = S n' *)
  simpl.
  rewrite -> mult_plus_distr_r.
  rewrite -> IHn'.
  reflexivity.
Qed.


Theorem beq_nat_refl :
  forall n : nat,
    true = (beq_nat n n).
Proof.
  intros n.
  induction n as [ | n' ].
  (* n = O *)
  simpl. reflexivity.
  (* n = S n' *)
  simpl. rewrite -> IHn'. reflexivity.
Qed.



Theorem plus_swap' :
  forall n m p : nat,
    (plus n (plus m p))
    = (plus m (plus n p)).
Proof.
  intros n m p.
  rewrite -> plus_assoc.
  rewrite -> plus_assoc.
  replace (plus n m) with (plus m n).
  reflexivity.
  rewrite -> plus_comm.
  reflexivity.
Qed.
```

# bin

就是反過來的二進制編碼

| dec | bin | bin in coq |
|-----|-----|------------|
| 0   | 0   | Z          |
| 1   | 1   | i Z        |
| 2   | 10  | o i Z      |
| 3   | 11  | i i Z      |
| 4   | 100 | o o i Z    |
| 5   | 101 | i o i Z    |
| 6   | 110 | o i i Z    |
| 7   | 111 | i i i Z    |

```coq
Inductive bin : Type
  :=
  | Z : bin
  | o : bin -> bin
  | i : bin -> bin.
```

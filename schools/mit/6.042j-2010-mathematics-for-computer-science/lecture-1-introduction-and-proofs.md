---
title: lecture 1 -- introduction and proofs
video: "https://www.youtube.com/watch?v=L3LMbpZIKhQ&list=PLB7540DEDD482705B"
---

# My Motive

[2025-10-16] 刚刚在 x-lisp 实现了 digraph 数据结构，
来看这个 lecture 系列休息系一下。

# Lecture

- 开篇老师就问什么是证明？

  老师的定义是：A proof is a method for ascertaining the truth.

  其中 ascertain 的意思是：find (something) out for certain; make sure of.
  因此 ascertain 其实就是以另一种方式说 to prove。
  所以这种定义是不太理想的。

  我更认同 Bishop 的定义：A proof is any completely convincing argument.

  Bishop 的定义中强调了证明与人的关系，带有一定主观性。
  这种主观性来自数学基础的不同流派之间的争论，
  如果数学家在数学基础是什么这个问题上都不能达成共识，
  那么对证明的定义带有主观性就是不可避免的。

  经典数学中有很多证明是没法被经验所实证的，
  比如 Bishop 从构造主义的角度论证了「中值定理」是荒谬的，
  但是数学家们依然相信这些定理和证明。
  这种分歧使得我们不得不说，
  「证明是什么」这个问题，
  取决于你对数学基础的认识是什么。

  如此以来，就所能说服的人的多少而言，
  数学基础就有了一个序关系。
  Bishop 所相信的构造主义，
  相比古典数学是能说服更多数人的。

  这个序关系还能被加强。
  基础的构造主义是「能够构造出来，才能证明存在」，
  可以被加强为「存在构造算法，才能证明存在」，
  可以被加强为「存在高效的构造算法，才能证明存在」。

- 老师的目的是要介绍 first-order logic 中的概念。

  通过一些数论中的例子来介绍：

  ```scheme
  (forall ((n nat?))
    (prime? (add (square n) (add n 41))))
  ```

  在写 forall 的时候，老师说 natural number 是 "universe of discourse"，
  这其实是不对的，因为 universe of discourse 是不用在 forall 语法中指出的。

  universe of discourse 可以用 `in-universe` 语法给出：

  ```scheme
  (in-universe nat?
    (forall (n)
      (prime? (add (square n) (add n 41)))))
  ```

- 用类型论表示上面的命题是：

  ```scheme
  (forall ((n nat-t))
    (prime-t (add (square n) (add n 41))))
  ```

  但是我们会强调集合论，
  在古典数学基础的体系内，
  用谓词系统来描述命题。

  所以我们还是会写：

  ```scheme
  (forall ((n nat?))
    (prime? (add (square n) (add n 41))))
  ```

  与类型论不同，
  我们没法用形式语言编写证明，
  并且由机器检查证明。
  采用 `forall` 和 `exists` 等形式语法，
  只是为了帮助我们清晰表达思想。

  可能 "the little prover" 就是在这种古典框架内做证明的，
  因此是非常值得学习的。

- 当然，上面这个命题是错误的，n 为 41 时候就是反例。

- 老师又给了几个例子，
  都是丢番图方程的可解性所形成的命题。

  最后一个例子是有实用价值的，
  因为和椭圆曲线和密码学有关。

  TODO

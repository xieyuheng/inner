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

  这种定义不太理想。

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

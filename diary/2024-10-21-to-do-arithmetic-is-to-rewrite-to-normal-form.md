---
title: to do arithmetic is to rewrite to normal form
date: 2024-10-21
---

看了 Paul 的 "Arithmetic" 这本书，
很明显地认识到 arithmetic 就是在于 rewrite to normal form。

罗马计算版就是 multiset rewriting。

进位制下的 7 + 8 = 15 也是 rewrite 到每一位所能容下的符号，
也是 multiset rewriting。

Paul 的 "Measurement" 这本书中，
解代数方程 d*(d-1)=1，的过程，
也是 rewrite to normal form 的过程。
但是这里涉及的可能是 constraint processing
中的 hypergraph rewriting 了。

读 "Measurement" 知道 measurement 在于比较，
而 Arithmetic 也是为了比较，
rewrite to normal form 的目的就是去比较。

大多数计算模型，
比如 lambda calculus，
都是用 rewrite to normal form 来定义的。
难怪计算机在开始的时候，是用来做 arithmetic 中的计算的。
难怪计算机叫「计算」机。

# proof theory

与这种 "rewrite to normal form" 的结构形成对比的，
就是 proof theory 中搜索的感觉，
给定目标类型，然后构造满足目标类型的证明，
像是搜索而不是计算。

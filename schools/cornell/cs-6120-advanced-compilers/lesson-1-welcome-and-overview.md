---
title: lesson 1 -- welcome and overview
source: "https://www.cs.cornell.edu/courses/cs6120/2020fa/lesson/1/"
---

[2025-10-04] 我第一次听说 Proebsting’s Law，
也就是说编译器优化对软件运行速度的提升，
远远小于硬件对软件运行速度的提升。

> **Proebsting's Law: Compiler Advances Double Computing Power Every
> 18 Years**
>
> I claim the following simple experiment supports this depressing
> claim. Run your favorite set of benchmarks with your favorite
> state-of-the-art optimizing compiler. Run the benchmarks both with
> and without optimizations enabled. The ratio of of those numbers
> represents the entirety of the contribution of compiler
> optimizations to speeding up those benchmarks. Let's assume that
> this ratio is about 4X for typical real-world applications, and
> let's further assume that compiler optimization work has been going
> on for about 36 years. These assumptions lead to the conclusion that
> compiler optimization advances double computing power every 18
> years. QED.
>
> This means that while hardware computing horsepower increases at
> roughly 60%/year, compiler optimizations contribute only
> 4%. Basically, compiler optimization work makes only marginal
> contributions.
>
> Perhaps this means Programming Language Research should be
> concentrating on something other than optimizations. Perhaps
> programmer productivity is a more fruitful arena.

source: https://proebsting.cs.arizona.edu/law.html
info: https://oleksii.shmalko.com/20211028115609/

老师说，但是因为摩尔定律正在减慢，
并且有很多新的特殊硬件，
所以编译器写手还是有市场的。

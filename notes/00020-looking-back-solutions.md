---
title: Looking back solutions
date: 2021-07-03
keywords: [cicada, implicit arguments]
---

# 实现带有 implicit argument 的 lambda

相关的论文中提到，实现 implicit argument，需要 unification。
为什么需要 unification？

应该先假设没有任何难点和问题，尝试去实现 implicit argument，
当发现问题之后，尝试清晰地描述所遇到的问题，
然后问「为什么这个问题可以用 unification 来解决？」

我们的工作进行到这里，会产生疑惑，
我想，是因为在 Polya 所提出的解决问题的四个阶段中，
我们缺少了 look back 这个阶段。

(1) understand
(2) plan
(3) carry out
(4) look back

look back 在于，产正了某个解决方案之后，
将这个解决方案抽象出来，
去想它能如何适用于解决类似的问题。

不止是用 unification 实现 implicit argument 会有疑惑，
在考虑 bidirectional type checking 和 NbE 等技术的时候，
我们也缺少 look back。

好的 look back 应该可以使我们出离 PL 这个 problem domain 之外。

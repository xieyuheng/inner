---
title: tidy first?
subtitle: a personal exercise in empirical software design
author: kent beck
year: 2023
---

# 学习动机

[2024-12-03] 我正在写一个基于 x window system 的 pixel canvas 程序，
其中目前有 `canvas_t` 和 `canvas_window_t` 两个 class，
前者是不依赖 x window system 的，
后者包含 canvas，并且处理所有和 window 相关的逻辑。
但是这样导致了 API 使用起来不是很方便，
因此我想把两个 class 融合，也就是进一步增加 coupling。
但是这样做是否正确呢？这本书的理论可以给出答案。

# Foreword

前言的作者是 Larry Constantine，
coupling 和 cohesion 概念的提出者。

> In theory, there is no difference between theory and practice, while
> in practice there is.

> That core theory is simply this: that the complexity of computer
> code depends on how it is organized into parts, on how coupled those
> parts are with each other and on how cohesive the parts are in
> themselves.

> Coupling and cohesion are simply measures of the complexity of
> computer code, not from the perspective of the computers executing
> the programs but that of human beings trying to understand the
> code. To understand any program, whether to create it or to correct
> it or to change it, requires understanding the piece of code
> immediately in front of you as well as those other pieces to which
> it is connected, which it depends on or affects or is affected
> by. It is easier to understand the immediate piece of code if it all
> hangs together, if it makes sense as a whole, if it forms what
> cognitive psychologists call a gestalt. That’s cohesion. It is also
> easier to understand it in terms of its relationships with other
> pieces of code if these relationships are few and relatively weak or
> highly constrained. That’s coupling. Coupling and cohesion are
> really all about how your brain deals with complicated systems.

# Preface

TODO

# Part I. Tidyings
# 1. Guard Clauses
# 2. Dead Code
# 3. Normalize Symmetries
# 4. New Interface, Old Implementation
# 5. Reading Order
# 6. Cohesion Order
# 7. Move Declaration and Initialization Together
# 8. Explaining Variables
# 9. Explaining Constants
# 10. Explicit Parameters
# 11. Chunk Statements
# 12. Extract Helper
# 13. One Pile
# 14. Explaining Comments
# 15. Delete Redundant Comments
# Part II. Managing
# 16. Separate Tidying
# 17. Chaining
# 18. Batch Sizes
# 19. Rhythm
# 20. Getting Untangled
# 21. First, After, Later, Never
# Part III. Theory
# 22. Beneficially Relating Elements
# 23. Structure and Behavior
# 24. Economics: Time Value and Optionality
# 25. A Dollar Today > A Dollar Tomorrow
# 26. Options
# 27. Options Versus Cash Flows
# 28. Reversible Structure Changes
# 29. Coupling
# 30. Constantine’s Equivalence
# 31. Coupling Versus Decoupling
# 32. Cohesion
# 33. Conclusion
# Appendix: Annotated Reading List and References

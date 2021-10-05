---
title: The cost of composable little functions
---

# value-equal v.s. readback exp-alpha-equal

x y value-equal

x readback y readback exp-alpha-equal

# exp-alpha-equal v.s. pretty-print string-equal

x y exp-alpha-equal

x pretty-print y pretty-print string-equal

# the cost

For these equivalent functions, instead of computing a normal form or string,
we could have return a false as soon as we found the top of value is different.

We should better keep our implementation optimizable,
because these functions might be future bottle necks.

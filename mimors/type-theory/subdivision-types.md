如何在类型系统中表达细分？和 path type 所用的技术有关吗？

如果只考虑用哪些信息足以描述细分，那么对于细分的点，我们只需要描述它在哪条线段上，对于细分的线段，我们需要描述它在哪个实心多边形上，同时还要描述它的边界，对于更高阶的细分数据也是如此，只需要描述边界就可以。

由于细分是潜在的，是在需要的时候引入的，所以可能 scope 有关。

从波利亚解题的角度讲，我们已经有了 plan，难点在于如何 carry out，因为目前看来，通过实现语言来做实验的成本很高。

inet 和 linear type 可以简化这种困难吗？

在 the little typer 中 expression 就是 value，而在 inet 中，value 和构造 value 的 statement 是完全分离的，构造图的时候需要如此，构造高阶的 cell complex 时，可能也需要如此。
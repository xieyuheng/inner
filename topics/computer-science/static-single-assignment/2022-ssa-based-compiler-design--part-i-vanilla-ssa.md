---
title: SSA-based compiler design
subtitle: Part I Vanilla SSA
year: 2022
---

# 1 Introduction (Jeremy Singer)

> In computer programming, as in real life, names are useful handles
> for concrete entities. The key message of this book is that having
> _unique names_ for _distinct entities_ reduces uncertainty and
> imprecision.

比如在类型检查和推导的过程中，
一个表达式的诸多子表达式原本是匿名的，
但是我们需要给每个子表达式分配一个类型变量（unique name），
这样才能形成类型变量之间的方程组。

> This book is about the _Static Single Assignment form_ (SSA), which
> is a naming convention for storage locations (variables) in
> low-level representations of computer programs.

## 1.1 Definition of SSA

> The simplest, least constrained, definition of SSA can be given
> using the following informal prose:
>
> A program is defined to be in SSA form if each variable is a target
> of exactly one assignment statement in the program text.

> One important property that holds for all varieties of SSA,
> including the simplest definition above, is _referential
> transparency_: i.e., since there is only a single definition for
> each variable in the program text, a variable’s value is
> _independent of its position_ in the program.

> Programs written in pure functional languages are referentially
> transparent. Such referentially transparent programs are more
> amenable to formal methods and mathematical reasoning, since the
> meaning of an expression depends only on the meaning of its
> subexpressions and not on the order of evaluation or side effects of
> other expressions.

下面的代码不是引用透明的：

```
x = 1;
y = x + 1;
x = 2;
z = x + 1;
```

就算 assignment 不是对 lvalue 的赋值，
而是对 binding 的赋值（比如 scheme 的 let 语义），
上面的代码也不是引用透明的。

需要被转化为每个 assignment 都有 unique 名字，才能算做引用透明：

```
x.1 = 1;
y = x.1 + 1;
x.2 = 2;
z = x.2 + 1;
```

此时 `y == z` 当且仅当 `x.1 == x.2`。

## 1.2 Informal Semantics of SSA

> The φ-function is the most important SSA concept to grasp. It is a
> special statement, known as a pseudo-assignment function
> [phoney-function].
>
> The purpose of a φ-function is to merge values from different
> incoming paths, at control-flow merge points.

```c
x = input();
if (x == 42) {
  y = 1;
} else {
  y = x + 2;
}

print(y);
```

```c
x = input();
if (x == 42) {
  y.1 = 1;
} else {
  y.1 = x + 2;
}

y.3 = phi(y.1, y.2);
print(y);
```

> Such pseudo-functions are required to maintain the SSA property of
> unique variable definitions, in the presence of branching control
> flow.

从 propagator model 的角度看，
变量的 unique name 保证了可以用变量来代表 cell，
而保持 cell 作为 entity 的 identity 不乱。

phi-function 对于 propagator model 来说就是两个 cell 的 merge。
runtime 只有一个 branch 的值会 merge 到 phi 所定义的 cell。
但是注意 SSA 是用于静态分析的，
在静态分析时，变量所对应的 cell 保存的是分析用的信息。
此时两个 branch 都有信息需要 merge 到 phi 所定义的 cell。

> It is important to note that, if there are multiple φ-functions at
> the head of a basic block, then these are executed in parallel,
> i.e., simultaneously not sequentially.

也就是说 basic block 开始要用 scheme 的 `let` 语义。

> This distinction becomes important if the target of a φ-function is
> the same as the source of another φ-function, perhaps after
> optimizations such as copy propagation (see Chap. 8).

在初次构造 SSA 的时候，这种情况不会出现，
但是 optimization 的过程中可能会出现。

> When φ-functions are eliminated in the SSA destruction phase, they
> are sequentialized using conventional copy operations, as described
> in Algorithm 21.6.  This subtlety is particularly important in the
> context of register allocated code (see Chap. 22).

parallel `let` 语义要被转化为 sequential `let*`，
就像 scheme 中 `let` 需要被 desugar 为 `let*`。


> Strictly speaking, φ-functions are not directly executable in
> software, since the dynamic control-flow path leading to the
> φ-function is not explicitly encoded as an input to φ-function.

可以理解为 phi 是需要忘掉「其参数 variable 是来自哪个 block」这个信息的。

> This is tolerable, since φ-functions are generally only used during
> static analysis of the program. They are removed before any program
> interpretation or execution takes place.

带有循环的 SSA 变换的例子：

```c
x = 0;
y = 0;

while (x < 10) {
  y = y + x;
  x = x + 1;
}

print(y);
```

变换为：

```c
x.1 = 0;
y.1 = 0;

while (
  x.2 = phi(x.1, x.3);
  y.2 = phi(y.1, y.3);
  x.2 < 10
) {
  y.3 = y.2 + x.2;
  x.3 = x.2 + 1;
}

print(y.2);
```

注意，在 SSA 代码中的运行过程中，并不是每个变量都只能被赋值一次。
比如上面的代码，在 loop 的过程中，loop body 中的变量会被赋值多次。

也就是说，我们只需要 static single assignment 而不是 dynamic single assignment。

dynamic single assignment 在 automatic parallelization 中有用。
比如 linear logic 和 interaction nets 中的 variable 在运行时只能被用一次。


> Full details of the SSA construction algorithm are given in
> Chap. 3. For now, it is sufficient to see that:
>
> 1. A φ-function has been inserted at the appropriate control-flow
>    merge point where multiple reaching definitions of the same
>    variable converged in the original program.
>
> 2. Integer subscripts have been used to rename variables x and y
>    from the original program.

算法也许也是先插入 phi，再 rename。
因此上面的例子的 subscripts 是 1 2 3 而不是 1 3 2。

## 1.3 Comparison with Classical Data-Flow Analysis

> As we will discover further in Chaps. 13 and 8, one of the major
> advantages of SSA form concerns data-flow analysis. Data-flow
> analysis collects information about programs at compile time in
> order to make optimizing code transformations. During actual program
> execution, information flows between variables. Static analysis
> captures this behaviour by propagating _abstract_ information, or
> data-flow facts, using an operational representation of the program
> such as the control-flow graph (CFG). This is the approach used in
> classical data-flow analysis.

也就是说在 SSA 之前人们就在用 propagation 来解决静态分析中的问题了，只不过是用的是 CFG。
SSA 是解决静态分析问题的方案之一，直接学习 LLVM 的人可能会忽略这一点。

> Often, data-flow information can be propagated more efficiently
> using a functional, or sparse, representation of the program such as
> SSA.

所谓 sparse，就是分析的单元从 CFG 的 basic block，
改为更细的 instruction（SSA value）。

> For other data-flow problems, properties may change at points that
> are not variable definitions. These problems can be accommodated in
> a sparse analysis framework by inserting additional
> pseudo-definition functions at appropriate points to induce
> additional variable renaming. See Chap. 13 for one such instance.

看来 LLVM 的 MemoryDef 就是 pseudo-definition 的例子，
当然还有 MemoryUse 作为 pseudo-use。

> Part II of this textbook gives a comprehensive treatment of some
> SSA-based data-flow analysis.

## 1.4 SSA in Context

> The majority of current commercial and open source compilers,
> including GCC, LLVM, the HotSpot Java virtual machine, and the V8
> JavaScript engine, use SSA as a key intermediate representation for
> program analysis.

> Many compilers that use SSA form perform SSA elimination before
> register allocation, including GCC, HotSpot, and LLVM.  Recent
> research on register allocation (see Chap. 22) even allows the
> retention of SSA form until the very end of the code generation
> process.

# 2 Properties and Flavours (Philip Brisk and Fabrice Rastello)

## 2.1 Def-Use and Use-Def Chains

def-use chain 是 def cell 到 use cell 的一对多映射。
use-def chain 是 use cell 到 def cell 的一对一映射。

介绍 SSA 如何简化 def-use chain：

> First, SSA form simplifies def-use chains as it combines the
> information as early as possible.

在 propagator model 看来，def cell 就是 put cell，
SSA 要求 put cell 的位置唯一。

但是从 propagator model 看来，
即使不要求这个唯一性，
propagator network 依然奏效。

那么，为什么不直接给每个变量一个 cell，
而是要给每个变量的一个版本一个 cell 呢？

从 Fig. 1.1 Example control-flow graph for non-zero value analysis 可以看出：
因为代码形成分支的时候，可能有些 put 在运行时是走不到的，
不同的版本可以区分这些分支中对变量的 put 为不同的 cell。

但是只考虑 abstract information 或者说 data-flow facts 的 merge，好像没有差别。
不同的分支中产生的 data-flow facts 最终是要被 merge 到一起的，
为什么不直接当场对同一个变量的 cell 做 merge 呢？

也许是为了查询不同代码位置上的 data-flow facts，
只有给某个位置分配一个 cell，才能查询这个位置的 data-flow facts。

## 2.2 Minimality

TODO

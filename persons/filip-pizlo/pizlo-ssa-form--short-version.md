---
title: Pizlo SSA form (short version)
author: Filip Pizlo
source: "https://gist.github.com/pizlonator/79b0aa601912ff1a0eb1cb9253f5e98d"
date: 2025-02-14
---

<motive>
[2025-10-14]
学习 bril 的时候，发现有两个版本的 SSA，
旧版本就是经典的使用 phi 的 SSA，
新版本是这个 Pizlo SSA。

这里作者原文就是 markdown，
所以我用 XML 来写笔记了。

另外，在刚开始学习 SSA 的时候，
就发现一个新产生的更好的方案，
是很棒的感觉。

另外需要学习的文档：

- 这篇文档的加长版本：https://gist.github.com/pizlonator/cf1e72b8600b1437dda8153ea3fdb963
- 类 SSA 方案的总结：https://bernsteinbear.com/blog/ssa/
- Filip Pizlo 的一些回答：https://news.ycombinator.com/item?id=43009952
</motive>

Here's a much more complete description of how I do SSA,
beyond just how I do Phis:

- https://gist.github.com/pizlonator/cf1e72b8600b1437dda8153ea3fdb963

This describes how I do SSA form, which avoids the need to have any
coupling between CFG data structures and SSA data structures.

Let's first define a syntax for SSA and some terminology. Here's an
example SSA node:

    A = Add(B, C)

In reality, this will be a single object in your in-memory
representation, and the names are really addresses of those
objects. So, this node has an "implicit variable" called A; it's the
variable that is implicitly assigned to when you execute the node. If
you then do:

    X = Sub(A, 1)

Then "A" is just a pointer to the Add node, and we're using the
implicit variable "A".

Here's an example function:

    int foo(int a, int b)
    {
        int x;
        if (a)
            x = b + 1
        else
            x = b * 2
        return x + 42;
    }

Here's an SSA program with a Phi in Pizlo form:

    root:
        A = GetArgument(0)
        B = GetArgument(1)
        Branch(A, then, else)
    then:
        X1 = Add(B, 1)
        Upsilon(X1, ^X)
        Jump(return)
    else:
        X2 = Mul(B, 2)
        Upsilon(X2, ^X)
        Jump(return)
    return:
        X = Phi()
        R = Add(X, 42)
        Return(R)

In Pizlo form:

- Every SSA node has an implicit variable, as mentioned above.
- Every Phi node has a shadow variable in addition to the implicit variable.

<note>
这里说每个 SSA node 都有一个隐含变量，
SSA node 说的是 instr，
隐含变量说的是 assignment 的 target
也就是 instr 的 dest。

这种理解方式很不错，
因为可以把一个 basic block
想象成一个 key-value map，
其中 key 就是 variable。

single-assignment constraint
就对应于 key 的唯一性 constraint。
</note>

Let's say that given a Phi like "X = Phi()", the implicit variable is
called "X", and the shadow variable is called "^X".

Therefore, the semantics of an upsilon like "Upsilon(X1, ^X)" is just
"set ^X = X1". And the semantics of a Phi like "X = Phi()" is just
"set X = ^X".

<note>
这就是为什么 bril 的设计者，
改用了 `set` 与 `get` 这里两个 operator name。

我觉得 `get` 在这里适合叫做 `use`，
这样 single-use constraint 就明显了。

但是 `X = Phi()` 或 `X = get` 也有优点，
因为从 key-value map 的角度看，
所有 `=` 前面的都是 key。

这样看来 single-use constraint
和 single-assignment constraint
都可以被视为是 unique-key constraint。

从 key-value map 的角度看，
`set ^X = X1` 的时候，
其实是在修改 `X` 这个 key 下的 variable。
</note>

In other words, you can think of Upsilon as being a side effect (a
store to a shadow variable). And you can think of Phi as being a side
effect (a load from a shadow variable). You can model them that way in
your effect analysis to block reordering Upsilons and Phis.

But also, the shadow variables of Phis in Pizlo form are "Static
Single Use" (SSU) variables. This falls out naturally from the fact
that the only syntax for loading a shadow variable is the Phi
itself. So you can think of Pizlo form as "SSA-SSU form".

The main benefit of this form is that basic blocks - and all CFG data
structures - have zero knowledge about SSA. There are no basic block
arguments.

<note>
这确实是很好的设计。
不是说我们要避免用 block label 作为 operator 的参数，
毕竟 jmp 和 ret 都是用 label 为参数。
而是说复杂的 phi 指令，可以被分解为两个简单的指令。
这一点非常好。
</note>

There's no requirement that Phis appear at the tops of
blocks. In fact, this is a valid program in Pizlo form (albeit
suboptimal):

    M = Stuff(...)
    Upsilon(M, ^N)
    N = Phi()
    MoreStuff(N)

Here, there's a Phi in them middle of a basic block, and there's an
Upsilon just before it. That's fine. This is important, because it
means that you can do CFG transforms that blow away control flow edges
without worrying about fixing your Phis.

<note>
把 phi 依赖 block label，
就依赖了 CFG 的结构本身。
为什么之前人们没有发现这个缺陷？
</note>

In any Pizlo-form compiler, you'll want to have a Phi simplification
pass, which you can implement either by running Cytron or by running
any other SSA converter. The simplest is just to just fixpoint the
rule that if you have a Phi that has Upsilons that only use the Phi or
exactly one other value, then replace the Phi with that other value.

<note>
最后这一段我还看不懂。
</note>

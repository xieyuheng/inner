---
title: computability and logic (fifth edition)
author: [george s. boolos, john p. burgess, richard c. jeffrey]
year: 2007
---

# My Motive

[2026-03-19] 在实现 lambda-lisp 时，
想要判断递归定义的函数之间的等价。

但是一般情况下，函数之间的等价是不可判定的，
如何找到一个可判定的子集？

如果找到了，就可以作为 dependent type system 的基础的等价关系。
因此要学习可判定性的基础知识。

# Chat

https://chat.deepseek.com/a/chat/s/d8e7e308-71da-43b2-ba95-91ddb0d6e56f

# 1 Enumerability

> Our ultimate goal will be to present some celebrated theorems about
> inherent limits on what can be computed and on what can be proved.

> Before such results can be established, we need to undertake an
> analysis of computability and an analysis of provability.

> The main topic is a distinction between two different kinds of
> infinite sets, the enumerable and the nonenumerable.

## 1.1 Enumerability

> An _enumerable_, or _countable_, set is one whose members can be
> enumerated: arranged in a single list with a first entry, a second
> entry, and so on, so that every member of the set appears sooner or
> later on the list.

强调了 list 中所列的是元素的名字，而不是元素本身。

> A list that enumerates a set may be finite or unending. An infinite
> set that is enumerable is said to be _enumerably infinite_ or
> _denumerable_.

用直觉的方式定义可数集，
然后解释如何用以自然数为定义域的映射来捕捉这个直觉概念。

有限集当然是可数的。
与自然数集之间的双射，就展示了无穷集的可数。
如果考虑自然数集到某个集合的满射，
那么有限集的可数也可以用函数展示。

甚至偏函数（partial function）也可以，并且使用起来非常方便。
因为重要的是要求每个元素都有自然数与其对应，
而不是要求每个自然数都对应于一个元素。

## 1.2 Enumerable Sets

给出了很多具体的 enumerable sets 的例子。

# 2 Diagonalization

给出 non-enumerable set 的构造方式。

最重要的是，自然数（或正整数）的所有子集的集合是不可数的。

# 3 Turing Computability

> A function is _effectively computable_ if there are definite,
> explicit rules by following which one could in principle compute its
> value for any given arguments.

这章和后面几章，都是要尝试捕捉上面这个直觉定义。
通过证明很多尝试捕捉 effectively computable 的定义等价，
我们展示了我们成功捕捉到了这个直觉定义。

就像第一章，用 listing 给出 enumerability 的直觉定义，
然后用函数捕捉这个定义一样。

用形式定义捕捉直觉定义的过程，
其成功与否的判断，在于一致性与实用性。

> The instructions must be completely definite and explicit. They
> should tell you at each step what to do, not tell you to go ask
> someone else what to do, or to figure out for yourself what to do:
> the instructions should require no external sources of information,
> and should require no ingenuity to execute, so that one might hope
> to automate the process of applying the rules, and have it performed
> by some mechanical device.

尝试用来捕捉 effectively computable 这个直觉概念的定义，
之所以有用，就在于人们可以设计机器来帮助人类快速完成这类计算。
这就要求描述计算方式（算法）步骤明确且没有歧义等等。
但是现在 AI 的发展大大拓宽了机器可以帮助人类做的脑力劳动。

> ... in principle it could be completed in a finite amount of time if
> we stayed in good health so long, or the machine stayed in working
> order so long; but in practice we will die, or the machine will
> collapse, long before the process is complete.

也就是说即便是 effectively computable 这种对计算的定义，也是不够实用的。
想要有更实用的定义，还要考虑算法的复杂性。
「可计算」要换成「可用高效地算法计算」。

> Our eventual goal will be to prove that certain functions are _not_
> computable, _even if_ practical limitations on time, speed, and
> amount of material could somehow be overcome, and for this purpose
> the essential requirement is that our notion of computability not be
> too _narrow_.

能够知道哪些问题是在这种宽松的定义下为「不可计算」的，
也是十分有用且重要的。
因为这能避免人们为了寻找某个问题的算法而作无用功。

介绍图灵机：

- 用 flow graph 来代表图灵机程序。
- 为了展示图灵机计算过程，设计了一种简洁的格式。
- 以「翻倍」这个图灵机程序为例子，展示图灵机的运行过程与能力。

下面介绍「奇偶」「加法」「乘法」。

然后介绍一般的 n 元函数的参数以及返回值，
如何表示在图灵机中。
这类似现在计算机构架中的 calling convention，
只有约定好了 calling convention，函数才能复合。

> A numerical function of k arguments is _Turing computable_ if there
> is some Turing machine that computes it in the sense we have just
> been specifying.

图灵为什么用形式化的「图灵机」这种简单的机器，
来捕捉「可计算性」这个概念，我们很熟悉了。
但是在 AI 的时代，应该如何做出新的定义，
以捕捉更广泛的 AI 的概念呢？

可能就是「神经网络」吧？

# 4 Uncomputability

## 4.1 The Halting Problem

能用图灵机表达的函数的集合显然是可数的，
而所有（自然数的或正整数的）函数的集合是不可数的
（因为谓词是特殊的函数，而谓词对应于子集）。

因此有大量的函数都是不可计算的。

为了证明自然数的所有子集的集合是不可数的，
我们使用了「对角线方法」，这个方法本身就是具有构造性的。
可以以此为基础来构造「不可计算函数」的例子。

用对角线法所构造出来的函数是「图灵不可计算」的，
但是回顾 effectively computable 的定义，
这个函数看起来是可以计算的，
有明确的规则让我们可以计算出它的每一个值。

但是使用对角线法来构造「图灵不可计算函数」时，
需要判断一个图灵机是否能停机。
而停机问题是不可计算的。

> This is the essential question: determining whether machine Mn,
> started scanning the leftmost of an unbroken block of n strokes on
> an otherwise blank tape, does or does not eventually halt.

> Is _this_ perfectly routine? Must there be some point in the routine
> process of following its operations at which it becomes clear that
> it will never halt?

在实现 lambda-lisp 中的等价判断函数使，我就是这种感觉。
对于简单的例子，所实现的判断函数可以正常运行，
但是对于复杂的例子，会无限循环。
找出具体导致无限循环的问题，看似可以解决，
但是如何知道没有别的更复杂的例子，
其中包含了没有考虑到的情况，
会再次导致判断函数无限循环？

下面证明停机问题是图灵不可计算的。
其归谬法构造独立于对角线方法，
但也是受对角线方法的启发，有异曲同工之妙。

# 5 Abacus Computability

这一章所构造的新机器将用于证明，
所有的递归函数都是图灵可计算的。

这种证明方式很像是编译器了：
将 abacus machine 的程序编译到 turing machine。

## 5.1 Abacus Machines

图灵机用于描述正整数上的函数很方便，因为 stroke 代表 1。
但是为了方便证明别的捕捉可计算性的方式与图灵机等价，
需要把正整数扩展为自然数。

所谓 abacus machine，其实就是带有无穷多个寄存器的机器，
并且不限制寄存器所能保存的自然数的大小。

primitive instruction 只有两个：

- add one to box m and go to r
- if box m is not empty, then subtract one from box m and go to r
  if box m is empty, then go to s.

其他的 instruction 都可以用这两个来实现。

## 5.2 Simulating Abacus Machines by Turing Machines

只给 abacus machine 两个 primitive instruction，
就是为了编译到 turing machine 时方便。

和 turing machine 一样，
这里也要约定一个 calling convention，
这里允许了函数的返回值被保存到指定的寄存器中，
而不是像现代计算机构架一样，指定一个寄存器来保存返回值。

## 5.3 The Scope of Abacus Computability

证明 abacus machine 能用来实现所有的递归函数。

# 6 Recursive Functions

## 6.1 Primitive Recursive Functions

这里的定义感觉这里所描述的计算模型才是对数学来说最自然的，
虽然没有 lambda abstruction，
不能用匿名函数来形成 abstruction，
但是可以通过定义新函数来形成 abstruction。

basic functions:

- zero (constant function)
- successor (add1)
- identity and projection

means of combination:

- composition

  ```scheme
  (h/n x1 ... xn) = (f/m (g1/n x1 ... xn) ... (gn/n x1 ... xn))
  ```

- primitive recursion

  ```scheme
  (h x 0) = (f x)
  (h x (add1 prev)) = (g x prev almost)
    where almost = (h x prev)
  ```

  上面的 primitive recursion 只能用来定义二元函数，
  想要定义多元函数，就必须把上面的 x 看成是 x1 ... xn 的缩写。

  如果有 lambda，就不必带上 x，
  可以用 "the little typer" 中的 `rec-Nat` 来实现 primitive recursion：

  ```scheme
  (define (rec-Nat n base step)
    (match n
      (zero base)
      ((add1 prev)
       (= almost (rec-Nat prev base step))
       (step prev almost))))
  ```

由上述方法定义的函数称作 primitive recursive function。
注意，不能自由地使用递归定义，只能用上面给定的递归组合子。

使用递归组合子的意义在于：
找到安全的递归定义模式，
使得所定义的函数是 total function。

## 6.2 Minimization

> We now introduce one further process for defining new functions from
> old, which can take us beyond primitive recursive functions, and
> indeed can take us beyond total functions to partial functions.

回顾 effectively computable 的定义，
这个定义是就 partial function 而言的，
重点在于有步骤明确的算法，
而不要求所定义的是 total function。

比如 turing computable 所定义的函数，
在某些参数上可能不 halt。

因此允许自由地使用递归定义也是可以的，
虽然所定义的递归函数在某些参数上不收敛。

这一章想要定义的 minimization，
是用搜索找函数的零点。
这个搜索过程显然可能会导致 partial function。

加上这个 minimization 算子后，
所能定义的函数就从 primitive recursive function 的集合，
变成了 recursive function 的集合。

> The hypothesis that, conversely, all effectively computable total
> functions are recursive is known as _Church’s thesis_ (the
> hypothesis that all effectively computable partial functions are
> recursive being known as the extended version of Church’s thesis.)

> The interest of Church’s thesis derives largely from the following fact.
> Later chapters will show that some particular functions of great
> interest in logic and mathematics are nonrecursive.

最简单的 nonrecursive function 的方式，
和构造图灵不可计算函数类似，
也是要用到对角线方法。

对角线方法的重点在于给函数以编码，
这个编码使得我们可以把作用于自然数的函数，
视为作用于所对应的函数的函数。

而在写解释器或者编译器的时候，
函数的定义域扩展到了所有的 value，
函数被编码到 value 的方式是自然且实用的。

另外，好像只有只考虑一阶函数的时候才需要编码，
如果像是在 lambda calculus 中，允许任意的高阶函数，
halt function 就可以直接被视为高阶函数了。

# 7 Recursive Sets and Relations

## 7.1 Recursive Relations

> A set of, say, natural numbers is _effectively decidable_ if there
> is an effective procedure that, applied to a natural number, in a
> _finite amount of time_ gives the correct answer to the question
> whether it belongs to the set.

注意 "finite amount of time"，
这要求 characteristic function 是 total function。

> A set is called _recursively decidable_, or simply _recursive_ for
> short, if its characteristic function is recursive, and is called
> _primitive recursive_ if its characteristic function is primitive
> recursive.

用 recursive set 和 relation，配合 lisp 的 cond，
可以很方便地定义大量新的 recursive 函数，
几乎是一个实用的程序语言了。

之所以方便，
就在于作为 relation 的 characteristic function
的 predicate 之间可以进行布尔运算。
只不过布尔值不是独立的类型，
而是嵌入在自然数中的 0 和 1。

TODO

## 7.2 Semirecursive Relations
## 7.3 Further Examples

# 8 Equivalent Definitions of Computability

## 8.1 Coding Turing Computations
## 8.2 Universal Turing Machines
## 8.3 Recursively Enumerable Sets

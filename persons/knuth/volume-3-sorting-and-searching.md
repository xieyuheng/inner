---
title: volume 3 -- sorting and searching
---

# My Motive

[2025-12-28] 在实现 x-lisp-forth 时，
为了让每个在 `equal?` 的判断下相等的 value，
都有相同的 hash code，需要先把 record 的 key 排序。
所以要学一下排序算法。

类似的 hash table 也需要按照 key 排序，才能计算 hash code，
record 的 key 是 symbol，而 hash table 的 key 可能是任何数据。
因此要定义不同类型数据之间的序关系，从而能给任何数据所构成的 array 排序，

最典型的，支持任意数据之间的序关系的语言是 erlang。

# Chapter 5 Sorting

> Although dictionaries of the English language define “sorting” as
> the process of separating or arranging things according to class or
> kind, computer programmers traditionally use the word in the much
> more special sense of marshaling things into ascending or descending
> order. The process should perhaps be called ordering, not sorting;
> but anyone who tries to call it “ordering” is soon led into
> confusion because of the many different meanings attached to that
> word.

充分展示了英文在表意方面之不精确。
"sort" 在不同语境下，可能代表「排序」「整理」「分类」。
所谓「精确」就在于不依赖语境。

> Consider the following sentence, for example: “Since only two of
> our tape drives were in working order, I was ordered to order more
> tape units in short order, in order to order the data several orders
> of magnitude faster.”

“Since only two of our tape drives were in working order（状态）,
I was ordered（命令）to order（订单）more tape units in short order（紧急）,
in order（目的）to order（排序）the data
several orders（数量级）of magnitude faster.”

「由于我们的磁带驱动器只有两个能正常运转，
我接到命令尽快订购更多磁带单元，
以便能快数个数量级地整理数据。」

汉字可以直接地、形象地锚定不同的核心概念。
然后通过组字为词，创造大量精确的复合词。
复合词之间又由其组成部分之相同而相联系，
使得人们可以猜测新词的意义。

> Mathematical terminology abounds with still
> more senses of order (the order of a group, the order of a
> permutation, the order of a branch point, relations of order, etc.,
> etc.). Thus we find that the word “order” can lead to chaos.

「排序」其实蕴含了分类。
比如想要从一个 array 中取出 ADT 的不同 variant。
与其按照 variant 来 filter 多次，不如直接对 array 进行排序，
假设不同 sum type 之间的某种任意顺序都可以完成这个任务。
一次排序就能完成所有的分类，而 filter 对于每类都要运行一次。

> Computer manufacturers of the 1960s estimated that more than 25
> percent of the running time on their computers was spent on sorting,
> when all their customers were taken into account. In fact, there
> were many installations in which the task of sorting was responsible
> for more than half of the computing time.

> Even if sorting were almost useless, there would be plenty of
> rewarding reasons for studying it anyway! The ingenious algorithms
> that have been discovered show that sorting is an extremely
> interesting topic to explore in its own right.  Many fascinating
> unsolved problems remain in this area, as well as quite a few solved
> ones.

> From a broader perspective we will find also that sorting algorithms
> make a valuable _case study_ of how to attack computer programming
> problems in general.

可以尝试总结出来类似 polya 的 table -- "how to program it"。

> Many important principles of data structure manipulation will be
> illustrated in this chapter. We will be examining the evolution of
> various sorting techniques in an attempt to indicate how the ideas
> were discovered in the first place. By extrapolating this case study
> we can learn a good deal about strategies that help us design good
> algorithms for other computer problems.

TODO page 4

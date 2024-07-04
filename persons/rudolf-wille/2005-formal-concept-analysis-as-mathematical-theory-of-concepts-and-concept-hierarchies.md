---
title: Formal Concept Analysis as Mathematical Theory of Concepts and Concept Hierarchies
year: 2005
---

# 学习动机

我大致浏览了以下，
看到论文里可能在用 `... => ...` 这种 implication
来定义 concept lattice。
首先要学会这个。

# 1 Formal Concept Analysis, Mathematics, and Logic

> Only after more than a decade of development, the connections to
> Philosophical Logics of human thought became clearer, mainly through
> Charles Sanders Peirce's late philosophy.

> The concern of that paper ("Communicative Rationality, Logic, and
> Mathematics") is to explain and to substantiate the following
> thesis:

> The aim and meaning of mathematics finally lie in the fact that
> mathematics is able to effectively support the rational
> communication of humans.

> Here we only recall the key arguments founding this thesis:

> First, logical thinking as expression of human reason graps actual
> realities by the main forms ofhuman thought: concepts, judgments,
> and conclusions (cf. [Ka88], p.6).

"conclusions" 一词可能代表完整的逻辑推演系统。

> Second, mathematical thinking abstracts logical human thinking for
> developing a cosmos of forms of potential realities (see [Pe92],
> p.121).

只有通过 "potential realities" 联系到实际，才符合皮尔士的实用主义。

> Since concepts are also prerequisites for the formation of judgments
> and conclusions, we can adapt the above thesis to Formal Concept
> Analysis as follows:

> The aim and meaning of Formal Concept Analysis as mathematical
> theory of concepts and concept hierarchies is to support the
> rational communication of humans by mathematically developing
> appropriate conceptual structures which can be logically activated.

我觉得更符合实用主义的动机是，
FCA 可以帮助人们从已有的数据发现新的概念。

# 2 Concepts and Formal Concepts

> Concepts can be philosophically understood as the basic units of
> thought formed in dynamic processes within social and cultural
> environments.

"the basic units of thought" 就是说概念是人们思考的基础对象，
而这种假定，就是构建了一个人与世界的模型，去解释现象。
皮尔士的实用主义，也在于构造这样的模型。

回想 "The Book of Why"，
当一位提问者问 Why 一类的问题的时候，
回答者回答的形式是在一个模型中解释 Why，
而提问者所期待的也是 Why 在一个模型中被解释。

整个第二章就是作者读了另一本书关于 concept 的书：

> "Begreifen und Verstehen. Ein Buch über Begriffe und Bedeutungen"
> ("Conceiving and Understanding. A Book on Concepts and Meanings")
> written by Thomas Bernhard Seiler.

然后一章一章地用 FCA 来解释那本书中关于 concept 的诸多观点。

## 2.1 Concepts Are Cognitive Acts and Knowledge Units

我不太理解作者这一章想讲什么。

> Personal concepts exceed conventional meaning with additional
> aspects and connotations. Conventional concepts and meanings are
> objectified and standardized contents, evolved in recurrently
> performed discourses.

我尤其不理解上面所讲的，
personal concepts 与 conventional concepts 之间的区别。
这种差异看来已经不是可以从数学角度去区分的了。

## 2.2 Concepts Are Not Categories, but Subjective Theories

这一节有趣的例子是关于三角形的：

```
语境 = （某些具体三角形，特殊的三角形类别）
```

此时 lattice 的 top 是「三角形」这个概念本身，
可以想象三角形这个概念本身又是某个更大的 lattice 中的非 top 节点。

由于偏序关系不排至无关系的元素，
所以整个世界中的所有概念都可以被包含到一个大语境下。

- 可否设计带有模块系统的 FCA，
  使得人们所写的 context 可以互相 import 呢？

  显然可以 import 别人的 context，然后向里面加新的 object 和 attribute。
  并且，考虑到 context 的函数式实现，就算是删除 object 和 attribute 的操作，
  或者修改邻接关系的操作，都是可以在 import 之后做的。

> It is not surprising that the logic of the lexical word concepts of
> triangles is determined by implications with one-element premise and
> incompatibilities; seemingly, our everyday thinking has
> intersubjectively incorporated the predominant use of logical
> implications with one-element premise (cf. [Wi04]).

我还没有完全理解这里的论点，
但是 implication with one-element premise 确实重要。

- Horn clause 刚好相反，是 implication with one-element conclusion。

## 2.3 Concepts Are Not Generally Interlinked in the Sense of Formal Logic

也许，personal concepts 与 conventional concepts 之间的区别是说，
implication 不能理解为谓词逻辑意义上的集合包含关系，
不是谓词逻辑，而是某种和人的主观意识相关的模态逻辑。

后面讲了一个关于治疗厌食症（anorectic）病人的例子，
首先让病人填人际关系表（一个 CrossTable），
然后把所获得的 concept lattice 作为诊断结果，
来观察病人人际关系中的问题。

为什么要分析厌食症病人的人际关系呢？
因为：

> Since such a patient is understood to suffer from a loss of control
> over interpersonal relationships, the examination is performed to
> make disturbed relationships conscious so that she becomes able to
> understand those disturbances and to overcome her mental disorder.

可能只是简单地代表主观的概念与客观的概念，
但是我想没必要这样区分，
因为逻辑（就算是谓词逻辑）都能同时应用到主观和客观两种情况。

## 2.4 Concepts Are Domain Specific and Often Prototypical

TODO

## 2.5 Concepts as Knowledge Units Refer to Reality

TODO

## 2.6 Concepts Are Analogous Patterns of Thought

TODO

# 3 Formal Concept Analysis as Transdisciplinary Mathematics

TODO

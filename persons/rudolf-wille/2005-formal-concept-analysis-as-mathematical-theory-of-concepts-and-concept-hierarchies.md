---
title: Formal Concept Analysis as Mathematical Theory of Concepts and Concept Hierarchies
year: 2005
---

# My Motive

这个论文是作者读了另一个作者的，
一篇关于「概念」的哲学论文之后，
用 FCA 来解释其中论点的过程。

**动机 A：**

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

> Formal concepts result from applications of **derivation operators**
> in formal contexts which represent _domains of interest_.

所谓 "derivation operators" 就是：

- 找一组对象都拥有的属性集合；
- 找一组属性都属于的对象集合。

> Protoconcepts have been formally introduced in [Wi00b] for the
> development of a Boolean Concept Logic with "negation" and
> "opposition" as unary operations (see also [VW03]).

Boolean Concept Logic 看来也是很有趣的理论，
并且是 FCA 的进阶理论。

注意 protoconcept 的定义与 concept 的定义差别很小。

```
concept :-
  extent = derive(intent)
  intent = derive(extent)

protoconcept :-
  derive(extent) = derive(derive(intent))
  derive(intent) = derive(derive(extent))
```

## 2.5 Concepts as Knowledge Units Refer to Reality

> According to [Se01], concepts are adapted to circumstances and facts
> of the world arround us, but do not copy realities.

> Concepts consider things and events out of a specific perspective
> and reconstruct only those aspects and relations which follow from
> the specific view.

形成 concepts 的过程就是构建模型的过程，而每个模型都有局限性。

Context 是 `(G, M, I)`，其中数据是二元组 `(G, M)`；
这一节提到了所谓 many-valued contexts `(G, M, W, I)`，
其中数据是三元组 `(G, M, W)`，
并且称为 objects, attributes, and attribute values,
这与 Clojure 的 Datomic 中所使用的 E-A-V 三元组相同
-- (Entity, Attribute, Value)`。

一个限制是第三个位置相对于前两个位置的单值性，
即，如果前两个位置相等，并且两个三元组
-- `(g, m, w1)` 和 `(g, m, w2)` 都在数据库中，
那么 `w1` 和 `w2` 也相等。

如何用新增的这个 value 维度来处理 specific perspective？

- 我没能理解这里用的数学符号。

## 2.6 Concepts Are Analogous Patterns of Thought

> According to [Se01], concepts do not grasp realities directly, but
> realities are incorporated, examined, and assimilated in an
> analogous manner in cognitive schemata (formed by previous
> experiences) which are finally adapted to the incorporated
> realities. In this way, the analogous character of concepts evolves
> so that conceptual thought can be understood as analogous
> representation of realities.

如果概念只能通过类比之前的经验来把握现实，
难么类比的开端是什么呢？
应该是经验。
经验可以提供类比的基础。

在这样的模型中：

- 要么必须将经验与概念分开，经验不是概念；
- 要么允许概念被转化为经验，经验是特殊的更熟悉的概念。

也许，艺术对于科学的意义，
就在与可以快速提供大量经验。

> Mathematics never represents forms of realities as they are, but
> forms of realities give rise to abstracted mathematical forms and
> structures multifariously.  In this way mathematics becomes
> constantly richer and increases its ability to assist human thought.

现实永远比抽象更丰富，
抽象的永远是现实的影子。

我对于抽象的数学结构的想象，在充分熟悉了之后，是否也能算作经验呢？

- 考虑范畴论中的 diagram chasing，
  人们对范畴论中概念的把握，
  也是来自图论的类比，
  而图论就是人们最直接的拓扑经验了。

- 也许人们所能直接获得的基础经验，
  比如时间、几何、拓扑等等，
  就是人们想象力的边界。
  而正是想象力的边界在限制着人类抽象的数学理论的发展。

这里提到 "Why can concept lattices support knowledge discovery in
databases?" 一文中有更多的关于这个话题的讨论。
可以看一下。

因为 inversion in a circle 中，点和直线也形成的对偶关系，
所以作者构造了一个无穷的 FCA 来理解它们。
这有点牵强，我很难想出这有什么用。
并且我也很难想出，这和这一节开篇所提到的论点有什么关系。

读这篇论文一直给我这种感觉，
好像是 Rudolf 作为 FCA 理论的奠基人，
想要让理论与更广泛的其他理论有所联系，
但是我觉得重点应该放在 FCA 的应用上。

## 2.7 Concepts Are Principally Conscious, but Their Content Is Seldom Fully Actualized in Consciousness

这一节的论点，我同样看不太懂。

但是例子很有意思：

- 以图书馆的书为 objects；
- 以人们给书加的 keywords 为 attribute。

来形成一个 Context。

这种加 keywords 的行为，太普遍了，
比如 github 的 repo 有 keywords，
我自己的笔记有 keywords，
辅助记忆的 mimor 卡片有 keywords。
因此以这个角度入手，去写一个 FCA 的应用，
也许是很不错的。

- 记得我刚刚会写程序的时候，
  想要做一个用 keywords（或者或 tags）寻找文件的系统，
  不是用 path 来在一个 tree 中寻找文件的，
  因为以 keywords 为 path 中的 directory 的名字时，
  在 path 中这些 keywords 是带有顺序的，
  但是在整理文件和搜索文件时，
  我并不在乎 keywords 的顺序。

作者还提到了，使用这个技术的图书馆，
是他们的交叉学科研究中心的图书馆，
作者总是想强调「叉学科研」，
但是我们也要注意 Errett Bishop 的格言：

> Meaningful distinctions deserve to be maintained.

具体地，这个例子中有：

- 377 个 keywords；
- 1554 个文档，平均每个带有 32 个 keywords。

人们使用这个 concept context 的方式就是画出来 concept lattice，
但是显然画出这么大的 concept lattice 是没法看的，
因此人们制作了 137 conceptual views，
也就以一个 keywords 的子集为主题来缩小 concept lattice。

比如 Informatics and Knowledge Processing 这个主题，有 keywords：

- Formalization
- Artificial Intelligence
- Expert Systems
- Knowledge Processing
- Hypertext

这里还描述了一个 use case，
假设一个研究员想要找 literature about expert systems dealing with traffic，
她可以先用 Informatics and Knowledge Processing 这个主题，
点击 Expert Systems，然后再用 Town and Traffic 这个主题，
这个主题有 keywords：

- Parking
- Street
- Traffic
- Town
- Means of Transportation

想象为 lattice 设计 UI/UX 真是有意思。

既然 FCA 的主要结果就是画出 concept lattice 给人看，
那么这种 app 的设计就是核心重要的事情了。

## 2.8 Concepts as Habitual and Virtual Knowledge Can Be Implicitly and Explicitly Actualized

这里给出的例子想要说明，
通过分析 concept lattice 的结构
（比如拓扑结构、对称性等等），
可以发现原数据集中的错误，
并且给出修改方法。

具体的例子是德国建筑条例中，不同条例对房间功能的要求，
objects 是建筑条例，attributes 是房间的功能。

所发现的数据集中的错误，就是条例中不合理的地方。

## 2.9 The Language as Medial System Promotes the Actualization of Concepts

这一节想讲的是 FCA 在语言学中的应用，
具体的形式语境是：
- 以作曲家与古典音乐曲子为 objects；
- 以描述人们对音乐的感受的词为 attributes。

这一节还描述了一个有趣的 attribute exploration 应用：

> In general, an attribute exploration is performed as an interactive
> procedure of questions and answers. The questions ask for the validity
> of attribute implcations deduced from the just present formal
> context. The answers given should be "yes" if the implication is
> considered to be valid in the assumed domain, or "no" and then be
> justified by a counterexample taken to be a new object of the
> underlying formal context. A typical question in our exploration was:
>     Has every musical composition with the attributes "dramatic",
>     "lively", and "transparent" also the attributes "sprightly ",
>     "rhythmizing", and "fast"?

这一篇论文的「动机 A」算是达成了。
在浏览论文时所看到的 implication 就是这一节所说的 attribute exploration。

这是一个很有趣的交互，人适合被做成 app。

另外还提到了 implication 的基，
即所有其他的 implication 都可以由基推导出来。

- 可能这与 sequent calculus 有关，
  因为既然这里所有的逻辑表达式都是 implication，
  那么这里所用的推导规则应该就是 cut。

> ... the Ganter-Algorithm [Ga86] (see also [GW99a]) which guarantees
> that the final list of valid attribute implication forms the
> Duquenne-Guigues-Basis of all attribute implications of the
> resulting context.

## 2.10 Concepts Have Motivational and Emotional Qualities

> Another dream is to develop a conceptually structured lexical
> landscape of knowledge as an extension of language thesauri.

用 FCA 分析词典之类的工具书（拥有近义词反义词之类的信息），
也许这些工具书本身就应该以 concept lattice 的形式呈现。

- https://en.wikipedia.org/wiki/Roget's_Thesaurus
- https://en.wikipedia.org/wiki/Thesaurus

## 2.11 Concepts Have a History and Go Through a Developmental Process

> According to [Se01], each personal concept goes through a long
> history of development in which its content progressively
> changes. Conventional concepts on the one hand are anchored in the
> personal cognition, knowledge and thought, and on the other hand are
> subjected to continuous social change because of their dependence on
> discourses.

所以有 Zettelkasten 这种形式的笔记，可以记录个人概念的演变。

也许人类思想的表面上的 self-dynamics 和 autonomy，
都与 concept lattice 的动力系统有关。

所谓动力系统，就是说允许 concept lattice 变化，
并且研究它的变化规律。

我的意思是，也许通过模拟这个动力系统，
我就能在 AI 系统中模拟人类思想的
self-dynamics 和 autonomy 这些属性。

> In formal contexts and their formal concepts, processes of
> developing concept structures and of building conceptual theories
> can be represented, examined, improved, and documented.

设计一个 FCA 的语言，然后实现模块系统包与管理系统（版本控制）。

- 如果要设计 FCA 的语言，这个语言是否要带有 lambda 呢？
  亦或是没有抽象能力的纯描述语言。

作者总是提到 conceptual landscape 一词，
也表明了 FCA 的目的就是画出 concept lattice 来观看。

这一节给出的例子是，
部分 attributes 是时间区间（半区间）的形式给出的，
比如「早于 1800 年」「晚于 1780 年」等等。

也可以理解为语言学分析，
但是（用索绪尔的话来说）这次是「历时的」而不是「共时的」。

## 2.12 Concept Formation Is Not a Formalizable Automatism

这节意思是说想要自动画出可读性高的 concept lattice 并不容易。

# 3 Formal Concept Analysis as Transdisciplinary Mathematics

> The discussion of Seiler's twelve aspects of concepts shows that
> there are close connections between concepts and formal concepts in
> each of the considered aspects.

我觉得这个结论纯属搞笑。
很多 Seiler 的论点与文章中所讨论的例子的关系非常牵强。
只有例子本身是有意思的，作者对例子的牵强附会的解释反而有害。

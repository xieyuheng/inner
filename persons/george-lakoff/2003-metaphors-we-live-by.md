---
title: metaphors we live by
authors: [george lakoff, mark johnson]
year: 2003
---

# My Motive

[2025-06-02] 看这本书的原因是在设计 simple type 的 lisp 时，
又要重新考虑设计语法的问题。
即便是在 lisp 的框架内，设计语法时也需要一些指导思想，
因此想到了结构主义的「差异即意义」。

与结构主义语言学相对的就是这本书所代表的认知语言学。

"The book of why" 的理论告诉我们，
每个 why 问题的背后都需要一个模型，有了模型才有可能回答 why 的问题。
如何构造模型呢？拓扑模型和几何模型显然都是和人们的体验相关的。
可以尝试从建立模型这个角度去理解这本书中的观点。

又说「数学是关于想象力的」，
但是想象的对象是什么？
好像还是以人们的体验为基础的。

# My Notes

[2025-06-04] 庞加莱在研究拓扑结构的时候，
会尝试从经验出发，寻找人们构造出如是拓扑结构的原因。
说人类的认知结构是以 metaphor 为基础的，
也就是说人们的认知是以经验为基础的。
但是还要认识到人们有从经验做抽象的能力，
例如人们所构造的数学结构。

# 1 Concepts We Live By

想要研究人类 conceptual system 中的 metaphorical structure，
就要研究人类 linguistic system 中的 metaphorical structure。

另外，从科学的角度理解了这些 metaphorical structure，
就可以控制它对我们的影响，甚至构造新的 metaphor。

> The essence of metaphor is understanding and experiencing
> one kind of thing in terms of another.

在两种现象中，观察到相似的关系，就能形成 metaphor。
这和数学结构之间的同构和同态类似。

这一章的典型例子是 ARGUMENT IS WAR metaphor。
为什么人们要用和 war 相关的词语来讨论 argument 呢？
也许和数学结构一样是为了经济性，即可以重用旧词。

> The most important claim we have made so far is that metaphor is not
> just a matter of language, that is, of mere words. We shall argue
> that, on the contrary, human _thought processes_ are largely
> metaphorical. This is what we mean when we say that the human
> conceptual system is metaphorically structured and defined.

# 2 The Systematicity of Metaphorical Concepts

当发现了 ARGUMENT 和 WAR 之间的部分相似性，
并且建立起来了 ARGUMENT IS WAR metaphor，
WAR 的结构就会反过来塑造 ARGUMENT 的结构。

这一章的例子是 TIME IS MONEY metaphor。

# 3 Metaphorical Systematicity: Highlighting and Hiding

> The very systematicity that allows us to comprehend one aspect of a
> concept in terms of another (e.g. ,comprehending an aspect of
> arguing in terms of battle) will necessarily hide other aspects of
> the concept. In allowing us to focus on one aspect of a concept
> (e.g., the battling aspects ofarguing), a metaphorical concept can
> keep us from focusing on other aspects of the concept that are
> inconsistent with that metaphor.

比如在辩论中，如果我输了辩论，那么我就纠正了我的错误的 belief。
但是没法在 war metaphor 中被表达出来的。

> Someone who is arguing with you can be viewed as giving you his
> time, a valuable commodity, in an effort at mutual understanding.
> But when we are preoccupied with the battle aspects, we often lose
> sight of the cooperative aspects.

这一章的例子是关于语言的复杂的 conduit metaphor：

IDEAS (OR MEANINGS) ARE OBJECTS.
LINGUISTIC EXPRESSIONS ARE CONTAINERS.
COMMUNICATION IS SENDING.

> The speaker puts ideas (objects) into words (containers) and sends
> them (along a conduit) to a hearer who takes the idea/objects out of
> the word/containers.

某个文化圈常用的 metaphor 为其提供了默认的 model，
来理解作为 metaphor 的 domain 的现象。
但是这种默认的 model 经常是肤浅的，不科学的，
经常会让人们误入歧途的。

> The part of the metaphor that says LINGUISTIC EXPRESSIONS ARE
> CONTAINERS FOR MEANING entails that words (and sentences) have
> meanings independent of contexts and speakers.

> It is important to see that the metaphorical structuring
> involved here is partial, not total.

metaphor 是同态而非同构。

# 4 Orientational Metaphors

即方位隐喻。

看不同文化的隐喻差异是很有趣的，
比如 "I'm depressed" 中有方位隐喻，
因为 depressed 的词源是 to press down。
而中文翻译「焦虑」的「焦」，
《說文》本作焳。火所傷也。

这里提到了隐喻的经验基础（experiential basis），
想用经验基础来解释隐喻的存在，以及隐喻之间的关系。

# 5 Metaphor and Cultural Coherence

在一种文化内，不同的 metaphor 会趋于 coherence。

# 6 Ontological Metaphors

> Understanding our experiences in terms of objects and substances
> allows us to pick out parts of our experience and treat them as
> discrete entities or substances of a uniform kind.

看来在作者的理解中，
objects 和 substances 的对立，
就是离散与连续的对立。

关于离散：

> Human purposes typically require us to impose artificial boundaries
> that make physical phenomena discrete just as we are: entities
> bounded by a surface.

这里的例子是动词的名词化，
比如 the experience of rising prices
被赋予名词 inflation。

这里给出的例子是：

THE MIND IS AN ENTITY
THE MIND IS A MACHINE
THE MIND IS A BRITTLE OBJECT

> The machine metaphor gives us a conception of the mind as having an
> on-off state, a level of efficiency, a productive capacity, an
> internal mechanism, a source of energy, and an operating condition.

对比一下可以发现中国对 MIND 的隐喻是非常不同的，
在读中国古典著作的时候，可以尝试去发现和总结一下其中的核心隐喻。

关于容器隐喻：

> We are physical beings, bounded and set off from the rest of the
> world by the surface of our skins, and we experience the rest of the
> world as outside us. Each of us is a container, with a bounding
> surface and an in-out orientation. We project our own in-out
> orientation onto other physical objects that are bounded by
> surfaces. Thus we also view them as containers with an inside and an
> outside.

这里说的有些问题，
因为人体并非典型的容器，
不是这个隐喻的 domain 而是 codomain。

即 BODY IS CONTAINER，
意思是有 CONTAINER -> BODY 的 morphism，
并非反过来。
本书中形如 BODY IS CONTAINER，
的句子都是 codomain IS domain。

或者说，人们所创造的拓扑和几何结构，
并非单一或主要来源于人体之内外的体验，
尽管这个体验也是促使人们创造这些结构的经验的一部分。

人对世界的认识确实是以隐喻为基础的，经验确实是认知的基础。
但是人不只能使用一种经验到另一种经验的直接隐喻，
人还能从多种隐喻中抽象出其核心重要的结构，
拓扑和几何结构就是最典型的例子。

> And such defining of a territory, putting a boundary around it, is
> an act of quantification. Bounded objects, whether human beings,
> rocks, or land areas, have sizes. This allows them to be quantified
> in terms of the amount of substance they contain.

这里再次提到了 substance，
可以看出在作者看来 object 与 substance 的对立，
确实是离散与连续的对立，
因为连续这一概念本身的基础正是边界。

下面提到了 VISUAL FIELDS ARE CONTAINERS 这个隐喻，
但是我觉得这已经不是一个简单的隐喻了，
而是人们通过抽象的几何结构来认识空间的行为。

下面所举的 RACE 例子，让人想到写程序时
object as container has properties：

> A race, for example, is anevent, which is viewed as a discrete
> entity.  The race exists in space and time, and it has well-defined
> boundaries.  Hence we view it as a CONTAINER OBJECT, having in it
> participants (which are objects), events like the start and finish
> (which are metaphorical objects), and the activity of running (which
> is a metaphorical substance).

```scheme
(define-class race-t
  (claim participants (list-t participant-t))
  (claim start event-t)
  (claim end event-t)
  (claim running activity-t))
```

```cicada
class Race {
  participants: List(Participant)
  start: Event
  end: Event
  running: Activity
}
```

> Thus, activities are viewed as containers for the actions and other
> activities that make them up. They are also viewed as containers for
> the energy and materials required for them and for their by-products,
> which may be viewed as in them or as emerging from them.

> Various kinds of states may also be conceptualized as containers.

activities 和 states 都通常被理解为 containers，
也解释了为什么集合论可以用来作为很多模型的基础理论。

# 7 Personification

> Perhaps the most obvious ontological metaphors are those where the
> physical object is further specified as being a person. This allows
> us to comprehend a wide variety of experiences with nonhuman
> entities in terms of human motivations, characteristics, and
> activities.

例如：

INFLATION IS A PERSON
INFLATION IS AN ADVERSARY

> The point here is that personification is a general category that
> covers a very wide range of metaphors, each picking out different
> aspects of a person or ways of looking at a person.

> When we are suffering substantial economic losses due to complex
> economic and political factors that no one really understands, the
> inflation is anadversary metaphor at least gives us a coherent
> account of why we're suffering these losses.

# 8 Metonymy

转喻。

> ... we are using one entity to refer to another that is related to it.
> This is a case of what we will call _metonymy_.

> Metaphor and metonymy are different _kinds_ of processes. Metaphor
> is principally a way of conceiving of one thing in terms of another,
> and its primary function is understanding. Metonymy, on the other
> hand, has primarily a referential function, that is, it allows us to
> use one entity to _stand_ for another.

典型的转喻是：

THE PART FOR THE WHOLE

选择了什么 PART 去代表 WHOLE，也包含了意义。

当我们使用转喻时，我们不光指像了被指代物，
还强调了指代物与被指代物之间的关系。

例如：

THE FACE FOR THE PERSON

> We function in terms of a metonymy when we perceive the person
> in terms of his face and act on those perceptions.

又例如：

PRODUCER FOR PRODUCT
OBJECT USED FOR USER
CONTROLLER FOR CONTROLLED
INSTITUTION FOR PEOPLE RESPONSIBLE
THE PLACE FOR THE INSTITUTION
THE PLACE FOR THE EVENT

这种看似模糊的引用，在自然语言中常见，
在程序语言中，想必是很难出现的。
但是其实，程序语言中命名函数与变量的过程，
还是要依赖自然语言。
也就是说，程序语言的语言学要素可以被分为两类：
- 一类用以形成对机器的意义。
- 一类用以形成对人类的意义。

宗教符号都是转喻，比如：

DOVE FOR HOLY SPIRIT

# 9 Challenges to Metaphorical Coherence

两个不同的具体的隐喻，可能被同一个更抽象的隐喻所包含。
此时称这两个更具体的隐喻为 coherence。

# 10 Some Further Examples

TODO

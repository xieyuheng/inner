---
title: transcendental syntax 2.0
author: Jean-Yves Girard
year: 2012
---

# 学习动机

[2025-01-14] 我怀疑 transcendental syntax 所实现的语言，
与 graph-based rewriting 的 inet 类似。

注意 transcendental 一词来自康德。

# Abstract

> How come that finite language can produce certainty — at least a
> sufficiently certain certainty, sometimes apodictic — in the
> presence of infinity? The answer can by no means be found in
> external reality, quite the contrary: it seems that the very purpose
> of semantics is to make this question untractable with the help of
> an ad hoc analytical newspeak in which one cannot even formulate the
> above question. Transcendental syntax comes from the constatation
> that logic is better off when there is no A reality B at all and
> thus restores the priority of syntax over anything else. What
> follows is the present state of a new programme.

# 1 The conditions of possibility of language

## 1.1 Introduction: philosophy

关心哲学的人所发展出来的逻辑学 v.s. 不关心哲学的人所发展出来的逻辑学。

后者的典型是分析哲学：

> Which amounts at following the dominant approach to logic, fregeism,
> the so-called analytical philosophy, which is the philosophy of
> those who don’t care about philosophy.

前者的问题是虽然有指导意义，但是技术上很弱。

> Notwithstanding its technical impotency, philosophy can assume a
> central architectural role. A bit like the role played by category
> theory in mathematics: there are no real theorems about categories,
> moreover, diagrams suck! But categories provide us with a
> structuration of mathematical objects.

## 1.2 Semantics

> The (implicit) legacy of Frege, i.e., the philosophy of those who
> don’t care about philosophy, can be summarised as follows:
>
> - (i) The language is about something else, the denotation
>   (semantics); everything is in the "else".
>
> - (ii) In particular the subject must be expelled.
>
> - (iii) If there is no reality -- no semantics foundations --
>   everything is permitted.
>
> The argument for this is pure scientism: science should be "objective".

"subject must be expelled" 是说要排除所有的主观主义。

后面把科学主义比作巫师和托勒密天文学的讽刺没什么道理。

> As to logic, let us cast an a priori doubt as to the relevance of
> this objectivism: logic is about language, which involves a subject,
> not about objects like physics。; physics gave up objectivism, while
> logic sticks to this archaic conception!

> Aristotle observed long ago that the correction of a syllogism has
> little to do with its factual truth. He would, occasionally, use a
> semantic argument for a _refutation_, but never for a _justification_.
> The fact is that semantics -- whatever quality they may have -- are
> all incomplete: they fail to catch the specificity of language. We
> can indeed see "the" reality as a way of quotienting the language;
> there are various styles of quotients, of variable quality.

Ray 的逻辑学教程中，在解释语义的时候，
是否也是以 refutation 的形式呢？
读的时候没有注意到语义与语法的区分。

## 1.3 Deontics

看后面的用词 "deontic formatting"，
deontic 可能就是指语法对象，
就是实现解释器时的 `Exp` 类型。

> Indeed, the idea that the reality (the denotation) is prior to the
> language (the sense) forgets one detail: like in Voltaire’s joke,
> reality is indeed a creation of the language, for the technical
> reasons just expounded, but also from a common sense remark: the
> language _formats_ the reality.

> The deontic formatting is -- whether one likes it or not --
> absolutely necessary; for instance, in order to avoid Richard’s
> paradox, one must format, i.e., restrict the language. This
> formatting makes evaluation, i.e., semantics, possible.

Richard’s paradox 是：
"The smallest number not definable in less than 12 words."
问题就在于 "definable" 这个词的意义不明，
必须限制在某个有具体定义的 `Exp` 类型下，才能消除悖论。

> The denegation of deontics has a name, essentialism: essentialism
> claims that there is nothing like formatting, that everything is
> given in advance etc.

> Essentialism (the hidden format) is at work at all semantic layers:

> -1: Supposes that all questions receive an answer Y/N; how do we
> deal with l = 2a + 1 (l length of the ship, a age of the captain)
> without format?

> -2: Categories are strongly essentialist; etymologically,
> "morphism"d refers to the format. To the point that categories are
> unable to handle polymorphism, e.g., subtyping, records.

范畴论语义确实很难处理 subtyping。

> -3: The rule of the game; what is this third partner,
> this hidden "referee"?

> In order to handle the referee, it necessary to introduce another
> layer, the deontic layer -4. The origin of this layer is to be found
> in multiplicative proof-nets, but one can find prefigurations in:

所列举的 layer 代表人们想要把语法映射到的不同数学对象，
这种映射就是对语法的解释，因此叫语义。

既然语法本身也构成一个数学对象，
就直接研究这个数学对象本身，以其自身为语义就可以。
毕竟，映射会带来等价类，等价类会忽视一些可能有意义的区别。

> - Prawitz: observed in the 1960s the symmetry between introductions
>   and eliminations. We could say that introductions are those rules
>   who A match B the eliminations (and vice-versa)
>   w.r.t. normalisation. Due to the overformatting of natural
>   deduction, this remark remains but a potentiality.

"overformatting" 理解为等价类分的太细。

> - Herbrand: usually considered as a poor relative of Gentzen’s
>   Hauptsatz, the theorem is only concerned with classical
>   quantification, for which it provides a semantic-free
>   interpretation.

## 1.4 Enters Immanuel

> Kant explains the coherence of perception -- an experimental fact --
> by its _transcendental_ conditions, i.e., the hypotheses that make
> it possible. Whether these conditions are necessary is a delicate
> question that may lead to unwelcome conclusions; but there is no
> problem with sufficiency. I propose to do the same for logic:
> coherence -- not limited to consistency which is but the poor man’s
> coherence --, i.e., cut-elimination, Church-Rosser, the disjunction
> property, are not accounted for by semantics.

> The question is thus "What makes logic coherent?", the only possible
> answer being in the language itself: logical artifacts are
> constructed so as to ensure coherence. To find the hypotheses making
> logic possible, this is _transcendental syntax_.

就像从「要满足某些运算律」开始，来设计具体的代数结构。
对于逻辑学来说，反过来就是从一个 strong normalize 的计算模型开始。

> I daresay that I always did logic in this way, beginning with system
> F in 1970. Linear logic is a typical exercise in transcendental
> syntax: starting with an interesting layer -2 interpretation
> (incomplete or inconsistent like all such interpretations), I found
> the conditions of possibility for the new operations disclosed, what
> involved a not too bad layer -1 semantics -- phase semantics --; but
> what really gave the conditions of possibility of linear logic was
> proof-nets, which levelled with natural deduction.

> There is a big difference being doing something and being conscious
> of doing it; this is why philosophy is important. In my case, the
> reference to Kant (and to Hegel, but I was already conscious of
> doing some sort of dialectics), gave me the words, i.e., enabled me
> to attack the questions frontally. ... By the way, _constructivism_
> is a typical kantian expression, meaning that reality is a
> construction, for instance the result of a formatting.

以语法为主导来发展逻辑学，感觉 lisp 作为 meta language 再合适不过了。

与语法相关的概念有：

- Occurrences -- 例如 prolog 中，
  同一个 logic variable 的多次出现所代表的 unification。

- Name -- name of bound variables 以及 name of record attributes。

TODO 还有什么概念是在语法中有的，而在语义中没有的？

> However, transcendental syntax proper involves much more, namely
> everything related to _certainty_:

> - (i) In which sense is a proof a finite object?
>
> - (ii) How is it possible that a finite object gives us certainty
>   about the infinite?

> - (iii) This certainty cannot be absolute (incompleteness);
>   it is thus relative...  relative to what?

人们从证明所得到的，确实是 certainty。
TODO 但是这时如何得到的呢？

# 2 From multiplicatives to GoI

## 2.1 Multiplicatives

TODO

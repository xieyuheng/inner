---
title: "transcendental syntax 4: logic without systems"
author: Jean-Yves Girard
year: 2022
---

# My Motive

[2025-01-14] 在考虑 inet 的类型系统时，遇到很多难点。
这里想要看看这里的观点会给 inet 的类型系统什么启发。
可能也与我所设想的 propagator model 的类型系统有关。

别忘了有人实现了这里的语言：
https://github.com/engboris/transcendental-syntax

# Abstract

> A derealistic, system-free approach, with an example: arithmetic.

# 1 BHK revisited
## 1.1 A system-free approach
## 1.2 Axiomatic realism
## 1.3 The first leakage: emptiness
## 1.4 The second leakage: operationality
## 1.5 The third leakage: language
## 1.6 The fourth leakage: usine

# 2 The architecture of logic
## 2.1 Logic vs. set theory
## 2.2 Systems vs. toolbox

> So we don’t quite need logical systems: if we are not happy with
> our formulas, connectives, etc., define new ones by biorthogonality,
> establish their basic properties and add them to our data base. This
> stock may take the form of an open toolbox containing various
> designs together with the name of the behaviour they belong to. A
> list of untyped artifacts -- delogicalised proofs -- together with
> their types, those types being attributed externally, by arbitrary
> mathematical methods. The toolbox requires no sophisticated logical
> structure, e.g., a sequent calculus formulation: we can even use the
> most archaic logical formulation (axioms and Modus Ponens), which
> allows us to draw consequences from the principles listed in the
> data base, i.e., combine the tools. No cut-elimination,
> normalisation, etc. at the level of the toolbox is needed, since it
> is the task of the tools themselves: when we combine them by Modus
> Ponens, they initiate a converging merging process.

回归到动态类型语言。

> This is a major improvement over the fishbowl approach for which
> each novelty prompted a change of system, the creation of a
> schismatic chapel.  An approach which culminated with logical
> frameworks [7] where systems T, U, V, ... could be put under the
> same roof with no right to communicate: like hospital patients, each
> of them quarantined in his room, lest he contaminate the others.

这种观点与 Sussman 对 静态类型 vs. 动态类型 的观点类似。

> The fact that l’usine has been delegated to current mathematics,
> i.e., set theory, makes our toolbox absolutely faultless -- except
> the legitimate doubt (section 2.3). The only limit to this approach
> is our own imagination.

## 2.3 Certainty

> The logical discussions of yesteryear were polluted by the obsession
> of foundations.  We must adopt an adult approach to the question and
> reflect upon our certainties or, dually, our doubts.

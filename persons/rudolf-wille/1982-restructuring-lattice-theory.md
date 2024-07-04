---
title: Restructuring lattice theory
subtitle: An approach based on hierarchies of concepts
year: 1982
---

# 学习动机

这篇论文是 Formal Concept Analysis 的奠基论文。

可以在其中学到这个理论的历史与研究动机。

# Abstract

> Lattice theory today reflects the general status of current
> mathematics: there is a rich production of theoretical concepts,
> results, and developments, many of which are reached by elaborate
> mental gymnastics; on the other hand, the connections of the theory
> to its surroundings are getting weaker and weaker, with the result
> that the theory and even many of its parts become more isolated.
> Restructuring lattice theory is an attempt to reinvigorate
> connections with our general culture by interpreting the theory
> _as concretely as possible_, and in this way to promote better
> communication between lattice theorists and potential users of
> lattice theory.

所谓 "as concretely as possible"，
就在于 FCA 是 finite complete lattice 的表示论。

> The approach reported here goes back to the origin of the
> lattice concept in nineteenth-century attempts to formalize
> logic, where a fundamental step was the reduction of a concept
> to its "extent".

所说的 'the reduction of a concept to its "extent"'，
指的就是集合论与谓词逻辑。

> We propose to make the reduction less abstract
> by retaining in some measure the "intent" of a concept.

intent 是属性的集合，
而属性在集合论和谓词逻辑中，
可以用谓词表示。

FCA 本质上是有限的，
实际上在处理无限的（比如连续的）extent 时，
需要对 extent 进行抽样；
在处理连续的 intent 时，
需要用谓词将它离散化。

# 1. Restructuring lattice theory

TODO 重读论文并整理笔记。

> Lattice theory today reflects the general status of current
> mathematics: there is a rich production of theoretical concepts,
> results, and developments, many of which are reached by elaborate
> mental gymnastics; on the other hand, the connections of the
> theory to its surroundings are getting weaker: the result is
> that the theory and even many of its parts become more isolated.

We need to restructuring other theories of mathematics.

> This is not only a problem of lattice theory or of mathematics.
> Many sciences suffer from this effect of specialization.

> Scientists and philosophers are, of course,
> aware of the danger of this growing isolation.

> In [17], H. von Hentig extensively discusses
> the status of the humanities and sciences today.
> One consequence is his charge to "restructure" theoretical developments
> in order to integrate and rationalize origins, connections,
> interpretations, and applications.

> In particular, abstract developments should be brought back to
> the commonplace in perception, thinking, and action.

> Thus, restructuring lattice theory is understood as an attempt to
> unfold lattice-theoretical concepts, results, and methods
> in a continuous relationship with their surroundings.

> One basic aim is to promote better communication between
> lattice theorists and potential users of lattice theory.

> Traditional philosophy considers a concept to be determined
> by its "extent" [extension] and its "intent" [intension, comprehen-
> sion]: the extent consists of all objects (or entities) belonging
> to the concept while the intent is the multitude of all attributes
> (or properties) valid for all those objects (cf. Wagner [36]).

> The approach reported here takes these
> "concept lattices" as the basis and discusses how parts of
> arithmetic, structure and representation theory of lattices may
> be developed out of problems and questions which occur within the
> analysis of contexts and their hierarchies of concepts.

# 2. Concept lattices
# 3. Examples
# 4. The determination problem

> The concept lattice can be understood as a basic answer to
> two fundamental questions concerning a context, namely
> 1. the question of an appropriate classification of the objects,
> 2. and the question about the dependencies between the attributes.

> Hence an important problem is:
> How can one determine the concept lattice of a given context (G,M,I)?

# 5. The description problem

> Connected with the description problem is the basic problem:
> How can one describe the concept Lattice of a given context (G,M,I)?

> "Good" solutions of this problem are helpful for further
> analysis and communication, especially with non-mathematicians.
> The most communicative description is given by Hasse diagrams.
> If we label in a Hasse diagram of L(G,M,I) the circle representing
> ({g}", {g}') by a name for g (g in G) and the circle representing
> ({m}', {m}") by a name for m (m in M), as in the examples in Section 3,
> the diagram retains the full information about (G,M,I) because
> gIm is equivalent to ({g}", {g}') <= ({m}', {m}") by the theorem in Section 2.

> Although Hasse diagrams are frequently used, it is surprising that
> there is no "theory of readable Hasse diagrams".
> Drawing a Hasse diagram is still more an art than a mechanical
> skill that even a computer can work out. Because of the absence
> of a theory, we shall only discuss the development of a Hasse
> diagram by an example.

# 6. From structures to concept lattices
# 7. Measurement
# 8. Completions of partial concept lattices
# 9. Further remarks

> This restructuring of lattice theory
> does not pretend to be complete in any sense.

> Many ties of lattice theory to its surroundings have not been mentioned.
> A comprehensive development of lattice theory
> should integrate and elaborate much more: for instance,
> - origins as in logic (cf. Rasiowa [28]),
> - connections as in geometry (cf. Birkhoff [5]),
> - interpretations as in computer science (cf. Scott [33]),
> - and applications as in quantum mechanics (cf. Hooker [18]).

> Especially, its significance to other mathematical disciplines
> which is still considered to be the main source has to be clarified.

> Besides the interpretation by hierarchies of concepts,
> other basic interpretations of lattices should be introduced;
> an important role is already played by the interpretation of lattices as closure systems.
> Feedback will always be given by the communication with potential users of lattice theory.

> Coming back to the initial question:
> Why develop lattice theory?

> we may conclude that there is no short answer.
> The justification of lattice theory
> arises from its place in the landscape of our culture in general.

# Formal Concept Analysis (Coursera Lecture)

---
- Author: Sergei Obiedkov
- Page: https://www.coursera.org/learn/formal-concept-analysis/home/welcome
---

## What is formal concept analysis?

- https://www.coursera.org/learn/formal-concept-analysis/lecture/huf4I/what-is-formal-concept-analysis?

Formal concept analysis is a framework for data analysis based on the notion of a “concept”.
So what is a concept? The Oxford Dictionary of English defines concept as
“an idea or mental image which corresponds to some distinct entity or class of entities,
or to its essential features, or determines the application of a term (especially a predicate),
and thus plays a part in the use of reason or language”.
This understanding follows an old philosophic tradition
that goes back at least to the Port-Royal logic
and suggests two ways to describe a concept.
One is to simply enumerate all the entities that fall under it. The set of all such entities or objects is the extension of the concept. Of course, in practice, it may be not that simple. The concept of a dog covers a huge number of entities: it’s really a challenge to enumerate all of them. Even worse, it covers also dogs that lived in the past and that will live in future; so this set is potentially infinite. Another way to specify a concept is to name the conditions that an object must satisfy to be considered an instance of the concept. These conditions constitute the concept intension. So we may say that a dog is an animal with four legs, a tail, it barks and so on. Again, it’s not easy—and not always possible—to specify the intension precisely. In FCA, formal concept analysis, we work with concepts that arise in very concretely and formally specified contexts. These formal concepts are only models of concepts of human cognition, and as any model, they have their limitations. The extension of a formal concept depends on our choice of language used to describe the objects, while the intension depends on the objects included in the context. There’s a hierarchy of concepts. One concept is more general than another one if the former covers all objects covered by the latter and some other objects. So the concept of animal is more general than the concept of dog; the concept of furniture is more general than the concept of table. This generality order has some special properties and, mathematically, is a partial order, which implies that two different concepts cannot both be more general than each other, but they can be incomparable. In addition, this partial order is a special mathematical structure called lattice: every two concepts have a unique least general generalization and a unique most general specification. Concept lattices are visualized by line diagrams, where every node corresponds to a concept and more general concepts are placed above less general ones. And these diagrams by themselves often provide a useful insight into the structure of the data. But concept lattices not only offer a visual representation; they can be used to find certain meaningful patterns in data, to navigate through data by traveling from more general concepts down to individual objects, or vice versa, or by exploring neighborhoods of similar concepts. They can also be used for classification of new data, when we see that a new observation fits a particular concept with particular properties. Let’s start by looking at some examples of concept lattices and their diagrams.

## Understanding the concept lattice diagram

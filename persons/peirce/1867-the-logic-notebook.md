---
title: The Logic Notebook
author: Charles Sanders Peirce
date: 1867
---

# 1867-03-23

I cannot explain the deep emotion with which I open this book
again. Here I write but never after read what I have written for what
I write is done in the process of forming a conception. Yet I cannot
forget that here are the germs of the theory of the categories which
is (if anything is) the gift I make to the world. That is my child. In
it I shall live when oblivion has me -- my body.

This matter of the logical principles of the different kinds of
inference is a difficult matter. One way of putting it would be this.

Every symbol denotes certain objects and connotes certain characters.
The symbol represents each of those objects to have each of those
characters. The symbol may be a false one; it may be that the objects
it denotes do not have the characters it connotes. But if `S` is `M`
in this sense -- not merely that `M` is a name for `S` but that it is
the name of a class of things among which `S` is and if `M` is `P` not
merely in the sense that --

    then S is P.

Here the principle is that

    That which is M is what M is.

- **Xie:**

  Think about the duality between object and attribute in
  [formal concept analysis](https://en.wikipedia.org/wiki/Formal_concept_analysis).

  In Peirce's terminology "character" is attribute (in programming we
  also use the word "property" for attribute).

  If we use `:` for "belong" and `<:` for subset, and have the
  following two meanings of "is":

  - `S` is `M` -- `S : M`
  - `M` is `P` -- `M <: P`

  Then the following rule can explain Peirce's principle:

  > That which is M is what M is.

  ```
  S : M
  M <: P
  -------
  S : P
  ```

  > That which [belongs to] M [also belongs to] what M [is as subset of].

  I guess the following abbreviation for the choice of symbols by Peirce:

  | Abbreviation | Meaning            |
  | ------------ | ------------------ |
  | S            | Subject            |
  | M            | Middle (Predicate) |
  | P            | Predicate          |

  This double meaning of "is" first occurs in Aristotle's books.

  TODO Write detailed comparison of Aristotle's "is".

  TODO Can we understand Aristotle's categories as different contexts
  under which the meaning of "is" is different?

Every one of the integrant parts of `m` is an integrant part of each
prime aliquot [factor] of `m` and vice versa.

A purely contentless principle. As a logical principle should be.

Now let us take up the synthetic arguments.

Whatever is a character of every thing denoted by `M` is a character
of `M`. Whatever has every character of `M` is denoted by `m`.

- **Xie:** Suppose we have a set of objects denoted by `M`,
  we can first observe these objects' common attributes,
  then define `M` again by these common attributes.

  After then, we can use the later definition of `M`
  to determine whether a new object as every attributes,
  and write `m : M`.

  We can also update `M` by adding new objects into it,
  and in turn update `M`'s attributes.

  We think about concept lattice as a system
  where elements are reactive tp each other,
  just as in reactive programming.

Here are two principles. But they do not apply to induction and
hypothesis just as they stand.

- **Xie:** The terminology "induction" and "hypothesis" refer to
  Peirce's deduction v.s. induction v.s. hypothesis.

  If we can understand Aristotle's categories as different contexts of "is",
  then we can also understand Peirce's categories (see the first paragraph of this note)
  as different contexts of reasoning.

Whatever is a common character of many things denoted by `M` is likely
to be a character of `m`.

- **Xie:** Here, "common character of many things" means
  the summarise of common attributes might be not complete.

That does not quite hit the point. It does not contain the idea that
the things must have been taken at random out of those denoted by `M`.

- **Xie:** If we select "many things" in random,
  the summarised "common attributes" will be more representivity.

- **Xie:** Merely distinguishing two meaning of "is",
  and understand the duality of object v.s. attribute,
  is not enough to express the reasoning we used in scientific method.

In what point of view shall we regard this necessity for a random
selection?

Suppose we look at the matter thus. Certain things have a certain
character in common. It follows that there _must_ be some genus of
these things which have the character. We cannot take any genus
_lower_ than that which they are selected as belonging to. To take a
higher one would involve a perfectly arbitrary proposition.

- **Xie:** In a concept lattice,
  "lower" means more specific,
  "higher" means more general.

  > Etymology of "genus":
  >
  > Borrowed from Latin genus ("birth, origin, a race, sort, kind")
  > from the root gen- in Latin gignere,
  > Old Latin gegnere ("to beget, produce").

  Think about DNA.

  Think about Chomsky's generative grammar,
  where sentences of a language is generated by some production rules,
  we do not know the rules in advance,
  we can only observe the sentences and guess the rules.

  Maybe we shoud understand "genus" as formal concept here.

I am convinced that this is a very awkward way of taking hold of the
matter.

Suppose we take it up another way.

For any subject or predicate we can substitute what?

Only that which this subject or predicate represents -- only that
which fulfils the function of that subject or predicate -- only that
which the subject or predicate represents _to the proposition_ or to
the other terms of it.

Now a subject is a direct symbol of _its_ subject to its predicate
and a predicate [is a direct symbol] of its predicate to its subject.

But a subject is also an imperfect representation of that genus from
which it has been taken -- by which it is determined. It is not a
_semeion_ sign of it as I have said -- it is an example of it.

A predicate is a representation of the thing of which it is a random
character -- a copy of it.

This is horribly vague.

# 1867-03-25

Here is another point of view.

What is the function of a symbol as subject? To stand for certain
things. Then if a predicate be true of all the things that it stands
for as yet, that is for all which we yet know it to stand for, the
symbol may stand as subject provisionally.

The difficulty with this is that it does not represent the synthetic
probability of the inference.

It is however a good idea that a random selection is equivalent to all
known -- the genus of those two would fit _that_.

We have `M` is `P` in the sense that

- the _actual_ denotation or things taken under `M` are `P`
  (contingent)

- and 2nd in the sense that all possible things taken under `M` would
  be `P` (necessary)

On the same principle

`S` is `M` in the senses

- 1st that `S` has the qualities taken of `M` (attributive)

- 2nd that `S` has all qualities of `M` (subsumptive)

Still it may be doubted if Hypothesis proceeds by random selection of
qualities of the new predicate.

Then the principle would be

    the possible is like most of the actual.

- **Xie:** The above principle is to understand the following inference again,
  in the 1st senses (contingent and attributive):

  ```
  S : M
  M <: P
  -------
  S : P
  ```

  ```
  S possiblely belongs to M, because it has attributes taken from M.
  M is possiblely a subset of P, because samples taken from M belongs to P
  (we can also say P has most of M's attributes).
  --------------------
  S possiblely belongs to P.
  ```

# 1867-04-01

What is taken -- the present -- of a class if it has any common
character -- that character probably belongs to the class, or to the
majority of it. And if what is known of the characters of a thing
belong to another thing, the second thing has most of the characters
of the first, probably.

- **Xie:** I understand the first sentence as:

  Take samples of `M`, find the common attributes of the samples,
  most of the attributes will belongs to `M`.

  And I understand the second sentence as:

  The `M` is probably a subclass of `P`, if `P` has
  most of the properties [characters] of `M`.

  The second sentence is the principle of subclass (under "probably"),
  the first sentence is its dual.

The reason is that the parts compose the whole and therefore
what does not belong to the majority of the whole
does not belong to the majority of the parts.

- **Xie:** Or say without negation:

  The reason is that the parts compose the whole and therefore
  what belongs to the majority of the parts
  belongs to the majority of the whole.

What does not belong to most of the parts
does not belong to the parts taken mostly,
because the parts to be taken are all the possible parts.

- **Xie:** Or say without negation:

  What belongs to the parts taken mostly belongs to most of the parts,
  because the parts to be taken are all the possible parts.

# 1867-04-12

The distinction must be observed between Induction and Hypothesis
as formal operations and between them as leading to truth.

- **Xie:** I understand "formal operations" as
  building up expressions and statements,
  and I understand "leading to truth" as
  semantics such as checking of judgments
  and evaluation of expressions.

---

# 1867-04-24

Let me consider a little about the nature of truth.

_First_. I notice that if we define an image to be a representation
completely determined in content so that in it every attribute is
affirmed or denied there is probably no image. And is not this what
is requisite to make an image? What is an image? There is a good
question for dialectical research.

- **Xie:** I understand "image" as concept in formal concept analysis,
  and "dialectical research" means to view concept lattice as a reactive system.

  Judgments about truth are our attitudes toward expressions of concepts.

---

As it seems to me that the world has not yet exhausted the instruction
to be derived from Sophisms I shall undertake some analysis of a
collocation of them which seems to me to lead at once to a solution of
the darkest questions of metaphysics.

In the first place what is meant by a hypothetical proposition, when
is it true? Take this one -- If the carotid artery of a man is cut, he
will die. Or this -- if the shadow of the moon is cast on the earth,
there is an eclipse of the sun.

Truth may be defined as the concurrence of the extension and
comprehension of a representation which has extension and
comprehension independent of one another.

- **Xie:** Take Dan's definition of judgment:

  > A Judgment is an attitude that a person takes towards expressions.

  Correspondence:

  - Comprehension -- a person's attitude.
  - A representation which has expression -- expression.

Thus if a representation is a _mere_ likeness (as no human
representations are) which stands for nothing except what it happens
fully to agree with in characters; it cannot be false of any thing
because it only stands for whatever it fully agrees with. And
therefore truth has no meaning in reference to it.

TODO

So if a representation merely points out certain things and implies
nothing of them.

But if a representation at once indicates certain objects and inde-
pendently implies certain characters, its truth or falsity depends on
whether those characters can be predicated of those objects.

This definition is a bad one -- it contains a diallele -- but it will
answer as a preliminary explanation and even sometimes as a test.

[First apply what has been said to a categorical.]

Now in a hypothetical proposition the function of the protasis is to
mark the _sphere_ of the representation, which it may do by means of
its connotation or otherwise. The apodosis on the other hand conveys
the _content_ of the representation. And the question whether the
proposition is true is the same as whether that _content_ belongs in
fact to that sphere. Thus in the proposition -- If the shadow of the
moon is cast on the earth, the sun is eclipsed -- the former clause
indicates the circumstances to which the statement made in the latter
clause is applicable.

Take now another case. If the motion of the earth in its orbit were
suddenly arrested and the perturbative effects of other bodies
prevented, it would fall in a direct line to the sun. Unless the word
truth be taken in a quite improper sense, this proposition is
true. Yet how? For in this case there are no such circumstances as
those indicated, they are even physically impossible, so that this
would seem to be a representation like a copy which [...]

Let `x` be that of which I know absolutely nothing
Then I do know that I know nothing of `x`
Therefore `x` is not `x`.

# 1867-09-26

# 1867-09-27

# 1867-09-28

# 1867-09-28

# 1867-10-02

# 1867-11-24

# 1867-12-07

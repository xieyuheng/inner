---
title: Lecture about Univalent Foundations at the Institut Henri Poincaré
author: Vladimir Voevodsky
date: 2014-04-22
place: Institut Henri Poincaré
video: https://www.youtube.com/watch?v=CJugQ8AGCUo
---

First, let me state the following over simplified, but, I believe,
important for the understanding thesis:

> The main idea of the Univalent Foundations is how to
>   interpret type-theoretic universe mathematically.

In answering this question I was strongly influenced by the
paper of M. Makkai "First Order Logic with Dependent Sorts,
with Applications to Category Theory". There Makkai
discusses What he sees as the future foundations of mathematics
and even give them a name: "Invariant Foundations".

- The paper is specially useful for people
  who are familiar with first order logic
  but not yet experienced user of type systems.

------

He then writes:

> The universe of the Invariant Foundation is not clearly defined yet,
> it should contain ana-n-categories for all natural n's;
> the the totality of ana-n-categories, with their morphisms, etc.,
> will form a ana-n+1-category.

- He basically means the foundations should be infinity-category of infinity-categories.

It is a very natural idea and it took me a lot of effort to understand that
it is **wrong** and that the universe of the new foundations of mathematics
should not be infinity-category of infinity-categories
but instead the infinity-groupoid of infinity-groupoids and their equivalences.

- This is the break through point of the development of Univalent Foundations,
  No, we do not need categories, we only need groupoids.

  Categories are great, but categories are basically
  higher level analogs of partially ordered sets.

  Partially ordered sets are also great,
  but it is the sets that are fundamental,
  while partially ordered sets are just one of the possible structures
  which one can endow sets with.

  The same is true here,
  it is the homotopy types that are fundamental,
  while category is just one possible structure
  which one can endow homotopy types with.

  Groupoids are fundamental because they are homotopy types.

------

Unlike infinity-categories, which we still do not understand well,
the infinity-groupoids are easy to understand
due to the reversal of an idea of Grothendieck:

> ... the intuition appeared that infinity-groupoids should constitute
> particularly adequate models for homotopy types,
> the n-groupoids corresponding to truncated homotopy types\
> (with `pi(i) = 0` for `i > n`).
>
> -- Esquisse d'un Programme 1984

- The program that Grothendieck proposed is not found,
  but it is one of the most fruitful paper over the last 50 years.

- Homotopy types are easy to understand
  because they can be understood by topological spaces.

------

According to this "Grothendieck correspondence",
infinity-groupoids are models of homotopy types.

Hence, there should be a homotopy type corresponding to
the infinity-groupoid of infinity-groupoids and their equivalences.

- The universe we are searching.

What is it?

------

Note that the previous line of reasoning is done
in an inconsistent reasoning system which assumes that
there is such a thing as an infinity-groupoid of **all** infinity-groupoids.

- An inconsistent system is still very valuable for creative reasoning,
  and we can always use a consistent system later in the development.

- Xie: This argument is like the argument against premature optimization in programming.

In a more complex system such as ZFC, which we believe to be consistent,
the reasoning becomes more complex. Instead of talking about
the infinity-groupoid of **all** infinity-groupoids,
we should fix two set-theoretic universe `U0` and `U1`,
such that `U0` is an element of `U1`,
and talk about infinity-groupoids and their equivalences in `U0`
as an infinity-groupoid in `U1`.

This infinity-groupoid should correspond to
a well-defined homotopy type `U = U(U0)` in `U1`.

(We use `U` to denotes the universe we are searching.)

- But this is not a so well-defined homotopy type,
  because `U(U0)` depends on `U0`,
  the universe we constructed is not unique.

Can we construct this homotopy type directly?

------

What else do we know about this `U`?

The main thing we know about `U` is that
there should be a fibration `U' -> U` over `U`
which corresponds to the fibration over the infinity-groupoid of infinity-groupoids
whose fiber over an object is the infinity-groupoid representing this object.

In the simplified reasoning system, where there is a set of all sets,
this would be the universal fibration i.e. the fibration which classifies all fibrations.

- Other fibrations can be induced from the universal fibration in exactly one way.

In a more complex consistent reasoning system with universes,
this fibration should still satisfy
the "uniqueness" part of the definition of "universal"
but not the "for all" part.

Fibrations which satisfy the "for all" part but not the "uniqueness" part
are known in algebraic geometry as "versal fibrations".

- The "versal" means from which everything can be obtained by pullback but not necessarily in one way.

------

I was looking for a word to use for fibrations
which satisfy the "uniqueness" part but not the "for all" part
and decided to call them "univalent".

And this is why the foundations are called "Univalent Foundations".

------

The main ideas of Univalent Foundations
are used to develop a library of formalized mathematics in Coq.

The library we developed is `UniMath`.

- In `UniMath` we only use very conservative subset of Coq,
  no inductive constructions other than the ones
  which are necessary for the standard constructions of Martin-Löf's type theory.

- My goal is to create proof assistants to move mathematics ahead,
  not only to use them to check things that have already been done,
  but to use them as we are inventing things.

But it appears that further progress in the direction
of developing proof assistants for the everyday use by mathematicians
require us to find ways to collaborate with the communities
using other proof assistant architectures.

------

What does one need to have in a type-theoretic proof system
in order to express the main ideas of Univalent Foundations in it?

1. A universe U.
2. Dependent products.
3. Dependent sums.

------

Do we need a sequence of embedded universes?

- Yes -- if we are looking for a consistent system.
- No -- if we do not care about formal consistency.

The second perspective is very important,
because we want to use the ideas of univalence
not only in mathematics
but also in computer science.

- The ideas of univalence is that
  we understand that not everything is set,
  and we understand in which way something is not a set.

  The negative statement "not a set"
  can be turned into positive statements.

This means designing "univalent programming languages".

And most successful programming languages are inconsistent deduction systems,
it is possible to write non-terminating programs in such languages.

------

It is important to be able to experiment with various combinations of univalence with inconsistency.

In addition, building various proofs of inconsistency in such a system
will provides a good testing ground for implementations.

------

Adding a `U : U` universe is only one way in which
inconsistency helps to make things more simple.

Another way is to have inductive types and matching machinery
with weaker than what is necessary for consistency termination rules.

------

- The relation between inconsistency and consistency
  will be very important to help us observing the structure of systems.

------

Univalent Foundations prohibit the use of strict `prop`
(a type of objects whose structure is never used in computation).

It is like strict equality.
Maybe we can find a way to keep it (in the right way).

------

## Q&A

**Martin-Löf**: Didn't you give a very good reason yourself,
against the rule you proposed, which will make the system inconsistent,
and you made further suggestions which seems doubtful to me.

**Voevodsky**: ..., Well, I don't know. I mean, ...
Gödel teaches us we have to make compromises.

- Xie: Questioners totally missing the main ideas of the talk.

**Another Questioner**: ...

**Voevodsky**: My idea here is to be more daring.
Just if we think like that it works.
When I think, I think about set of all sets,
it does not prevent me from coming up with correct ideas (as long as we verify them later).
So why not?

**Voevodsky**: Switching between different modes of proof assistant,
will give me a mechanical way to verify which of the assumptions have been used.

**Voevodsky**: I do not believe in the consistency of Peano arithmetic.
I think it is plain inconsistent. But I am using it all the time.
The important thing is that we do not restrict ourselves, [its] unnecessary.

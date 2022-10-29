---
title: Aristotle's Syllogistic
subtitle: From the standpoint of modern formal log1c
author: Jan Lukasiewicz
years: [1951, 1957]
---

# CHAPTER I ELEMENTS OF THE SYSTEM

## 1. The true form of the Aristotelian syllogism

Aristotle always puts the predicate in the first place
and the subject in the second. He never says "All B is A",
but uses instead the expression "A is predicated of all B"
or more often "A belongs to all B".

```
All B is A
All C is B
-----------
All C is A
```

- **Xie:** This can be understand as subtype relation:

  ```
  B < A
  C < B
  ------
  C < A
  ```

  Note that, in set theory, if B < A,
  there is an inclusion function -- ι (iota) from B to A,
  (a.k.a. insertion, injection).



  ```
  ι(B, A): B -> A
  ι(C, B): C -> B
  ------------------------------------
  (c) => ι(B, A)(ι(C, B)(c)) : C -> A
  ```

Barbara:

```
A is predicated of all B
B is predicated of all C
-------------------------
A is predicated of all C
```

or more often

```
A belongs to all B
B belongs to all C
-------------------
A belongs to all C
```

- **Xie:** From [What does 'of' mean in 'to be predicated of'?](https://ell.stackexchange.com/questions/48769/what-does-of-mean-in-to-be-predicated-of)

  > When we predicate a quality or property of a subject,
  > we are stating that the subject possesses that quality or property.

  See also [Wiktionary](https://en.m.wiktionary.org/wiki/predicate) for it's etymology:

  > Etymology 1
  >
  > From post-classical Late Latin praedicātum (“thing said of a subject”),
  > a noun use of the neuter past participle of praedicō (“I proclaim”).
  >
  > Etymology 2
  >
  > From Latin praedicātus, perfect passive participle of praedicō (“publish, declare, proclaim”),
  > from prae + dicō (“proclaim, dedicate”), related to dīcō (“say, tell”).
  > Doublet of preach.

- **Xie:** We should understand

  ```
  A is predicated of all B
  B is predicated of all C
  -------------------------
  A is predicated of all C
  ```

  as

  ```
  A > B
  B > C
  ------
  A > C
  ```

## 2. Premisses and terms

Every Aristotelian syllogism consists of three propositions called *premisses*.
A premiss is a sentence affirming or denying something of something.
In this sense the conclusion is also a premiss,
because it states something about something.

- **Xie:** Compare the premiss here with Martin Löf's judgment,
  which is defined by Dan as:

  > A judgment is an attitude that a person takes towards expressions.
  > When we come to know something, we are making a judgment.

The two elements involved in a premiss
are its subject and predicate.
Aristotle calls them "terms".

The original meaning of the Greek "term",
as well as of the Latin "terminus", is "limit" or "boundary".

The terms of a premiss, its subject and predicate,
are the limits of the premiss, its beginning and end.
This is the very meaning of the word "term",
and we should be careful not to identify this logical word
with such psychological or metaphysical words as
"idea", "notion", "concept", or "Begriff" in German.

Every premiss is either universal, particular, or indefinite.

- "All" and "no" added to the subject -- universality.
- "some" and "some not" (or "not all") added to the subject -- particularity.
- A premiss without such quantity -- indefinite.

In building up his logic Aristotle did not take notice
either of singular or of empty terms.
In the first chapters of the Prior Analytics,
containing the systematic exposition of his syllogistic,
only universal terms are mentioned.

Aristotle certainly would not accept as meaningful expressions like
"All Calliases are men" or "Some Calliases are men", if there were only one Callias.

- **Xie:** Compare with distinction between element and type in type theory.
  To form A -> B, A and B must be type.

  Maybe we can say, Aristotle talk about types not elements.

## 3. Why singular terms were omitted by Aristotle

Aristotle divides all things into three classes.

1. Some, he says, are such that
   they cannot be predicated truly of anything at all,
   like Cleon and Callias and the individual and sensible,
   but other things may be predicated of them,
   e.g. man or animal.

   - **Xie:** Compare with Dan's definition of type:

     > Expressions that describe other expressions are called types.

2. Some other things,
   and these are the second class,
   are themselves predicated of others
   but nothing prior is predicated of them.
   For this class of things no example is given,
   but it is clear that Aristotle means
   what is most universal, like being.

   - **Xie:** In type theory, the universe of all types is an example.

     Maybe Aristotle knows "type in type" is not good.

3. To the third class belong those things that
   may be predicated of others and others of them,
   e.g. man of Callias and animal of man,
   and as a -- rule, concludes Aristotle,
   arguments and inquiries are concerned
   with this class of things.

   - **Xie:** This means "A is predicated of all B" can be understood as:

     | From  | Meaning              |
     |-------|----------------------|
     | B : A | B is an element of A |
     | B < A | B is a subtype of A  |

     And Aristotle limits the inquiries to B < A.

- **Xie:** Lukasiewicz points out that, the given classification
  is not a division of things but adivision of terms.

  Terms (or say expressions) are our way of modelling (reasoning about) the world.


This is the greatest defect of the Aristotelian logic,
that singular terms and propositions have no place in it.
What was the cause?

There is an opinion among philosophers that
Aristotle constructed his system of logic
under the influence of Plato's philosophy;
for it was Plato who believed that
the object of true knowledge must be stable
and capable of a precise definition,
which is of the universal and not of the singular.

- **Xie:** Compare with modern day category theory.

I [Lukasiewicz] cannot agree with this opinion.

## 4. Variables

```
All S is R
Some S is P
------------
Some R is P
```

- **Xie:** This example makes most sense when viewed in set theory.
  where S, R, P are sets, and "is" is the "belong" in set theory.

  ```
  S < R
  exists (x: S) P
  ---------------
  exists (ι(S, R)(x): R) P
  ```

## 5. Syllogistic necessity

## 6. What is formal logic?

## 7. What is formalism?

# CHAPTER II THESES OF THE SYSTEM

## 8. Theses and rules of inference

THE Aristotelian theory of the syllogism
is a system of true propositions
concerning the constants A, E, I, and O.

- "true propositions" means
  true for all values of the variables.

True propositions of a deductive system I call *theses*.

types of theses :

- implications :
  begin with "if :a then :b"

  1. laws ofconversion
     - and laws of the square of opposition
       not mentionedin the Prior Analytics
     for example :
     "If all B is A, then some A is B."

  2. syllogisms
     of form "if :a and :b, then :c"
     for example :
     "If all B is A,
     and all C is B,
     then all C is A."

- laws of identity :

  1. "A belongsto all A" or "All A is A"

  2. "A belongs to some A" or "Some A is A"

- It must be said emphatically that
  no syllogism is formulated by Aristotle
  as an inference with the word "therefore" (apa),
  as is done in the traditional logic.
  Syllogisms of the form :
  "All B is A; all C is B;
  therefore all C is A"
  are not Aristotelian.

## 9. The syllogistic figures

## 10. The major, middle, and minor terms

## 11. The history of an error

## 12. The order of the premisses

## 13. Errors of some modern commentators

## 14. The four Galenian figures

# CHAPTER III THE SYSTEM

## 15. Perfect and imperfect syllogisms

## 16. The logic of terms and the logic of propositions

## 17. The proofs by conversion

## 18. The proofs by reductio ad impossibile

## 19. The proofs by ecthesis

## 20. The rejected forms

## 21. Some unsolved problems

# CHAPTER IV ARISTOTLE'S SYSTEM IN SYMBOLIC FORM

# CHAPTER V THE PROBLEM OF DECISION

# CHAPTER VI ARISTOTLE'S MODAL LOGIC OF PROPOSITIONS

# CHAPTER VII THE SYSTEM OF MODAL LOGIC

# CHAPTER VIII ARISTOTLE'S MODAL SYLLOGISTIC

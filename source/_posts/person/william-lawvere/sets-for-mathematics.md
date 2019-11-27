---
title: Sets for Mathematics
---

# Sets for Mathematics

------
- Authors: William Lawvere, Robert Rosebrugh
- Date: 2003
------

# 1 Abstract Sets and Mappings

## 1.1 Sets, Mappings, and Composition

## 1.2 Listings, Properties, and Elements

If we are interested in the set `A`,
- we should use maps of type `T -> A` for some auxiliary `T`
  to talk about special subset of `A` (picking up element of `A`),
- we should use maps of type `A -> T` for some auxiliary `T`
  to talk about properties elements of `A` might have (such as true and false)
  or we can view such maps as classification of elements of `A`.

## 1.3 Surjective and Injective Mappings

## 1.4 Associativity and Categories

## 1.5 Separators and the Empty Set

## [Note] The category of sets

- **[Xie]** We know that category theory is specified by a record of axioms,
  this chapter discuss how to add more axioms to this record of axioms,
  to get a complete specification of set theory
  (the word "complete" need further definition).

  - **[AXIOM]** `set_t` is a `category_t`, with sets as objects and maps as morphisms.

  - **[AXIOM]** `set_t` has terminal object, say `unit_t`,
    which can be used to recover the notion of set membership,
    which is called indexing.

  - **[AXIOM]** the terminal object `unit_t` is a separator.

    ``` js
    unit_separate_map : {
      f1, f2 : { X -> Y }
      {
        x : { unit_t -> X }
        ---------
        eqv_t(compose(x, f1), compose(x, f2))
      }
      ---------
      eqv_t(f1, f2)
    }
    ```
  - **[Exercise 1.15]** In the category of abstract sets S,
    any set A with at least one element 1 also a separator.

  - **[AXIOM]** `set_t` has initial object, say `void_t`.

## 1.6 Generalized Elements

To view `unit_t -> A` as element of `A`,
and `T -> A` as generalized element of `A`,
an element that can vary.

In this chapter we are emphasizing the category
in which the sets themselves are constant,
but later we will explicitly describe and construct examples of categories
in which the sets are (even continuously) variable.

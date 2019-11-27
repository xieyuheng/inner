---
title: Sets for Mathematics
---

# Sets for Mathematics

------
- Authors: William Lawvere, Robert Rosebrugh
- Date: 2003
------

# 1 Abstract Sets and Mappings

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
      e : {
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

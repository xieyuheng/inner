# Formal Concept Analysis -- Implementation Note

## Wikipedia

- <https://en.wikipedia.org/wiki/Formal_concept_analysis>

The original motivation of formal concept analysis
was the search for real-world meaning of mathematical order theory.

One such possibility of very general nature is that
data tables can be transformed into algebraic structures called complete lattices,
and that these can be utilized for data visualization and interpretation.

- **[Xie]**
  We need to formalize the structure of complete lattice.
  - <https://en.wikipedia.org/wiki/Complete_lattice>

A data table that represents a relation between objects and attributes,
tabulating pairs of the form "object g has attribute m", is considered as a basic data type.
It is referred to as a formal context.

A formal concept is defined to be a pair (A, B),
where A is a set of objects (called the extent)
and B is a set of attributes (the intent) such that
- the extent A consists of all objects that share the attributes in B, and dually
- the intent B consists of all attributes shared by the objects in A.

The formal concepts of any formal context can be ordered in a hierarchy
called more formally the context's "concept lattice."
The concept lattice can be graphically visualized as a "line diagram",
which then may be helpful for understanding the data.

Often however these lattices get too large for visualization.
Then the mathematical theory of formal concept analysis may be helpful,
e.g., for decomposing the lattice into smaller pieces without information loss,
or for embedding it into another structure which is easier to interpret.

``` typescript
class context_t {
  objects: Array<string>
  attributes: Array<string>
  private incidence: Set<string>
}

// function context_rander_concept_lattice

type entry_t = { [ key: string ]: string }

function context_from_table(
  table: Array<entry_t>,
): context_t

function concept_p(
  ctx: context_t,
  extent: Set<string>,
  intent: Set<string>,
): boolean
```

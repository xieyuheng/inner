---
title: FCA -- Implementation Note
---

``` typescript
class Context {
  objects: Array<string>
  attributes: Array<string>
  incidence: Set<string>
}

// function randerConceptLattice

type Entry = { [ key: string ]: string }
type Table = Array<Entry>

function contextFromTable(table: Table): Context

function isConcept(
  ctx: Context,
  extent: Set<string>,
  intent: Set<string>,
): boolean
```

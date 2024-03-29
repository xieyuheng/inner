# robert lee constable's lecture

- core logic types
- history
- beyond core types
- two way street
  - bring new logic quantifiers from type system to logic
- uniform validity, uniform forall

- A simple proof of halting problem:
  标题：Robert Constable Lecture 2, OPLSS 2015
  链接：https://www.youtube.com/watch?v=OubXOd0Twl4
  时间：30:00

# proof of "not not (P or (not P))"

not not (P or (not P))
not not (P or (P -> contradictory))
not not (P | P -> contradictory)
not (P | P -> contradictory) -> contradictory
((P | P -> contradictory) -> contradictory) -> contradictory
((P | P -> contradictory) -> contradictory) -> P -> (P -> contradictory)
((P | P -> contradictory) -> contradictory) -> P ->       contradictory
(P                        -> contradictory) -> P ->       contradictory

but
1. we do not want to use false -> (P -> false)
2. "not not (P or (not P))" is not so meaningful
   because we can not just view "not A" as "A -> false"
   instead we need to know explicit what contradictory A implies (for example B and not B)

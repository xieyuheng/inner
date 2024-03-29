---
title: Type as Game
---

- type as game
- two players: `Verifier` and `Falsifier`
- object (including function) as winning strategy for `Verifier`
- just like a card game
  - `Falsifier` owns all the `Record` game -- logic conjunction
  - `Verifier` owns all the `Union` game -- logic disjunction
  - function type is called `Pi` (instead of `arrow_t`)
    - in `args` of `Pi` switch the ownership of cards between `Verifier` and `Falsifier`
    - in `ret` of `Pi` the ownership remain the same
- a player's choice is a path to the target position
  - choosing a choice means to resume a paused game
- use normal-play winning rule
  - normal-play is a rule of game,
    which means the first player who used up his/her choices loss the game
- no draw
- the order of play is not strict
  - sometimes we can swap the order of local plays
- different choices have different effect -- monomorphism
- different players have different choices
  - in a specific game, a choice belong to a player,
    given a choice, we know which player is playing the choice.

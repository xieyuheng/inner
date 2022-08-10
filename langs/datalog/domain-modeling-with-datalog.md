# Domain Modeling With Datalog

## Links

- DOMAIN MODELING WITH DATALOG by Norbert Wojtowicz
  - <https://www.youtube.com/watch?v=oo-7mN9WXTw>

## Modeling in General

Stream -- input.
Tree -- UI, DOM tree.
Mesh -- relation.

## Example of Mesh Modeling

Suppose we are building spotify,
we will have three kinds of node:
- Artist
- Listener
- Song

Other things can be modeled as relation between the node above:
- Album(Artist, Song)
- Playlist(Listener, Song)
- PlaylistSubscription(Listener, Playlist)

## Example of Datalog Modeling

Datalog system consists of:
- Entity
- Attribute
- Value
- Time

Suppose we use Datalog to build github.
TODO
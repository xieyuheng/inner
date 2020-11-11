# Class vs Interface

2020-11-11

## Explain our beliefs

Reading "Implementation patterns" by Kent Beck

Which teaches us to think about "why we write programs in our styles?",
and by encouraging us to explain our styles (ways (beliefs)), let us learn from ourselves.

The patterns of explanation of a pattern is the following:
- "I use this pattern as a way of saying ..."
- "This pattern is a way of saying ..."

The term "a way of saying" emphasis communication (communication with my fellow programmers),
which is the first for the three values: communication, simplicity and flexibility.

## About Class vs Interface

- Should I use `class` in ts instead of my current style, which only use `interface`?
  - My constraint is that sometimes I want to separate methods in their own files.
  - Using my style, polymorphism must be done by `interface` holding functions -- like `Parser`.
    - subtype can not override its supertype's fields.
      - we can override supertype's fields in the data's `create` method.

- My model of programming is data and functions, instead of object and messages.
  - object in js is simply used as record data.
  - should we use `type` instead of `interface`? -- Yes

- I use polymorphism as a way of saying,
  "Give me an object have these methods (satisfy these axioms),
  I can have a lots of generic methods implemented for you (generic proofs proven for you)."
  - like typeclass, but explicit.
  - like algebraic structure (or mathematic structure).

# Class vs Interface

2020-11-11

Reading "Implementation patterns" by Kent Beck

- Should I use `class` in ts instead of my current style, which only use `interface`?
  - My constraint is that sometimes I want to separate methods in their own files.
  - Using my style, polymorphism must be done by `interface` holding functions -- like `Parser`.
    - subtype can not override its supertype's fields.
      - we can override supertype's fields in the data's `create` method.

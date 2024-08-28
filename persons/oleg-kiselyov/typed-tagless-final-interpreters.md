# Typed Tagless Final Interpreters

An idea by Oleg Kiselyov.

Instead of using algebraic data type
to encode (initial encoding) target language's expression.

Instead using native language's type system and type class
to encode (finial encoding) target language's expression.

- "tagless" means no runtime overhead.

- Initial and Finial are dual to each other
  in the sense of category theory.

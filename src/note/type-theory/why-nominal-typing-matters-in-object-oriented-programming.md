# Why Nominal-Typing Matters in Object-Oriented Programming

- Xie:
  If all nominal typing has, but structural typing hasn't, is class name,
  we can do this in structural typing with dependent types.

## 3 Nominally-Typed OOP versus Structurally-Typed OOP

## 3.1 Contracts, Nominality and The Liskov Substitution Principle (LSP)

From the presented examples, it is easy to see
that a contract is made of two parts: requirements
upon the caller ("the client") made by the class ("the
provider") and promises made by the class to the
caller. If the caller fulfills the requirements, then the
class promises to deliver some well-defined service.
Requirements may be enforced by throwing checked
or unchecked exceptions when the stated conditions
are violated. Promises can be enforced by assertions
at the end of a method.

- Xie:
  This problem can also be solved at compile time by dependent types.

In practice however, the inclusion of behavioral
contracts in object types is too much to ask of a type
checker (because of the general problem of not being
able to statically check contracts, since behavioral
contracts are remarkably expressive). The solution
OO language designers choose is to go with an ap-
proximation.

The association of class names with
contracts, and OO type systems respecting nominal
information in typing and subtyping decisions, allows
a nominally-typed OO type system to be a tractable
approximation of DbC (design by contract);
hence, OO language designers of many
mainstream OO languages use nominal typing.

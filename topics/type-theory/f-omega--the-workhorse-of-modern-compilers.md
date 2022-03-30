---
title: F-omega -- the workhorse of modern compilers
author: Greg Morrisett
year: 2005
---

[ [ARCHIVE](https://web.archive.org/web/20140917015759/http://www.eecs.harvard.edu/~greg/cs256sp2005/lec16.txt)
| [SYLLABUS](https://web.archive.org/web/20150405073546/http://www.eecs.harvard.edu/~greg/cs256sp2005/) ]

Before growing the langauge with more "realistic" features, such as
say recursive functions, references, etc. it's worth noting that we
can expand the type-level of the language by providing support for
abstraction at the type level.  In particular, it is somewhat natural
to introduce functions at the type level (and applications.)  For
instance, in ML, a definition like:

```haskell
datatype 'a Exn = Fail | Succeed of 'a
```

is really defining Exn as a type-level function which, when applied to
a type, yields a type.  That is, Exn is not a type, but T Exn is a
type for any type T.  (It's just unfortunate that in ML, application
of type constructors is backwards.)

So we might augment our type-level with lambdas and applications.  Of
course, as soon as we do this, then we need to introduce some "types"
for the types to ensure that we only apply type-level functions to
arguments of the right "type".  The classifiers for types are called
*kinds* and we might as well take this as our definition for kinds:

```haskell
(kinds)   k ::= *               (the kind of types)
              | k1 -> k2        (a function kind)
```

Then we can define constructors as follows (we don't call them types
anymore because only some of the constructors correspond to types):

```haskell
(primitive
 constructors)   C ::= -> | x | 1 | + | 0 | All_k | Exists_k

(constructors)   t ::= C | a | \a:k.t | t1 t2
```

Notice that the constructor language is actually pretty small and
corresponds exactly to the simply-typed lambda calculus plus a few
constants.  Factoring the type system in this fashion is a great idea
for implementors because you can keep the type system relatively small
and compact.

The constants have kinds as follows:

```haskell
->       : * -> * -> *
x        : * -> * -> *
1        : *
+        : * -> * -> *
0        : *
All_k    : (k -> *) -> *
Exists_k : (k -> *) -> *
```

and of course we can give kinding rules to the constructor language as
follows, where D is a kind context mapping type variables to kinds.

```haskell
D |- a : D(a)

D,a:k1 |- t : k2
-----------------------
D |- \a:k1.t : k1 -> k2

D |- t1 : k'->k  D |- t2 : k'
-----------------------------
     D |- t1 t2 : k
```

So for instance, we can write the Exn constructor as follows:

```haskell
\a:*.(+ 1) a
```

and verify that it has kind * -> *.  That is, it takes a type and
delivers a type.  The System-F type "All a.a -> a" can be written
as follows:

```haskell
All_* (\a:*.(-> a) a)
```

That is, the All_* constructor takes as an argument a (type-to-type)
function and yields a type.  The same is true for the Exists
constructor.  Notice that this very nicely allows us to have
exactly one form of binding for type variables in the language
and to treat issues such as alpha-variance and capture-avoiding
substitution uniformly.

The term language looks exactly as before except that we'll support
abstraction over *any* kind of constructor, not just constructors of
kind * (i.e., types):

```haskell
e ::= c | x | \x:t.e | e1 e2 | 1 | (e1,e2) | #i e |
      inl_t e | inr_t e | (case e of inl(x)=>e1 | inr(x)=>e2) |
      /\t:k.e | e t | pack[t,e] as t' | unpack [a,x] = e1 in e2
```

The typing rules for terms is also relatively straightforward and
I'll only highlight a few of them:

```haskell
D |- t1 : *   D;G,x:t1 |- e : t2
--------------------------------
D;G |- \x:t1.e : -> t1 t2
```

Notice that we require that t1 is actually a well-formed type.
Before, we only had to check that the free type variables were
in scope, but now we have the possiblity that t1 has the wrong
kind (e.g., it could be of kind *->* instead of *.)

For type abstraction, we have:

```haskell
D,a:k;G |- e : t
-------------------------------
D;G |- /\a:k.e : All_k (\a:k.t)
```

and for type application we have:

```haskell
D;G |- e : All_k t1    D |- t2 : k
-----------------------------------
D;G |- e t2 : t1 t2
```

Notice that t1 might not be a lambda.  It could be a variable or an
application instead.  However, we know that *semantically* t1 must be
a function from kind k to * (i.e., types).  So we simply apply t1 to
t2 to get out that type.

The typing rules for existentials are similar:

```haskell
D |- t1 : k->*   D;G |- e : t1 t2
---------------------------------
D;G |- pack[t2,e] : Exists_k t1

D;G |- e1 : Exists_k t1   D,a:k;G,x:t1 a |- e2 : t2     D |- t2 : *
-------------------------------------------------------------------
D;G |- unpack [a,x] = e1 in e2 : t2
```

The first rule is somewhat subtle -- the way to think of it is that if
e has type T[t2] where T is some type with holes in it that have been
plugged by t2, then we can rewrite the type of e as (\a:k.T[a]) t2 and
note that this will reduce to T[t2].

The second rule allows us to open up an existential and introduces a
type variable with the abstracted kind, as well as a term variable
which is given the type t1 a (think T[a]) while running the expression
e2.  Note that the return type (t2) cannot have a free occurrence of a
in it.

There's one more rule to add, and this is one is crucial or we might
never be able to use All or Exist-types:

```haskell
D;G |- e : t1     D |- t1 == t2 : *
-----------------------------------
       D;G |- e : t2
```

This is a rule of "definitional equality" which says that if we can
prove that two types t1 and t2 are equal, and if we can assign e the
type t1, then we can also assign e the type t2.  This is a lot like
the subsumption rule in a subtype-based system.

And of course, we now need to say what constitutes equality for types.
In particular, we can use the equational theory that we generated for
the simply-typed lambda calculus!

```haskell
           D |- t : k
(refl)   ----------------
         D |- t == t : k

         D |- t1 == t2 : k
(symm)   -----------------
         D |- t2 == t1 : k

         D |- t1 == t2 : k    D |- t2 == t3 : k
(tran)   ---------------------------------------
                D |- t1 == t3 : k

         D |- t1 == t1' : k'->k  D |- t2 == t2' : k'
(app)    -------------------------------------------
               D |- t1 t2 == t1' t2' : k

              D,a:k1 |- t1 == t2 : k2
(lam)    ----------------------------------
         D |- \a:k1.t1 == \a:k2.t2 : k1->k2

         D |- (\a:k'.t1) : k'->k    D |- t2 : k'
(beta)   ---------------------------------------
           D |- (\a:k'.t1) t2 = t1[t2/a]

            D |- t1 : k1->k2
(eta)    ----------------------
         D |- (\a:k1.t1 a) = t1
```

In practical terms, the way you implement a type-checker (not type
inferencer!) for the language is to normalize types at all points and
then compare then up to alpha-equivalence.  It's often useful to use a
"variable-less" representation (say deBruijn indices) so that the
comparison devolves to just a structural comparison.  It's actually
a little trickier than this because we need to eliminate that pesky
definitional equality (in the same way that we eliminated subtyping)
because it can be applied anywhere.  So formally, we need to define
an "algorithmic" set of typing rules where we bake definitional
equality into the rules and only use normal forms.  For instance,
we might have the following for the application rule:

```haskell
D;G |- e1 : t1    D;G |- e2 : t2
D |- NF(e1) = -> ta tb
D |- NF(e2) = tc
ta =alpha= tc
--------------------------------
D;G |- e1 e2 : tb
```

and of course, we're obligated to show that the algorithmic rules
are both sound and complete with respect to the more declarative
rules given above.  But none of that is very hard since we've really
worked out all of the theory for doing this already.

This language is a variant of what is called F-omega and is quite
close to the internal representation of a number of type-preserving
compilers (including GHC, TIL and TILT.)  At this point, GHC has
exported most of the functionality to the source level, but for
the sake of inference, has a number of restrictions on types.
(See the papers by Mark Shields and Simon Peyton Jones for details
on the inference algorithm.)

In my experience, F-omega is an extremely powerful and useful
foundation for programming languages.  Advanced features, such as
Haskell's type classes, (the key parts of) SML and O'caml's module
systems, and even cutting edge features such as GADT's can be encoded
directly into this language.  And, as we'll see, it's crucial for
understanding type systems for OO languages.  Semantically, F-omega is
not that much harder to handle than System-F.  Again, life is much
easier if you stratify it so that it's predicative, but it's
relatively easy to construct a semantic model similar to the one
that we did for system F, even for the impredicative case.

On the surface, it may not appear all that useful to be able to
abstract type constructors, but let me assure you, you can do some
damned cool things with this...

To give you a feel, I would like to informally show you how both
ML modules and Haskell-style type-classes can be encoded in F-omega.

First, let's consider what is a module?  If we ignore types, then
a module is just a record of values.  For instance, the structure

```sml
struct
  val origin = (0,0)
  fun bumpx (x,y) = (x+1,y)
  fun bumpy (x,y) = (x,y+1)
  fun dist((x1,y1),(x2,y2)) = ...
end
```

is really just a record of values:

```sml
{origin = (0,0), bumpx = ..., bumpy = ..., dist = ...|}
```

with type:

```sml
{origin:int*int, bumpx:int*int->int*int, ...}
```

and as we argued earlier, we can think of type abstraction as an
application of existentials.  For instance, if we want to introduce
an abstract type of points:

```sml
struct
  type point = int*int
  fun bumpx (x,y) = ...
  ....
  fun dist (...) = ...
end
```

and "seal" the structure, we can express this in System-F as:

```sml
Exists a.
  {origin:a, bumpx:a->a, bumpy:a->a, dist:a*a->int}
```

Now if we want to abstract some kind of container, such as a stack
or a queue, then we want a signature that looks like this:

```sml
Exists C:*->*.
  {empty:   All a:*.C a,
   insert:  All a:*.a -> C a -> C a,
   remove:  All a:*.C a -> 1 + (a * C a)}
```

Notice that here, we want to abstract a *function* from types
to types not just a type.  So the ability to abstract constructors
is generally crucial for building polymorphic containers.

What about functors?  Consider the following ML functor which
is intended to turn a generic functional container (with a signature
similar to the one above) into one that is imperative:

```sml
signature Container =
sig
  type C : * -> *
  val empty : All a:*.C a
  val insert: All a:*.a -> C a -> C a
  val remove: All a:*.C a -> 1 + (a * C a)
end

signature MutableContainer =
sig
  type M : * -> *
  val empty : All a:*.1 -> M a
  val insert: All a:*.a -> M a -> 1
  val remove: All a:*.M a -> 1 + a
end


functor Mutable(X : Container):> MutableContainer =
struct
  type M = \a:*.ref (X.C a)
  val empty = /\a:*.\x:1.newref (X.empty())
  val insert = /\a:*.\x:a.\m:M a.m := (X.insert x !m)
  val remove = /\a:*.\m:M a.
                 case (X.remove !m) of
                   inl(_) => inl()
                 | inr(v,c) => (m := c; v)
end
```

We can define the Mutable functor as a function in F-omega (suitably
extended with refs and so forth) which takes an
existentially-quantified Container and returns and existentially
quantified mutable container.  I'll leave that as an exercise.
The advantage of doing things this way is that modules (existentials)
and functors (just normal functions!) become first class.

There are disadvantages to encoding modules in this fashion which
we'll hopefully talk about later.  Still, I hope that I'm giving
you the impression that the bulk of ML's module system can actually
be represented faithfully within F-omega.

What about a type-class?  Consider the following class definition:

```haskell
class Show a
  method show :: a -> string
```

In Haskell, when you use a method, such as show above, the type
inference imposes a constraint that there must be an instance of the
show method defined for the type at which you use the method.  For
instance, if I write:

```haskell
show true
```

then this generates a requirement that the boolean type (the type of
true) has a show method.  What about a polymorphic thing like:

```haskell
\a -> \b -> "(" ++ show a ++ "," ++ show b ++ ")"
```

The type generated for such a function is:

```haskell
forall a,b.Show a, Show b => a -> b -> string
```

That is, any caller of the function has to ensure that the constraints
are met (i.e., a and b are types that implement the show method.)
The way GHC implements such a function is to compile it to something
like this:

```haskell
forall a,b.Show a -> Show b -> a -> b -> string
```

where the constructor Show a is defined as:

```haskell
Show a = {show : a -> string,...}
```

In general, a type-class (such as Show) is represented by a dictionary
of methods representing evidence that the type is in the class.  This
sort of "evidence passing" interpretation is actually quite similar to
what happens with a coercion-based interpretation of subtyping.

Now consider that Haskell's treatment of effects depends crucially upon
monads and that monads are such a useful structuring mechanism, we'd
really like to define a class for Monads once and for all.  But,
the signature for a monad shows that Monads are in general type
constructors, not types.  That is, we want "constructor" classes,
not just type classes.  In particular, we'd like to define:

```haskell
class Monad (M:*->*)
  return :: All a:*.a -> M a
  bind   :: All a,b:*.M a -> (a -> M b) -> M b
```

In short, I would claim that the ability to abstract constructors,
not just types, is crucial for building re-usable containers and
abstractions.  By using F-omega as a foundation, we get a substrate
that is relatively simple and well-understood, but surprisingly
powerful.

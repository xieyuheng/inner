---
title: How I implement SSA form
author: Filip Pizlo
source: https://gist.github.com/pizlonator/cf1e72b8600b1437dda8153ea3fdb963
---

This document explains how I would implement an SSA-based compiler if
I was writing one today.

This document is intentionally opinionated. It just tells you how I
would do it. This document is intended for anyone who has read about
SSA and understands the concept, but is confused about how exactly to
put it into practice. If you're that person, then I'm here to show you
*a* way to do it that works well for me. If you're looking for a
review of other ways to do it, I recommend [this
post](https://bernsteinbear.com/blog/ssa/).

My approach works well when implementing the compiler in any language
that easily permits cyclic mutable data structures. I know from
experience that it'll work great in C++, C#, or Java. The memory
management of this approach is simple (and I'll explain it), so you
won't have to stress about use after frees.

I like my approach because it leads to an ergonomic API by minimizing
the amount of special cases you have to worry about. Most of the
compiler is analyses and transformations over the IR. If the IR's API
is great, then the compiler passes are easy to write, succinct, and
read almost like the pseudocode in a compiler textbook. This speeds
maintenance of the compiler, and makes it cheap (in terms of labor) to
add optimizations. This then lets you add lots of optimizations
quickly, which leads to the compiler having lots of optimizations,
which then leads to great throughput and happy users.

The key ideas:

- **Instruction-value uniformity**

  Each instruction is a value, and each
  value is an instruction. Phi is an instruction. Even a constant is
  an instruction. Let's call it an `Inst` for the purpose of this
  doc. SSA data flow is represented by each `Inst` having an `args`
  vector, which is of type `vector<Inst*>`. So, each data flow edge is
  represented as a pointer from the user `Inst` (where the data flows
  to) to the `Inst` being used (where the data flows from).

- **Phi/Upsilon form**

  [Described here](https://gist.github.com/pizlonator/79b0aa601912ff1a0eb1cb9253f5e98d).

  This decouples the CFG from the SSA form, which makes all CFG
  transforms easier to write. It doesn't inhibit the implementation of
  any SSA optimization that I know of (i.e. it's exactly as powerful
  as SSA). It's also easy to compile out of (if you want to exit SSA).

- **Arrays**

  Each function body is just `vector<BasicBlock*>`. Each basic block
  contains its instructions by just having a `vector<Inst*>`. Each
  basic block knows its index in the function body vector. Each `Inst`
  knows its index in its basic block's `Inst` vector. I'll explain how
  to make this fast and ergonomic.

- **Absence of use lists**

  I find it easier to build the IR, and not any harder to use it, if
  there are no edges from `Inst`s to the `Inst`s that use them.

- **Uniform effect representation**

  Each `Inst` can produce for you a description of its effects.
  If an `Inst` reports no effects, then this means that:

  - it can be evaluated at any point in the program where its `args`
    are available,
  - can be evaluated repeatedly,
  - and doesn't have to evaluated at all if nobody uses its result.

  The effect specification is in terms of _abstract heaps_ written or
  read. Instructions that don't write any abstract heaps, and whose
  result are unused, can be deleted. There's no other way to say
  "don't kill this instruction" or "don't reoder this instruction in a
  way that would break". So, phases that want to consider the
  interferences between instructions don't have to special case
  anything to be effective; they just query the instruction effects.

# Syntax for SSA

Here's an `Inst`:

    Int32 A = Add(B, C, foo.hipsterlang:666)

Each `Inst` has a:

- Type (`Int32` in this example).

- Name (`A` in this example). Could be a monotonically increasing
  integer, or a string, or whatever makes you happy.

- Opcode (`Add` in this example).

- Arguments, represented as `vector<Inst*>` (`B, C` in this example -
  so the `B` and the `C` are really pointers to `Inst`s called `B` and
  `C`).

- An origin. At a minimum, this can store a stack of
  filename:linenumber pairs. This is optional in the sense that some
  instructions might not have an origin. (`foo.hipsterlang:666` in
  this example.)

`Inst` forms a class hierarchy, because some `Inst`s will want more
data. For example, all literals/constants/immediates are represented
by instructions that return them, like:

    Int32 One = Const(1)
    Int32 X = Sub(A, One)

Here, the `1` argument to Const is not part of the Inst's
arguments. Instead, there a `ConstInst` subclass of `Inst` that has an
extra field that holds the immediate.

The fact that constants are `Inst`s reduces the number of special
cases in the compiler. For example, consider that you might want to do
a transformation where every value is filtered through the `Wombat`
opcode - so `Foo(Bar(42))` would become `Wombat(Foo(Wombat(Bar(Wombat(42)))))`.
If constants were not `Inst`s, then you'd have to write a special case
for filtering the constants. But since constants are just `Inst`s in
my representation, you'd just insert a `Wombat` after each `Inst`.
Also, if you wanted to analyze what are all of the values that might
be used by a function, this representation just requires you to loop
over `Inst`s in the function rather than also considering some objects
on the side (like constant values). There is no cost to doing this,
since `Const` has no effects and no `Inst` inputs, so repeated uses of
`Const(1)` can be combined into one and hoisted by the common
subexpression elimination and loop invariant code motion passes that
you'll be writing anyway. And, since it's SSA, you can always pattern
match constants flowing into instructions, like:

    if (inst->opcode() == Add &&
        inst->args(1)->opcode() == Const &&
        inst->args(1)->as<ConstInst>()->value() == 0)
        inst->replaceWithIdentity(inst->args(0));

I like to give myself helpers for this in `Inst` so I can even do:

    if (inst->opcode() == Add && inst->args(1)->isConst(0))
        inst->replaceWithIdentity(inst->args(0));

You'll want some kind of richer types, including probably tuples and
vectors. It's OK to use `IntPtr` as your pointer type, if you're
compiling something like C, or if you're at the stage of a high-level
language compiler pipeline where you understand the IR using low-level
concepts like pointers.

Each function is just basic blocks that can branch to one another
however they like. Logically, the successors of a basic block are
arguments to the terminal instruction in that block, but I like to
store them in the `BasicBlock` itself. Here's an example function.

    root:
        Int32 A = GetArgument(0)
        Int32 B = GetArgument(1)
        Void B = Branch(A)
      successors: then, else
    then:
        Int32 One = Const(1)
        Int32 X = Add(B, One)
        Void R1 = Return(X)
    else:
        Int32 Two = Const(2)
        Int32 Y = Mul(B, Two)
        Void R2 = Return(Y)

Note that even instructions that don't return a value have a name and
their type is Void. This makes debugging the compiler easier and
removes special-cases.

Also note that even function arguments are represented as their own
`Inst`s (the GetArgument instructions in the example), to preserve the
instruction-value uniformity. The GetArgument instruction is pure like
Const, so repeated uses are free (they will be combined by common
subexpression elimination and hoisted by loop invariant code motion).

I allow unreachable basic blocks and unstructured control flow.

I wouldn't give each basic block a list of predecessors, but I would
have a utility function that returns a map from `BasicBlock*` to its
predecessors. I.e. it returns something like a `map<BasicBlock*,
set<BasicBlock*>>`. This is cheap to compute because you'll do it once
per compiler pass (in the worst case - some passes won't need it) and
the rest of the compiler pass is most likely considering every
instruction. There are many more instructions than blocks, so
generally, algorithms that are `O(number of blocks)` don't show up as
bottlenecks.

It's important to have a `validate` pass that you can run in between
compiler passes that checks that the IR is following the rules (like,
if `A` is used by `B` then `A` must dominate `B`).

# Phi/Upsilon Form

[Phi/Upsilon form is also described as "Pizlo form" in this
document.](https://gist.githubusercontent.com/pizlonator/79b0aa601912ff1a0eb1cb9253f5e98d/raw/050e878649923d44ccc766929d2473f85c2b6332/pizlossa.md)

Phi/Upsilon form is a way to implement SSA's Phi without introducing
any concepts that make `Inst`s more complicated. The Phi is split into
two parts, that I call Phi and Upsilon. Phi and Upsilon instruction
opcodes minimize the amount of special casing in the compiler's guts.
You only have to worry about the special properties of Phi and Upsilon
when doing optimizations specifically to the Phis and Upsilons, or
when doing data flow analysis. "Not special casing" means for example
that a Phi or Upsilon is allowed to appear anywhere in the basic
block's array of `Inst`s. Another example of "not special casing" is
that the Phi `Inst` doesn't point to any basic blocks, and doesn't
have any special kinds of use edges. One big implication is that
control flow transforms don't have to do anything about Phi or Upsilon
(they are as simple to write as control flow transforms outside SSA).

Normal SSA means that each `Inst` has an implicit variable associated
with it, and when the `Inst` executes, it assigns the result to that
variable. Then all of the places where another `Inst` points at our
`Inst` are just reads from that variable. Those semantics are
important to keep in your head when writing compiler code in SSA. This
is the key place where Phi/Upsilon adds a special case: a Phi
instruction has a second variable associated with it, called the
shadow variable. You'll have to model that shadow variable in any data
flow analysis of Phi/Upsilon SSA form.

Upsilon is written like this:

    Upsilon(SomeValue, ^SomePhi)

Note that the ^SomePhi "argument" is not part of the `Inst`'s
`vector<Inst*>` arguments; it's a special field in `UpsilonInst`.
This is important, because it should not be analyzed as a data flow
use. Phi is written like this:

    SomePhi = Phi()

Upsilon writes `SomeValue` to the shadow variable of `SomePhi`. Phi
reads and returns the current value of its shadow variable. Because
the only way to read a shadow variable is to execute that shadow
variable's Phi, the Phi-Upsilon relationship follows Static Single Use
form - a kind of upside-down SSA.

Let's consider this example program.

    root:
        Int32 A = GetArgument(0)
        Int32 B = GetArgument(1)
        Void B = Branch(A)
      successors: then, else
    then:
        Int32 One = Const(1)
        Int32 X1 = Add(B, One)
        Void U1 = Upsilon(X1, ^X)
        Void J1 = Jump()
      successors: return
    else:
        Int32 Two = Const(2)
        Int32 X2 = Mul(B, Two)
        Void U2 = Upsilon(X2, ^X)
        Void J2 = Jump()
      successors: return
    return:
        Int32 X = Phi()
        Int32 FortyTwo = Const(42)
        Int32 RS = Add(X, FortyTwo)
        Void RT = Return(RS)

This IR is equivalent to the following C function.

    int foo(int a, int b)
    {
        int x;
        if (a)
            x = b + 1
        else
            x = b * 2
        return x + 42;
    }

I describe Upsilon and Phi as having effects. I'll also tell you even
more about this in the next section. The effects are needed to ensure
that we don't reorder or remove Upsilons and Phis incorrectly.

- Upsilons cannot be reordered arbitrarily. Since an Upsilon is an
  assignment, reordering two Upsilons that refer to the same Phi would
  change the program's meaning.

- Phis cannot be reordered with Upsilons, since the Phis read the
  value written by the Upsilon.

The easiest way to achieve this is to invent an abstract heap called
SSAState and have Upsilon report that it writes to the heap as part of
its effects, and have Phi report that it reads from the heap as part
of its effects. You could make this more precise by giving a separate
abstract heap for each Phi.

The main benefit of this form is that basic blocks - and all CFG data
structures - have zero knowledge about SSA. There are no basic block
arguments. There's no requirement that Phis appear at the tops of
blocks. In fact, this is a valid program in Pizlo form (albeit
suboptimal):

    Int32 M = Stuff(...)
    Void U = Upsilon(M, ^N)
    SomeType W = Whatever()
    Int32 N = Phi()
    SomeType X = MoreStuff(N)

Here, there's a Phi in them middle of a basic block, and there's an
Upsilon before it with some stuff in the middle. That's fine. This is
important, because it means that you can do CFG transforms that blow
away control flow edges without worrying about fixing your Phis.

In any SSA compiler, you'll want simplifications for the SSA
operations like Phi. In Phi/Upsilon form the main thing you'll want is
a fixpoint rule that if you have a Phi that has Upsilons that only use
the Phi or exactly one other value, then replace all uses of the Phi
with uses of the other value. You'll also want aggressive dead code
elimination to only flag Upsilons as used if their Phis are used. The
combination of these optimizations will simplify the previous example
to:

    Int32 M = Stuff(...)
    SomeType W = Whatever()
    SomeType X = MoreStuff(M)

Doing such optimizations means you'll want to have a utility pass that
gives you the "Phi arguments", i.e. the set of Upsilons that store to
the Phi. You'll use this anytime you want to write optimizations that
pattern-match over the Phis. Computing the Phi arguments is O(N) (you
have to walk the whole program), but that's fine since iterating the
IR is cheap (it's just arrays!) and passes are O(N) anyway.

# Arrays

At first glance, it feels most natural to represent basic blocks as
linked lists of instructions, since this allows O(1) insertion and
removal. But linked lists are annoying to get right and the work
required to maintain them properly is not worth it compared to the
array approach that I use. One reason for this is that to track
dominance between instructions in the same block, you need to know
each instruction's index in the block anyway - so even if you use
linked lists, you'll need some kind of O(N) index updating
pass. Another reason is that in practice, implementing a linked list
is harder than just having a `vector<Inst*>`. Finally, linked lists
are more expensive to traverse than arrays - and compilers like to
traverse all instructions a lot.

One of the subtle reasons why I use arrays rather than linked lists
comes down to subjective feelings based on experience: I've worked on
multiple compiler IRs that use arrays (Bartok, DFG, [B3,
Air](https://webkit.org/blog/5852/introducing-the-b3-jit-compiler/); I
designed three of those) and multiple compiler IRs that use linked
lists (Fiji C1, LLVM; I designed Fiji and I [do a
lot](https://www.unrealengine.com/en-US/tech-blog/bringing-verse-transactional-memory-semantics-to-c)
[of LLVM
hacking](https://github.com/pizlonator/llvm-project-deluge/blob/deluge/Manifesto.md))
and I *feel more productive* in the compilers that use arrays. It's
not a huge productivity advantage - definitely less than 2x; I also
feel productive with linked lists (like LLVM), so it's not an enormous
preference.

The biggest reason to use arrays if you're implementing your own IR
from scratch is just that the array approach is something like 5x
easier to implement. You'll get a lot of mileage out of implementing
the array approach using some `vector<>` template, maybe even one in
the STL. On the other hand, the linked list approach is really about
hacking up custom linked lists (each `Inst` is a linked list node in
that world) and so you'll have to get each of the classic linked list
algorithms exactly right. That takes way more time than using
`vector<Inst*>`, even if you include the things I recommend
implementing in this section. So if you want to go zero-to-IR in as
little time as possible, just use arrays.

The fact that everything is stored in arrays seems to present a
problem for transformation: how do you insert? How do you remove?

If we used arrays naively, then array insertions would be O(N), and so
passes would be O(N^2). But there's a trick for avoiding this. The key
insight is that passes are going to loop over basic blocks, and then
the instructions in those basic blocks, pretty much no matter
what. Usually it's because the pass wants to find instructions of
interest to it. So, we amortize all insertions and removals behind
that existing O(N) pass over the function.

When looping over a basic block (almost always in ascending order), we
maintain an `InsertionSet`, which is just a `vector<tuple<size_t,
Inst*>>` internally, where the `size_t` is the index where we want to
perform an insertion. As we loop over the block, if we decide to
perform insertions, we append to the insertion set. We might insert
multiple things at the same index. Then, after we are done looping
over the basic block, we "execute" the insertion set on the block,
which performs a single pass over the basic block's instruction vector
to perform all of the insertions while fixing up the indices of the
instructions.

This idiom ends up being what a lot of passes look like:

    InsertionSet insertionSet;
    for (BasicBlock* block : function->blocks()) {
        for (Inst* inst : block->insts()) {
            switch (inst->opcode()) {
            case Opcode::Foo:
                // This calls the BarInst::BarInst constructor with inst->type(), Opcode::Bar,
                // inst->arg(0), and inst->origin(). Then it appends the new BarInst to the
                // insertionSet with inst->index() as the insertion index. Finally, it returns the
                // new BarInst.
                inst->arg(0) = insertionSet.insertBefore<BarInst>(
                    inst, inst->type(), Opcode::Bar, inst->arg(0), inst->origin());
                break;
            default:
                break;
            }
        }
        // This performs all of the insertions, fixes the indices (so assuming there was one Foo, that
        // one Foo's index, and the indices of all subsequent instructions, would be one greater than
        // before; and the newly inserted BarInst would get the Foo's old index), and clears the
        // InsertionSet.
        insertionSet.execute(block);
    }

This sample transformation replaces every Foo(X) with Foo(Bar(X)),
inserting the Bar instruction just before the Foo instruction. Note
that the `insertBefore` method creates a new BarInst object, passes
the `inst->type(), Opcode::Bar, inst->arg(0), inst->origin()`
arguments to its constructor, and appends the BarInst object to the
insertionSet at `inst`'s index.

I like to give InsertionSet additional powers, like detecting if
instructions are inserted out-of-order and then stable-sorting the
vector in `execute`. This permits this idiom to work (albeit at some
perf cost) even if you don't walk the basic block in forward
order. Also, passes that need to be able to insert instructions
anywhere in the function (not just the block they are walking) can do
that by creating a `map<BasicBlock*, InsertionSet>`, and then
executing all of the insertion sets after all the work is done.

On the other hand, removal of instructions is just a matter of
replacing the instruction with a Nop. You can have InsertionSet remove
Nops automatically, or you can have Nop removal be part of dead code
elimination. It's no big deal if Nops pile up during optimization, so
long as you run something to remove them eventually. The important
thing is that you want the lifetime management aspect of instruction
removal to be confined to one place - whatever does the Nop removal -
to keep the rest of the compiler simple. That Nop removal pass is
where you'll handle deallocating instructions (if you're in C++),
compacting the basic block vectors, and fixing the indices.

Having insertion and removal deferred has the nice property that IR
iterators don't get invalidated. If we used linked lists and had
insertions/removals happen eagerly, then we might invalidate whatever
iterator we're using to loop over a basic block because we had killed
off the instruction that the iterator was on. Having the API for
insertion/removal use an explicit deferral means that iterator
invalidation is easy to avoid.

I've shown InsertionSet working for the vector of instructions in a
basic block, but you'll also end up making it work for basic blocks in
a function. I usually have a BlockInsertionSet and an InsertionSet,
and they are wrappers around some generic insertion set algorithm that
works just as well for `vector<Inst*>` as for `vector<BasicBlock*>`.

Finally, the pervasive use of arrays and the fact that every major IR
element (basic blocks, instructions) has an index means that you can
have map and set data structures specialize for basic block and
instruction keys. If you use a basic block as a key in a map, then the
map can just have an array (rather than a tree or hashtable) that uses
the block index as the array index. If you use an instruction as a
key, then you can use a two-level array (first level uses the
instruction's basic block's index, second level uses the instruction's
index in the block).

# Absence of Use Lists

The main reason why a compiler would want to know who uses an `Inst`
is when replacing all uses of an `Inst` with another `Inst`. But this
can be achieved by simply giving each `Inst` the power to transform
itself into an Identity in-place.

Then, I just have a utility pass available that short-circuits all
uses of Identity(X) to uses of X.

This approach greatly reduces the work required to build the IR itself
and leads to a more compact in-memory representation, which then
reduces pressure to optimize compile times. How much memory it takes
to represent a program affects the compiler's cache locality and also
establishes a bound on how big of a program the compiler can handle at
all. Not having use lists improves cache locality and allows the
compiler to handle larger programs. It also means that naive
implementations of compiler passes are more likely to have acceptable
running time, which then leads to less time spent optimizing compiler
passes for compile time.

Note that the in-place replacement idiom works best if the instruction
object *becomes* an Identity instruction object while keeping the same
address, so all pointers to it continue to work. I've found that
replacing with Jump, replacing with Nop, replacing with Phi, and
replacing with Oops (what I call my "unreachable" terminal opcode) are
also useful. You can do this in one of two ways, depending on how
clever you want to be:

- Have a `Inst::replaceWith` method that does `this->~Inst()` and then
  `new (this) Inst(stuff)`. This works in C++ and it's safe because
  all of the instructions that we'd replace with (Identity, Nop, Jump,
  and Phi) require no additional data beyond what Inst has and so they
  can use the `Inst` basetype, which is guaranteed to not be larger
  than any `Inst` instance.

- Just mutate the `Inst`'s opcode to Identity, Nop, Jump, or Phi and
  have all of the fancy `Inst` subtypes defend against becoming
  Identity/Nop/Jump/Phi. This is easy to do in practice because you
  won't have very many virtual functions in `Inst` if you do it
  right. Maybe the `effects()` function, that I describe in the next
  section, will be virtual. I'd be tempted to go with this approach if
  I was writing a new compiler (though I've used the in-place
  destruct/in-place construct approach previously).

# Uniform Effect Representation

In pure SSA, the only thing preventing reordering of instructions is
that users of an `Inst` must be dominated by the `Inst` they are
using. But you'll want to represent a variety of operations in SSA
that read or write state. You'll also have operations that cannot be
reordered for subtle reasons not related to actual reads or writes -
for example, you might have instructions that trap, and those
instructions cannot be reordered with other control flow.

My approach to handling the interference between instructions is to
abstract it as reads and writes to abstract heaps. We will have lots
of abstract heaps, and the IR should provide API to the producer of
the IR to produce whatever abstract heaps they need to precisely
represent interference. Each instruction should be able to report a
list of abstract heaps that it reads, and a list of abstract heaps
that it writes. Abstract heaps can correspond to things like field
names (if you're compiling a language where fields of different names
never alias). They can also correspond to special things like the
shadow variables used by Phi/Upsilon. If you have a fancy points-to
analysis, then abstract heaps could even correspond to points-to set
members. We'll have multiple special abstract heaps for the compiler's
internal purposes.

Abstract heaps work best when they form a hierarchy. At the top of the
hierarchy is the World abstract heap. Two abstract heaps interfere
with one another if one is an ancestor of the other, or if they are
the same heap.

A read of an abstract heap interferes with any write of an interfering
abstract heap. A write of an abstract heap interferes with any read or
write of an interfering abstract heap.

Because abstract heaps form a hierarchy and they are used exclusively
for testing interference, we can represent an abstract heap as a pair
of integers (the preorder number in the hierarchy, and the postorder
number in the hierarchy). Two abstract heaps interfere if these two
ranges overlap.

The purpose of the effect representation baked into the IR is to
provide a precise always-available baseline for alias information that
is super easy to work with. Abstract heaps serve this purpose well
because they can express very precise aliasing information (you can
have many abstract heaps, and you can have instructions report that
they read/write multiple heaps, so you can be as precise as you want
about pairwise interference relationships between instructions). But,
it's OK for some passes to use additional alias/effect analysis that
gives more precise information. One example is from the previous
section, where an Upsilon (which claims to write!) is eliminated
because the Phi that referenced it was unused.

Instructions can either store a list of abstract heaps read/written
(i.e. store two lists of integer ranges) or you can have a utility
function that produces such lists on demand. This is a space-time
tradeoff. I've always gone for the latter approach (utility function
that produces the read/written abstract heaps on demand), except that
call/load/store instructions will explicitly store the affected
abstract heaps (so that it's up to the producer of the instruction to
tell what heaps they affect). Either way, the goal is to be able to
say stuff like:

    if (!inst1->effects().interferesWith(inst2->effects()))
        // do some reordering

And stuff like:

    Effects effects;
    effects.add(inst1->effects());
    effects.add(inst2->effects());
    if (effects.interferesWith(whatever))
        // stuff

You'll want to create effect summaries, for example to keep track of
all of the effects that happened in a basic block. The `Effects` data
structure can be an interval tree in the general case, though you
might want to have a "fast case" for when the number of ranges is
small where Effects just uses an array. In practice, Effects often
becomes small because your program will have plenty of Calls that
claim to read and write all of memory (and the top memory abstract
heap just subsumes all of the many little precise ones).

It's up to the producer of the IR to come up with abstract heaps to
represent memory. It's fine to create just a single Memory abstract
heap if you don't want to be precise (or can't be, like if you're
compiling WebAssembly's linear memory accesses). Additionally, the
compiler will provide abstract heaps for interferences not related to
memory:

- Phi/Upsilon needs to either have a single SSAState abstract heap, or
  an abstract heap for each Phi if you want to be more precise. In my
  experience, having a single SSAState abstract heap is adequate. This
  further reduces the amount of special cases your compiler needs to
  handle Phis correctly; any pass that uses the effect system as a
  guardrail will correctly handle Phi and Upsilon.

- Control flow instructions write to the Control abstract heap. All
  instructions that cannot be reordered around control flow read the
  Control abstract heap. For example, if you're compiling C then you
  have to account for the fact that a load instruction may trap, and
  so hoisting it above a branch might make a correct program trap when
  it shouldn't (since the branch might have been correlated with the
  pointer being valid). In my IRs, I prevent this by having the load
  instruction read the Control heap. Reading Control interferes with
  writing Control, and branches write to Control, so this trivially
  blocks that bad transformation. If you're compiling a language like
  Java, where lots of operations require checks that may throw
  exceptions (and exception order matters), we might have "side
  exiting" Check instructions that write Control.

For example, the simplest abstract heap hierarchy that I would have
(and maybe what I'd start with) would be:

- World

    - Memory

    - SSAState

    - Control

Depending on the language you're compiling, you might find other cases
of dependencies between instructions that are not elegant to express
using SSA data flow. You can always express dependencies by having
fake data flow edges (that just pass a 0-bit token value), but I find
fake SSA data flow to be annoying in practice. So, I like to decompose
all of those special dependencies into abstract heaps that I invent
for the purpose of getting the dependency relationship that I
want. For example, if I want to say that opcode Stuff depends on
opcode Thingy in some way, then I would add a StuffThingy abstract
heap, have Thingy write to it, and have Stuff read from it.

This approach means that any pass considering whether two instructions
can be reordered only needs to ask:

- Is there an SSA data flow dependency between the instructions? If
  so, then the dominance constraint must be preserved.

- What abstract heaps are read and written by the instructions? If
  those effect sets interfere, then the instructions cannot be
  reordered.

Lots of passes will be asking these kinds of questions, so you'll want
to make it easy to ask them. A general effect representation combined
with instruction-value uniformity means these questions can be asked
without ever having to special-case opcodes. And since abstract heaps
are pairs of integers, it's cheap to have loads of them, making this a
great approach especially if you have very precise alias analysis.

# Closing Thoughts

The approach to building an SSA-based compiler described in this doc
is what I would use if I was writing another compiler today. I like it
because it's straightforward to implement and leads to succinct code
in my experience. The last compiler I wrote this way (B3) generated
better code than LLVM generated for WebKit's purposes. So, I'm
confident that this approach won't stop you from writing a compiler
that generates excellent code.

I'll close by enumerating some additional thoughts I have:

- I recommend implementing Lengauer-Tarjan and using it for all
  dominance queries as well as conversion to SSA. Lengauer-Tarjan runs
  quickly and gives you a dominator tree, which means that the "does A
  dominate B" property can be expressed in O(1) per block. This is
  because you can walk the dominator tree to get pre/post order
  numbers, and then blocks just have to store a pair of integers and
  the dominance query is just a matter of range overlap. Additionally,
  lots of interesting compiler passes walk the dominator tree, so
  you'll need it anyway. Once you have Lengauer-Tarjan and you're
  accustomed to reasoning about dominance queries, it ends up feeling
  most natural to implement SSA conversion using dominance frontiers
  rather than any of the other approaches.

- I recommend having some way of representing non-SSA data flow in the
  IR. One approach is the LLVM one (you just load/store to memory to
  represent not-SSA). Another approach is to add the concept of a
  Variable, and have Set/Get instructions for writing and reading the
  variables (and then you'd also give Set/Get abstract heaps
  corresponding to the Variable they access).

- I recommend lowering from SSA to something that isn't SSA as part of
  instruction selection, since optimal instruction selection produces
  machine code that reassigns to stuff and trying to ascribe SSA form
  onto register uses and defs is more trouble than it's worth.

- I recommend using graph coloring (in particular,
  [IRC](https://dl.acm.org/doi/10.1145/229542.229546)) as the register
  allocator, because it's a well documented algorithm and it's
  competitive (it'll be hard to write a better regalloc). You'll want
  to have a spill fixup phase after it (basically, a common
  subexpression elimination for accesses to spill slots). I also
  recommend graph coloring for stack frame allocation.

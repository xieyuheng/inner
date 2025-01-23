---
title: connection graphs
author: alan bawden
year: 1986
---

# Motive

[2025-01-23]
根据 [wikipedia](https://en.wikipedia.org/wiki/Linear_graph_grammar)：

> a linear graph grammar (also a connection graph reduction system or
> a port graph grammar) is a class of graph grammar on which nodes
> have a number of ports connected together by edges and edges connect
> exactly two ports together.  Interaction nets are a special subclass
> of linear graph grammars in which rewriting is confluent.

因此下面的三个名字时同义词：

- linear graph grammar
- connection graph reduction system
- port graph grammar

并且 interaction nets 是其特殊情况。

# Introduction

> When thinking about programming languages, it is important to choose
> an appropriate abstract machine model. Such an abstract model serves
> to modularize the programming language problem into two pieces:
> translation of some high level language into the language of the
> abstract machine, and implemention of the abstract machine on real
> hardware. This paper presents connection graph grammars as an
> abstract model for parallel computation.

这和 inet-lisp 的实现方式一样。

# Connection Grauhs

> Intuitively, a connection graph is similar to the topological
> structure of an electronic circuit. An electronic circuit consists
> of a collection of gadgets joined together by wires. Gadgets come in
> various types -- transistors, capacitors, resistors, etc. Each type
> of gadget always has the same number and kinds of terminals. A
> transistor, for example, always has three terminals called the
> collector, the base, and the emitter. Each terminal of each gadget
> can be joined, using wires, to some number of other terminals of
> other gadgets.

> A connection graph differs from a circuit chiefly in that we
> restrict the way terminals can be connected. In a connection graph
> each terminal must be connected to exactly one other terminal; in
> particular, there can be no unconnected terminals.

> Some convenient terminology: The gadgets in a connection graph are
> called _vertices_. As in a circuit, the type of a vertex is called
> simply a _type_, and the terminals of a vertex are called
> _terminals_. The wires that join pairs of terminals are called
> _connections_. The number of terminals a vertex has is its
> _valence_.

也许我也应该用 valence 一词来代替 arity。

> The type of a terminal is called a _label_. Thus, associated with
> each vertex type is a set of terminal labels that determine how many
> terminals a vertex of that type will possess, and what they are
> called.

虽然称为类型，
但是如果 vertex 的类型中只包含 label 的名字的话，
其实类似动态类型的 record。

> Given a set of types, each with an associated set of labels, we can
> consider the set of connection graphs over that set of types, just
> as given a set of letters called an alphabet, we can consider the
> set of strings over that alphabet.

这个类比其实可以看作是推广，因为 string 是特殊的 graph。

# Connection Graph Grammars

> A _connection graph grammar_ is a collection of production rules
> called _methods_. Each method describes how to replace a certain
> kind of subgraph with a different subgraph. If the connection graphs
> over some set of types are analogous to the strings over some
> alphabet, then a connection graph grammar is analogous to the
> familiar string grammar.

> Figure 3. Example Method.

用 inet-lisp 的语法写：

```scheme
(define-node cons car cdr up)
(define-node transistor collector base emitter)

(define-rule*
    [(cons car x! up)
     (transistor collector x! emitter)]
  (connect car collector)
  (cons) (=> new-car new-cdr new-up)
  (connect new-cdr new-up)
  (cons up new-car emitter))
```

> Only one form of method will appear in the connection graph grammars
> generated: methods whose left hand side consists of exactly two
> vertices joined by a single connection. Figure 3 is an example of
> such a two-vertex method.

虽然 connection graphs 解除了
interaction nets 中单一 principle port 的限制，
但是保留了规则中只能出现两个 node 的和一种连接的限制。

# Implementing Connection Graphs

> I shall assume a fairly general description of a parallel computer:
> a collection of independent processing elements embedded in some
> communications medium. There is no shared memory -- all information
> is exchanged by explicit interprocessor communications. There must
> be some global addressing scheme for processing elements so that it
> makes sense for one processing element to transmit the address of
> another to a third party. Each processing element needs at least
> enough local memory to hold a handful of processing element
> addresses and small integers.

与 inet-lisp 的 parallel 不同，
这里拒绝了 shared memory，
而是用 interprocessor communication。

> It is clear how the representation of a connection graph can be
> distributed among the processing elements of a parallel computer:
> individual vertices can be held locally in processing elements, and
> connections can be implemented by passing around addresses.

> Methods can be applied as a purely local operation. The processing
> elements that contain the vertices that constitute an instance of
> the left hand side of some method can agree to replace that subgraph
> with an instance of the right hand side, without bothering any other
> processing elements. Thus many methods can be applied in parallel
> throughout the machine.

但是看上面两段的描述，还是 shared memory multithreading。

> Two-vertex methods are particularly easy to implement because they
> require the cooperation of at most two processing elements. It is
> hard to imagine a scheme for parallel computation in which no
> processing element ever has to cooperate with any other processing
> element, so this, in some sense, is minimal.

从这一段看来一个 vertex 对应一个 processing element，
可能 Bawden 想的是 actor model。
可能 Bawden 没能像 Lafont 一样，
发现其实可以用 shared memory multithreading。

> Any graph reduction based model can make similar claims of locality;
> the distinguishing feature of the connection graph model is the
> _connection_. Other models join objects to each other using
> pointer-like mechanisms, which allow an unbounded number of other
> objects to hold references to any given object. In a connection
> graph a trivalent vertex, for example, is referenced from (connected
> to) exactly three other places.

比如 propagator model 中，
一个 propagator 可以和任意多个 cell 相连。

但是这里不能只说和其他 graph rewriting 的区别在于 connection，
应该说区别在于 linear connection。

也不应该说 pointer 和 connection 有区别，
其实 linear connection 就是用 pointer 实现的，
只不过区别在于是否以 linear 的方式使用 pointer。

关于如何实现 Bawden 讲的很混乱，
还是看看如何编程吧。

# Translating Lisp into a Grammar

首先 function call 是用 explicit (call/n) 节点来表示的，
而不是以 function 本身为节点。

其次是给每个 lambda 所形成的 closure 分配一个 unique node，
比如这里的 (G0069) 和 (G0257)，
这些节点和 (call/n) 反应时会产生新的子图。

也就是说，并不是像用 interaction combinators 那样，
这里并没有一个一般的 (lambda) 节点。

> The introduction of anonymous vertex types, such as G0069, and new
> methods [rules] for those types, such as the method in figure 6, is
> a consequence of the declarative nature of LAMBDA-expressions. The
> graph of a LAMBDA-expression itself is always very simple,
> consisting of a single vertex of an anonymous type. The body of the
> LAMBDA-expression declares how that type should behave in
> conjunction with an appropriate CALL vertex.

这里就是把 closure 编译成 rule，
而 interaction combinators 是要实现 (lambda) 本身。
实现 (lambda) 本身的过程，
其实就类似在 inet 中实现 inet。

如果真的想实现匿名的 (lambda) 而避免生成 unique node，
可以简单地把 rule 的内容包含在 (lambda) 中。

处理 lambda 中的自由变量的方式就是给所生成的 node 增加 port！

> ... the extra terminal is used to pass the value of the free
> variable X from where the closure was generated to where it is
> invoked.

这样就有了一个很简单的把 lambda calculus 翻译到 inet 的方式了。

> Now we can see why we need only consider two-vertex methods: they
> can be used to capture the basic mechanism of procedure calling. One
> vertex represents the procedure to be called. Its terminals (except
> the one connecting it to the other vertex) are the environment of
> the procedure.  Its type is the procedure code.  The other vertex
> [(call/n)] is the argument list. Its terminals are the arguments to
> be passed to the procedure, and the continuation to be connected to
> the returned value. Its type is used to indicate the operation that
> should be performed (the procedure should be _called_), and allows a
> procedure call to be distinguished from a procedure that is merely
> connected to some static data structure, such as when a procedure is
> an element of a list built from CONS vertices.

注意，到目前为止这里所用到的 net 都还在 inet 的范围内，
即，都是带有 single principle port 的 node。

> This suggests how a two-vertex method can also be viewed as a
> messaae pass [1] [9]. One vertex is the object, the other is the
> message. The terminals of the object vertex are its instance
> variables. The terminals of the message vertex are the arguments to
> the message.

closure 可以看成是 object，
但是只能接受 (call/n) 这一种信息，
此时 object 的 instance variables
与 closure 的 free variables 对应。

> Figure 12 shows the method for an object of type 4
> receiving an ADD1 message.

显然和 object 与 message 之间的关系是对称的，
选择把一方看成是 object 另一方看成是 message 只是视角上的选择。
这与 inet 中 constructor 与 eliminator 之间的对称一样。

> When a two-vertex method is viewed as a message pass, the difference
> between the message and the object is entirely in the eye of the
> beholder.  The method could just as well be interpreted in the other
> way, so that object and message exchange roles!  This symmetry is
> possible because connections themselves are symmetrical.  In
> ordinary programming languages, where all objects are referenced
> through asymmetrical pointers, this symmetcy doesn't exist.

conditional 的翻译比较简单，
直接用 inet 的 rule 自带的 disjunction 就可以。
注意，和 lambda 一样，这里没有定义一般的 (if) node，
而是给每个 conditional 生成了一个 unique node。

TODO

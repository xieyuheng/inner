---
title: connection graphs
author: alan bawden
year: 1986
---

# My Motive

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
和 lambda 一样，这里没有定义一般的 (if) node，
而是给每个 conditional 生成了一个 unique node。
注意，和 lambda 所生成的 unique node 一样，
如果规则中有 free variable，
conditional 所生成的 unique node
也要带有额外的 port 来处理 free variable。

这里 (put)-(cell) 和 (get)-(cell) 之间的规则还是符合 inet 的，

```scheme
;; I use STATE as port name -- for passing state,
;; where the paper uses USERS.

(define-node cell value state)
(define-node put target new state)
(define-node get target cont state)

(define-rule (put (cell value) new state)
  (drop value)
  (cell new state))

(define-rule (get (cell value) cont state)
  (copy value) (=> first second)
  (connect cont second)
  (cell first state))
```

> The bottom four methods are variations of the same basic idea.
> They allow PUT and GET operations to propagate from multiple
> references at the leaves, up the fan-in tree, to the apex,
> where the state variable can be accessed.

```scheme
(define-rule*
    [(put target! new state)
     (copy value target! second)]
  (copy (put value new) state second))

(define-rule*
    [(put target! new state)
     (copy value first target!)]
  (copy (put value new) first state))

(define-rule*
    [(get target! cont state)
     (copy value target! second)]
  (copy (get value cont) state second))

(define-rule*
    [(get target! cont state)
     (copy value first target!)]
  (copy (get value cont) first state))
```

但是 (put)-(copy) 和 (get)-(copy) 之间的规则，
使得 (put) 和 (get) 可以越过 (copy) 就不符合 inet 了，
因为按照 inet 的定义，这里所连接的 (copy) 的 port 并不是 principle port。
如果真的这样定义就会产生 non-deterministic。
执行 put 和 get 的先后顺序，会影响运算结果。

> Figure 20 demonstrates how a CELL functions.

这里假设了引用同一个局部变量多次时，会有 implicit-copy。

```scheme
(let ([x (cell 3)])
  (put x 4)
  (get x))
```

如果用 explicit-copy（需要语言能够处理多返回值）：

```scheme
(define (main cont)
  (copy (cell 3))
  (=> c1 c2)
  (drop (put c1 4))
  (drop (get c2 cont)))
```

> That beta-reduction is in some way incompatible with both
> side-effects and non-determinism is well-known. Connection graphs
> give us a new way to look at this incompatibility. Expressions have
> a basically tree-like structure; multiple occurrences of variables
> in an expression introduce loops into the structure, as in
> figure 17. When beta-reduction textually substitutes expressions for
> variables, it eliminates the loops. Therefore, the beta-reduction
> rule is not sound when the meaning of an expression is taken to be
> the connection graph it describes.

> Connection graphs are a more realistic way to assign meanings to
> programming language expressions, because the interactions of
> expressions with side-effects and non-determinism are explicitly
> accounted for.

最后这两段有意思，
lambda calculus 好的性质 -- determinism -- 被说成了不好的。
并且 lambda calculus 也不是不能处理 side-effects，
只要稍加扩展就可以了。

# Implementation Status

> A prototype compiler for a connection graph programming language,
> and a simulator, have been implemented on Symbolics Lisp Machines.

> A code generator for the Thinking Machines Corporation connection
> machine, a fine grained, massively parallel computer [4], is
> currently under development.

> A few small test programs have been written. We are just beginning
> to write our first sizable program, a parallel production system.

这里提到的
[connection machine](https://en.wikipedia.org/wiki/Connection_Machine)
是个失败的并行计算机项目。

# Conclusion

> Connection graphs are well suited for implementation on at least the
> class of parallel computers consisting of independent, communicating
> processing elements. The mechanism of connections and two-vertex
> methods makes it easy and natural for such parallel machines to
> execute a connection graph grammar. With a little work, many common
> cases of interprocessor communication can be reduced to a single
> message transmission, and more efficient representations for
> vertices serving as conventional data structures can be deduced.

> A programming language based on connection graph grammars can be
> constructed using two-vertex methods to implement a procedure
> calling and message sending mechanism. The symmetry of connections
> allow the notions of _object_ and _message_ to emerge in a new light
> as completely dual concepts. A difficulty arises with respect to
> multiple occurrences of variables in expressions, but it is shown to
> be merely an old adversary in new clothing.

# My Conclusion

1986-connection-graphs 是在 Lafont 的 1990-interaction-nets 之前的论文。
Lafont 没有引用这篇论文，可能是不知道。

这里对计算模型的设计偏重试验，
没有考虑太多 deterministic 和 termination，
而 Lafont 通过增加限制来保证 deterministic 的同时，
还用大量理论篇幅来讨论 termination，
尽管对 termination 的讨论不是很令人满意。

这篇论文中有趣的 ideas：

（1）给出了一种把 lambda calculus 翻译到 inet 的方案：

- use explicit (call/n) node.
- function is like object that can only receive (call/n) as message.

（2）给出了 non-determinism 的 inet 的具体例子。

- 没有了 single principle port 的限制，
  non-determinism 来自一个 node (copy)
  对其他两个 node (put) (get) 都能反应。

- parallelization 还是可以的，
  因为 rule 的 "two-vertex" pattern 都是局部的且上下文无关的。

这个作者 Alan Bawden 的论文风格像是其他 Schemers 的论文，
我读起来很舒服，可以继续读他的博士论文。

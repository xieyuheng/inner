---
title: SICP Notes
year: 1996
---

# 5 Computing with Register Machines

> In this chapter we will describe processes in terms of the
> step-by-step operation of a traditional computer. Such a computer,
> or _register machine_, sequentially executes _instructions_ that
> manipulate the contents of a fixed set of storage elements called
> _registers_. A typical register-machine instruction applies a
> primitive operation to the contents of some registers and assigns
> the result to another register. Our descriptions of processes
> executed by register machines will look very much like
> “machine-language” programs for traditional computers.

> However, instead of focusing on the machine language of any
> particular computer, we will examine several Lisp procedures and
> design a specific register machine to execute each procedure.

每一个 procedure 都要被描述为一个 register-machine。
这意味着要设计一个一般的，可以描述任何 register-machine 的中间语言。

> Thus, we will approach our task from the perspective of a hardware
> architect rather than that of a machine-language computer
> programmer.

## 5.1 Designing Register Machines

> To design a register machine, we must design its _data paths_
> (registers and operations) and the _controller_ that sequences these
> operations.

可以对比一下这种设计，与现在流行的 SSA 的区别。
也许这里所设计的语言与 GCC 所用过的 RTL 很像，
毕竟 GCC 的作者是 sussman 的学生。

介绍 data-path diagram。
其特点好像是可以并行，或者有可能做并行优化。
更像是硬件描述语言，而不是汇编语言或者编译器中间语言。

这里的计算模型也预言了之后的 propagator model。
与 propagator 不同，这里有 explicit controller。

数据路径 和 控制单元 这两个概念，应该来自数字电路设计。

> In order for the data paths to actually compute GCDs, the buons
> must be pushed in the correct sequence. We will describe this
> sequence in terms of a controller diagram.

controller diagram 其实就是 flowchart，
注意 flowchart 与 automata 等价：

- 状态机（automata）：点代表状态，边代表转移。
- 流程图（flowchart）：点代表操作，边代表状态。
- 另外 petri net 作为二分图，是二者的推广，有两种点：
  - 一种代表操作，与流程图中的点一样；
  - 一种代表状态（place），与状态机中的点一样。

上面 data-path diagram 其中 edge 被加上了 button，
也类似二分图，与 petri net 相似。

而 controller diagram 不只是
data-path diagram 的另外一种视角，
还给判断与循环之类的控制信息。

相比之下，petri net 是想用一种图，
来同时描述 data-path 与控制信息。

### 5.1.1 A Language for Describing Register Machines

> Data-path and controller diagrams are adequate for representing
> simple machines such as GCD, but they are unwieldy for describing
> large machines such as a Lisp interpreter. To make it possible to
> deal with complex machines, we will create a language that presents,
> in textual form, all the information given by the data-path and
> controller diagrams. We will start with a notation that directly
> mirrors the diagrams.

下面将讲授设计语言的过程。

> Figure 5.3:

```scheme
(data-paths
 (registers
  ((name a)
   (buttons ((name a<-b) (source (register b)))))
  ((name b)
   (buttons ((name b<-t) (source (register t)))))
  ((name t)
   (buttons ((name t<-r) (source (operation rem))))))
 (operations
  ((name rem) (inputs (register a) (register b)))
  ((name =) (inputs (register b) (constant 0)))))

(controller
 test-b
   (test =)
   (branch (label gcd-done))
   (t<-r)
   (a<-b)
   (b<-t)
   (goto (label test-b))
 gcd-done)
```

上面所设计的语言很详细，
但是如果多次使用了一个 operation 比如 rem，
所设计的语法是没法区分一种 operation 的多次出现的。

> Unfortunately, it is difficult to read such a description. In order
> to understand the controller instructions we must constantly refer
> back to the definitions of the buon names and the operation names,
> and to understand what the buons do we may have to refer to the
> definitions of the operation names. We will thus transform our
> notation to combine the information from the data-path and
> controller descriptions so that we see it all together.

> To obtain this form of description, we will replace the arbitrary
> button and operation names by the definitions of their behavior.

所谓 "definitions of their behavior" 其实就是 expression。

```scheme
(controller
 test-b
   (test (op =) (reg b) (const 0))
   (branch (label gcd-done))
   (assign t (op rem) (reg a) (reg b))
   (assign a (reg b))
   (assign b (reg t))
   (goto (label test-b))
 gcd-done)
```

很像是 basic-lisp 的设计了：

```scheme
(controller
 test-b
   (branch (equal? b 0) gcd-done)
   (= t (rem a b))
   (= a b)
   (= b t)
   (goto test-b)
 gcd-done)
```

> Exercise 5.2: Use the register-machine language to describe
> the iterative factorial machine of Exercise 5.1.

```scheme
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
      product
      (iter (* counter product)
            (+ counter 1))))
  (iter 1 1))
```

```scheme
(define-machine factorial
  (register n product counter)
  (controller
    test-counter
      (test (op > (reg counter) (reg n)))
      (branch (label done))
      (assign product (op *) (reg counter) (reg product))
      (assign counter (op +) (reg counter) (const 1))
      (goto (label test-counter))
    done))
```

> Figure 5.4: A GCD machine that reads inputs and prints results.

```scheme
(controller
 gcd-loop
   (assign a (op read))
   (assign b (op read))
 test-b
   (test (op =) (reg b) (const 0))
   (branch (label gcd-done))
   (assign t (op rem) (reg a) (reg b))
   (assign a (reg b))
   (assign b (reg t))
   (goto (label test-b))
 gcd-done
   (perform (op print) (reg a))
   (goto (label gcd-loop)))
```

> Instead of having the machine stop after printing the answer, we
> have made it start over, so that it repeatedly reads a pair of
> numbers, computes their GCD, and prints the result.

### 5.1.2 Abstraction in Machine Design

TODO

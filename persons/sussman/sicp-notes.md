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

不同的是：

- 这里每个 operand 都要明显表示出来，比如 (reg) 和 (const)。
  类似地，operator 也要用 (op) 表示出来，
  label 要用 (label) 表示出来。
- 这里的 (test) 和 (branch) 配合，而不是一个 (branch)。

> This form of description is easier to read than the kind illustrated
> in Figure 5.3, but it also has disadvantages:

> - It is more verbose for large machines, because complete
>   descriptions of the d ata-path elements are repeated whenever the
>   elements are mentioned in the controller instruction sequence.
>   (This is not a problem in the  GCD example, because each
>   operation and button is used only once.) Moreover, repeating the
>   data-path descriptions obscures the actual data-path structure of
>   the machine; it is not obvious for a large machine how many
>   registers, operations, and buttons there are and how they are
>   interconnected.

也就是说图可以表达出更多关于连接细节的信息。

> - Because the controller instructions in a machine definition look
>   like Lisp expressions, it is easy to forget that they are not
>   arbitrary Lisp expressions. They can notate only legal machine
>   operations.  For example, operations can operate directly only on
>   constants and the contents of registers, not on the results of
>   other operations.

不能有嵌套的表达式，其实这个限制在上面的语法设计中已经能体现出来了。

比如：

```scheme
   (assign t (op rem) (reg a) (reg b))
```

反之，如果写成下面这样，
「不允许嵌套」就体现不出来了：

```scheme
   (assign t (rem (reg a) (reg b)))
```

此时下面的例子看起来也是合法的：

```scheme
   (assign t (rem (rem (reg a) (reg b))
                  (rem (reg a) (reg b))))
```

> In spite of these disadvantages, we will use this register-machine
> language throughout this chapter, because we will be more concerned
> with understanding controllers than with understanding the elements
> and connections in data paths. We should keep in mind, however, that
> data-path design is crucial in designing real machines.

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
      (branch (label factorial-done))
      (assign product (op *) (reg counter) (reg product))
      (assign counter (op +) (reg counter) (const 1))
      (goto (label test-counter))
    factorial-done))
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

> Figure 5.6: Controller instruction sequence for
> the GCD machine in Figure 5.5.

```scheme
(controller
 test-b
   (test (op =) (reg b) (const 0))
   (branch (label gcd-done))
   (assign t (reg a))
 rem-loop
   (test (op <) (reg t) (reg b))
   (branch (label rem-done))
   (assign t (op -) (reg t) (reg b))
   (goto (label rem-loop))
 rem-done
   (assign a (reg b))
   (assign b (reg t))
   (goto (label test-b))
 gcd-done)
```

对比原来的：

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

### 5.1.3 Subroutines

构拟 subroutine 被发明的历程。
但是此时还没有处理嵌套的调用和递归函数。

> Figure 5.10: Assigning labels to the continue register
> simplifies and generalizes the strategy shown in Figure 5.9.

```scheme
gcd
  (test (op =) (reg b) (const 0))
  (branch (label gcd-done))
  (assign t (op rem) (reg a) (reg b))
  (assign a (reg b))
  (assign b (reg t))
  (goto (label gcd))
gcd-done
  (goto (reg continue))
  ...

  ;; Before calling gcd, we assign to continue
  ;; the label to which gcd should return.
  (assign continue (label after-gcd-1))
  (goto (label gcd))
after-gcd-1
  ...

  ;; Here is the second call to gcd,
  ;; with a different continuation.
  (assign continue (label after-gcd-2))
  (goto (label gcd))
after-gcd-2
```

### 5.1.4 Using a Stack to Implement Recursion

> Figure 5.11: A recursive factorial machine.

```scheme
(controller
   (assign continue (label fact-done))   ;set up final return address
 fact-loop
   (test (op =) (reg n) (const 1))
   (branch (label base-case))
   ;; Set up for the recursive call by saving n and continue.
   ;; Set up continue so that the computation will continue
   ;; at after-fact when the subroutine returns.
   (save continue)
   (save n)
   (assign n (op -) (reg n) (const 1))
   (assign continue (label after-fact))
   (goto (label fact-loop))
 after-fact
   (restore n)
   (restore continue)
   (assign val (op *) (reg n) (reg val))  ;val now contains n(n - 1)!
   (goto (reg continue))                  ;return to caller
 base-case
   (assign val (const 1))                 ;base case: 1! = 1
   (goto (reg continue))                  ;return to caller
 fact-done)
```

如果有 call 和 return：

```scheme
(define-machine factorial
   (test (op =) (reg n) (const 1))
   (branch (label base-case))
   (save n)
   (assign n (op -) (reg n) (const 1))
   (assign val (call factorial) (reg n))
   (restore n)
   (assign val (op *) (reg n) (reg val))
   (return val)
 base-case
   (assign val (const 1))
   (return val))
```

或者更进一步假设 call 不能嵌套，
并且假设 return 的 value 会被保存在 `(reg result)` 中：

```scheme
(define-machine factorial
   (test (op =) (reg n) (const 1))
   (branch (label base-case))
   (save n)
   (assign n (op -) (reg n) (const 1))
   (call factorial)
   (assign val (reg result))
   (restore n)
   (assign val (op *) (reg n) (reg val))
   (return val)
 base-case
   (assign val (const 1))
   (return val))
```

TODO

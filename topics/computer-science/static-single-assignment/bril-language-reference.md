---
title: bril language reference
source: https://capra.cs.cornell.edu/bril/lang/syntax.html
---

看 bril 目前的设计：

- 每个 operator 的参数必须是 name，
  而不能是 immediate value。

  如果需要 immediate value，
  需要先用 const 把 value 保存到 variable 中。

  但是 name 有三类，被分到了 instr 的三个 fields 中：

  - args: local variable name
  - labels: local label
  - funcs: function

  可否进一步简化为都用 name？
  又或者说，这种区分是合理的，
  算是对 name 的类型声明。

  注意，在具体语法中这三者是以前缀区分的：

  - `variable`
  - `.label`
  - `@func`

- 没有 assignment operator，
  或者说没有 move operator。

  每个 instr 都可以有 dest（destination），
  是否对这个 dest 做 assignment，
  取决于具体的 instr。

  有 id operator，可以用来把某个 variable 中的值复制到新的 variable。
  为什么不管这个 operator 叫做 move 呢？
  因为 move 和 = 的意义重复了。

- SSA 有两个版本，一个是经典的 phi。
  一个是 set 和 get。

  set 和 get 类似 assign 和 reference，
  但是与 SSA 一般的限制刚好相反：
  一个函数内对于同一个变量，最多只有一个 get。

  用法就是：
  当两个分支 block A B，想要 merge 进入一个 block C 时，
  与其在 C 的开头用 phi，
  不如在 C 的开头用 get，在 A B 的末尾用 set。

  get 隐含了 dest 名字，是 shadow variable 名字。
  也可以让 set 和 get 的对象都是 channel。
  一个 channel 只能被 get 一次。

  但是 channel 就需要让人在 variable 之外，
  另取想新的名字，这样并不好。

  也许 get 应该叫做 use，
  其作用是声明我要用这个变量。

  既然说了：

  > Shadow variables obey a static single use (SSU) restriction, i.e.,
  > there may be only one get in a function that reads from each
  > shadow variable.

  那么不如直接叫 use。

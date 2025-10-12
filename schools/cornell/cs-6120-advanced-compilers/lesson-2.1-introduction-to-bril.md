---
title: lesson 2.1 -- introduction to bril
source: "https://www.cs.cornell.edu/courses/cs6120/2020fa/lesson/2/"
---

[2025-10-05]

- bril 是老师的 IR 项目：

  - https://capra.cs.cornell.edu/bril/
  - https://github.com/sampsyo/bril

  动机：

  - bril 是为教学而设计的，保持简单。
  - bril 可以随便扩展 operator 和 type。

- 直接用 JSON 作为 AST。

  具体语法：

  ```c
  @main {
    v0: int = const 1;
    v1: int = const 2;
    v2: int = add v0 v1;
    print v2;
    v3: ptr<int> = alloc v0;
    free v3;
  }
  ```

  翻译成 JSON AST：

  ```js
  {
    "functions": [{
      "name": "main",
      "instrs": [
        { "op": "const", "type": "int", "dest": "v0", "value": 1 },
        { "op": "const", "type": "int", "dest": "v1", "value": 2 },
        { "op": "add", "type": "int", "dest": "v2", "args": ["v0", "v1"] },
        { "op": "print", "args": ["v2"] },
        { "op": "alloc", "type": { "ptr" : "int" }, "dest": "v3", "args": ["v0"] },
        { "op": "free", "args": ["v3"] },
      ]
    }]
  }
  ```

  其实用 lisp 就可以兼顾具体语法和 JSON AST 的优点：

  ```scheme
  (define (main)
    (= v0 int (const 1))
    (= v1 int (const 2))
    (= v2 int (add v0 v1))
    (print v2)
    (= v3 (ptr int) (alloc v0))
    (free v3))
  ```

- 老师所用的很多工具都是 rust 系的，比如：

  - deno instead of node
  - uv instead of pip

  不同工具是用不同语言写的，想要跑起来，
  就要安装一些列 rust 系的工具，很麻烦。

  老师解释故意用很多语言写是为了证明 JSON AST 的普适性。
  但是这其实不用什么证明，大家都了解。

  正确的策略也许是用单一的语言写一套方便的工具，
  如果 bril 真 的有价值，
  别的开发者会根据需要用各自喜欢的语言来写新的工具的。

- 由于 bril 的 JSON AST 是描述 instruction list 的，
  所以这里老师展示如何用 python 把 instruction list 转化为 CFG。

  这里展示了 unix pipe 可以很方便地用来测试 JSON AST 的处理与转化程序。

  老师直接在课堂中现场写代码，这种教学方式也不错。
  重点也是学生需要暂停视频，先独立给出自己的实现。

  与 IU 的 homework + code review 的方式相比，
  现场写代码的方式只能用于视频课程，
  因为一般的课程没法暂停。

- 老师介绍了一个自己写的 snapshot testing 工具，
  叫 turnt：https://github.com/cucapra/turnt

  实际上按照我的经验，
  snapshot testing 用 unix pipe + git diff 就足够了，
  根本没必要再依赖额外的工具。

---
title: weak memory concurrency in c11
author: Ori Lahav
author-homepage: "https://www.cs.tau.ac.il/~orilahav"
source: "https://www.youtube.com/watch?v=mOqu8vGSysc"
---

# My motive

[2025-03-08] 在学习并行编程时，
发现如果想要实现无锁的算法，
就需要学习 weak memory model。
Ori Lahav 这个演讲是我找到的对这个主题最好的介绍。

# Example: Dekker's mutual exclusion

[Dekker's algorithm](https://en.wikipedia.org/wiki/Dekker's_algorithm)
是一个用 shared-memory + control flow 解决 mutual exclusion 问题的巧妙算法，
可以想象有两个病人问诊但是只有一个医生的场景。

由于这个算法中使用 shared-memory 的方式，
刚好可以展示 weak memory model 中反直觉的现象。

```scheme
(begin
  (store x 0)
  (store y 0)

  (thread-start
   (lambda ()
     (store x 1)
     (store a (load y))
     (if (eq? a 0)
       (critical-section))))

  (thread-start
   (lambda ()
     (store y 1)
     (store b (load x))
     (if (eq? b 0)
       (critical-section)))))
```

TODO

# Execution graphs

TODO

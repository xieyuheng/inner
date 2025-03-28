---
title: parallel implementation
date: 2025-03-18
---

并行编程学的差不多了。
记录一下目前能想到的实现并行 inet 的方案。

# 前提

首先计划使用 lock-free 的 queue，
因为如果不要求 queue 的两端都可以 enqueue 和 dequeue，
而是只要求一端可以 enqueue 另一端可以 dequeue，
并且又只要求两个 thread 在分别调用 enqueue 和 dequeue 时可以并行，
而不要求诸多 enqueue 或诸多 dequeue 之间的并行，
那么 lock-free 的实现就非常简单。
因为虽然 read 的数据重叠了（有 race），
但是 write 的数据没有重叠（没有 race）。

# 划分

设有一个 scheduler thread，和 N 个 worker thread。

scheduler thread 有 N 个 task queue，分别对应 N 个 worker：

- worker 可以 enqueue -- 把新生成的 task 返回给 scheduler。
- scheduler 可以 dequeue -- 把所收集到的 task 取出来，将会分配给某个 worker。

每个 worker thread 有一个 task queue：

- worker 可以 dequeue -- 取出 task 来处理。
- scheduler 可以 enqueue -- 把某个 task 分配给这个 worker。

# 分配

worker 返回给 scheduler 的 task 需要被重新分配给各个 worker，
分配 task 的策略可以多种多样，但是需要保证：

- 分配的速度本身不能太慢，scheduler 不应该卡住诸 worker 之运行。
- 分配要公平，要避免某个 worker 被分配了很多 task，
  而另外的 worker 没有分配 task 的情况。

大致思路是：

- scheduler query 所有 worker 的状态，
  以这些信息为依据，分配一波 task。
  然后循环。

具体方案如下：

- 首先，scheduler 读所有 worker 的待处理 task 数量，
  所读到的信息对于算法来说是 heuristic 的，
  因此不需要精确的数量，因此不需要 lock。

- 然后，找到待处理 task 最多的 worker，
  思路是以它所待处理的 task 数量作为基准，
  给其他 worker 补充 task，
  以求把所有 worker 的有待处理的 task 数量补齐。
  假设，有 5 个 worker，
  所读到的待处理 task 数量分别是 [4, 6, 8, 12, 20]，
  最大是 20，因此需要补充的 task 数量分别是 [16, 14, 12, 8, 0]，
  在这个基础上增加一个常数，比如 1，
  以保证所有的 worker 都能分配到新的 task，
  因此需要补充的 task 数量分别是 [17, 15, 13, 9, 1]。

- 然后，scheduler 用 worker 返回给 scheduler 的 task
  去补齐 worker 的待处理 task。
  scheduler 在一个件循环中，
  逐个 dequeue 属于它自己的 N 个 task queue，
  补充到待补充的 worker 中。
  注意，在这个过程中，为了尽量避免某个 worker 没有活干，
  需要用消除俄罗斯方块的方式消除数组 [17, 15, 13, 9, 1]。
  具体效果如下：
  - [17, 15, 13, 9, 1]
  - [16, 14, 12, 8, 0]
  - [15, 13, 11, 7]
  - [14, 12, 10, 6]
  - ...
  - [8, 6, 4, 0]
  - [7, 5, 3]
  - ...

- 当待补齐的数组完全消除时，
  就重新读所有 worker 的待处理 task 数量，再补齐一次。

## 算法何时停止？

如果 scheduler 正跑在消除某个数组的循环中，
并且跑了几圈（比如 3 圈）之后，
发现这几圈下来 N 个 task queue 里都全是空的，
就可以停下来看看是不是所有 worker 都已经停止了，
如果所有 worker 都已经停止了，
并且再次检查自己的 N 个 task queue 还是全是空的，
那么就可以确定是所有 task 都处理完了。

设计这唯一个算法的停止条件就足够了，
其他的情况都可以作为这个情况的特例。

## 算法如何开始？

启动所有 worker thread，
初始化 scheduler 的时候，
随便给 N 个 task queue 中的一个初始化一个 task，
然后启动 scheduler，就可以了。

## thread loop 到机器的类比

每一个 thread 函数，
都有一个无条件的循环，
可以被全局变量控制是否退出。

因此可以把一个运行中的 thread
想象成是一个运行中的机器，
全局变量就是开关。

但是与人们正常使用机器的流程不同的是：

- start thread = 制造机器 + 启动机器
- variable off = 停止机器
- join thread = 销毁机器

差异就在于，一般的机器是会不频繁制造和销毁的。
也许可以给每个循环都设计一个 explicit 开，
这样就可以简化 thread 和思想模型之间的对应关系：

- start thread = 制造机器
- variable on = 启动机器
- variable off = 停止机器
- join thread = 销毁机器

# [2025-03-23] 讨论实现细节

已经完成了 thread safe queue，
回到 scheduler 的问题讨论实现细节：

- 首先 `mod_t` 有自然的 `loader_worker`，
  这个 worker 是不参与并行计算的，
  只会在调用 `load_mod` 时参与单线程计算。

- 可以把 `(run)` 这个函数作为并行计算的 API：
  - 如果是在 debug，就单线程地 run task；
  - 如果不是在 debug，就创建 scheduler，
    然后把当前的 worker 中的 task 转移到 scheduler，
    调用 scheduler 的 API 来做并行计算。

- 也就是说 `worker_t` 有两类：
  - 一类是 scheduler 创建的 -- 也许可以称作 pool worker；
  - 一类是 `loader_worker`。
  pool worker 为了能够把新生成的 task 返回给 scheduler，
  要引用到创建这个 pool worker 的 scheduler，
  还要有一个 index 代表是 scheduler 所创建的第几个 worker。
  是否有引用到 scheduler 就区分了 pool worker。
  可以实现一个 `worker_is_in_pool`。

- `worker_t` 不知道 thread id，
  启动和停止 thread 都用 `scheduler_t` 的 API 来管理。
  因此 `scheduler_t` 在启动所有 worker 之后，
  要把 `thread_id` 保存到 `worker_thread_ids`。

- 也许为了避免 false sharing，
  应该给每个 worker thread 分出来一个 `worker_ctx_t`，
  而不是只是用 index 从 scheduler 中取数据。
  这个 ctx 可以是 `thread_start` 时传递的参数，
  因此可以保持 worker 简单。

- `worker_ctx_t` 包含 `worker_t` 和 return task queue。
  因此 `scheduler_t` 应该有 `worker_ctxs`，
  而不是有 `workers` 和 `task_queues`。
  以后所有给 `thread_fn_t` 的参数都可以叫做什么 `ctx_t`。
  但是 `scheduler_t` 应该不需要 `scheduler_ctx_t` 了。

- 虽然有 `worker_ctx_t` 了，
  但是 `worker_t` 还是需要引用 `scheduler_t` 和 `index`，
  因为在生成 node id 的时候，
  可以用这个 index 作为前缀，
  来保证 node id 的唯一性。

- 现在可以先创建 `worker_ctx_t`，
  并且让 `run_task` 在非 debug 时调用 `run_task_parallelly`。
  先让 `scheduler_t` 用最简单的方式实现 `run_task_parallelly`，
  把测试跑起来。然后再：
  - 开启一个 worker，把两个 queue 都用起来；
  - 开启多个 worker，实现调度算法。

- 也许根本没有 false sharing 问题，
  因为设想要放在 `worker_ctx_t` 中的数据是只读的。
  可以先尝试简单的 `worker_t` 有 `scheduler_t` + `index` 的方案。

# [2025-03-28] 第一波并行实验

根本用不到复杂的 scheduler 调度算法，
首先能在 embarrassingly parallel 的情况下避免 data race 就不错了。

由于 inet-lisp 中 subgraph pattern matching 很复杂，
不方便 debug data race，所以回到 inet-forth 中实现 parallel。

发现为了避免 data race 要修改的地方很多，
并且不得不在 batch 结束才 free 某些 wire，
因为可能会有 read (during is principal query)
和 free (during connecting two wires)
之间的 data race。

即便如此，由于测试用例不充分，
可能也每发现所有的 data race。

经过实验，只是简单地计算 nat 的 16 * 16 * 4，
就单线程的 inet-forth 就已经很慢了，
并且多线程并行的时候，并没有节省时间，
反而需要更多时间。

下一步该怎么办？

（1）可以继续尝试优化 inet-forth；

（2）或者学习更多并行计算知识之后再优化 inet-forth；

（3）可以尝试在 inet-lisp 消除在这次实验中发现的 data race，
看看是否 multiple principal ports 还有希望。

（4）学习已有的关于实现并行 inet 的论文，
看看他们是如何解决 data race 问题的。

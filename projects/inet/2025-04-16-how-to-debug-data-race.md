---
title: how to debug data race
date: 2025-04-16
---

先用 tsan 大致找到可疑的地方，
然后给可以的地方加锁 -- `mutex_lock` 看看程序能不能跑过，
先让程序跑过，然后把锁换成 `mutex_try_lock` 来打印具体 data race 的信息。

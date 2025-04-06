---
title: operating systems design and implementation
author: andrew tanenbaum
year: 2006
---

# My Motive

[2025-04-06] 在实现 inet 的并行计算时需要设计 scheduler，
而 scheduler 的设计类似 OS 的 process scheduler。

# Preface

TODO

# 1 Introduction

## 1.3 Operating system concepts

> The interface between the operating system and the user programs is
> defined by the set of "extended instructions" that the operating
> system provides. These extended instructions have been traditionally
> known as **system calls** ...

> The MINIX 3 system calls fall roughly in two broad categories: those
> dealing with processes and those dealing with the file system. We
> will now examine each of these in turn.

# 2 Processes

## 2.1 Introduction to processes

### 2.1.1 The Process Model

因为 CPU 需要在不同的 program 的运行之间切换，
所以需要保存 program 的运行状态，
process 是被数据化了的 process 的运行状态，
使得 OS 可以随时暂停某个 process。

> The key idea here is that a process is an activity of some kind. It
> has a program, input, output, and a state. A single processor may be
> shared among several processes, with some scheduling algorithm being
> used to determine when to stop work on one process and service a
> different one.

### 2.1.5 Process States

> In Fig. 2-2 we see a state diagram showing
> the three states a process may be in:
>
> 1. Running (actually using the CPU at that instant).
> 2. Ready (runnable; temporarily stopped to let another process run).
> 3. Blocked (unable to run until some external event happens).

> Transitions between these states:
>
> 1. [running -> blocked] Process blocks for input
> 2. [running -> ready] Scheduler picks another process
> 3. [ready -> running] Scheduler picks this process
> 4. [blocked -> ready] Input becomes available

> But the point is that instead of thinking about interrupts, we can
> think about user processes, disk processes, terminal processes, and
> so on, which block when they are waiting for something to happen.

这是否意味着 process 是用 interrupt 实现的？

> This view gives rise to the model shown in Fig. 2-3. Here the lowest
> level of the operating system is the scheduler, with a variety of
> processes on top of it. All the interrupt handling and details of
> actually starting and stopping processes are hidden away in the
> scheduler, which is actually quite small.

> The rest of the operating system is nicely structured in process form.

在实现 inet 时，我是否也需要在
task 和 worker 之间加上 process 的概念？
比如一组 tasks 是一个 process。

因为从 process 的状态看来，带有 task 的 worker
就是只有 running 和 ready 两个状态的 process。

但是也许不因该完全模仿操作系统的 scheduler，
因为 inet 的问题有特殊性，
可能有比模仿 process scheduler 更简单的解决方案。

## 2.4 Scheduling

> Categories of Scheduling Algorithms
>
> - All systems
>   - Fairness — giving each process a fair share of the CPU
>   - Policy enforcement — seeing that stated policy is carried out
>   - Balance — keeping all parts of the system busy
> - Batch systems
>   - Throughput — maximize jobs per hour
>   - Turnaround time — minimize time between submission and termination
>   - CPU utilization — keep the CPU busy all the time
> - Interactive systems
>   - Response time — respond to requests quickly
>   - Proportionality — meet users’ expectations
> - Real—time systems
>   - Meeting deadlines — avoid losing data
>   - Predictability — avoid quality degradation in multimedia systems

### 2.4.2 Scheduling in Batch Systems

- First-Come First-Served
- Shortest Job First
- Shortest Remaining Time Next
- Three-Level Scheduling (swap)

### 2.4.3 Scheduling in Interactive Systems

TODO

# 3 Input/Output
# 4 Memory management
# 5 File systems
# 6 Reading list and bibliography

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

### 2.1.2 Process Creation

TODO

# 3 Input/Output
# 4 Memory management
# 5 File systems
# 6 Reading list and bibliography

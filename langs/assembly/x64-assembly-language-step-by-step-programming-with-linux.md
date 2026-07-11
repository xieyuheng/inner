---
title: x64 assembly language step by step programming with linux
author: jeff duntemann
year: 2024
---

# My Motive

[2026-06-22] 在实现 meta-lisp 编译器的时候，
需要实现属于我们自己的 x86 汇编器。
因此学习一下 x86 汇编。

# Introduction

> Being a programmer is one thing above all else: It is understanding
> how things work. Learning to be a programmer, furthermore, is almost
> entirely a process of learning how things work.

"understanding how things work" 应该成为程序员的习惯。

# 1 It’s All in the Plan

> A computer program is a list of steps and tests, nothing more.

steps 很简单，所以要解释 tests。

人在生活中看似复杂的选择，
其实可以分解为简单的「是或否」的选择。

"winning ways" 中，把游戏从数学上抽象为了选择。
可否利用这种抽象来设计好玩的游戏（比如小丑牌）？

> Assembly Language Programming As a Board Game

这个比喻很有趣，大富翁游戏外面那一圈 path，
就是 programming instructions。

和汇编指令一样，大富翁也可能带有 jump，
但是差别在于，汇编指令每次前进一步，
而大富翁的前进的步数是随机的。

# 2 Alien Bases

> Hexadecimal is the programmer’s shorthand for the computer’s binary numbers.

    1111 0000 0000 0000 1111 1010 0110 1110
      F    0    0    0    F    A    6    E

- 1  byte = 8 bits = two hex numbers
- 4 bytes = 32 bits =  8 hex numbers
- 8 bytes = 64 bits = 16 hex numbers

# 3 Lifting the Hood

首先解释 Memory：

> Memory consists of containers for alterable patterns that retain an
> entered pattern until someone or something alters the pattern.

> Every byte of memory in the computer has its own unique address,
> even in computers that process 2, 4, or 8 bytes of information at a
> time.

> The CPU’s immediate work in progress is held in temporary storage
> containers called registers.

TODO

# 4 Location, Location, Location
# 5 The Right to Assemble
# 6 A Place to Stand, with Access to Tools
# 7 Following Your Instructions
# 8 Our Object All Sublime
# 9 Bits, Flags, Branches, and Tables
# 10 Dividing and Conquering
# 11 Strings and Things
# 12 Heading Out to C
# Conclusion: Not the End, But Only the Beginning
# Appendix A The Return of the Insight Debugger
# Appendix B Partial x64 Instruction Reference
# Appendix C Character Set Charts

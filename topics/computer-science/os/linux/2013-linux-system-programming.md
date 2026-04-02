---
title: linux system programming
subtitle: talking directly to the kernel and c library
author: robert love
year: 2013
---

# My Motive

[2026-04-02] 要为 meta-lisp 和 basic-lisp 设计 file 相关的 API。

# 1 Introduction and Essential Concepts

## Concepts of Linux Programming

### Files and the Filesystem

- file 是 byte stream，没有 metadata。
- inode 记录 file 在硬盘的位置，并且保存 metadata，但是不记录 file name。
- directory 作为特殊的 file，保存 file name 到 inode 的映射（称为 link）。
- directory 所保存的 link 称为 hard link，
  可以多 link 对应于一个 inode，
  inode 带有引用计数的。
- symbolic link 是保存文件名的特殊文件。

#  2 File I/O

TODO

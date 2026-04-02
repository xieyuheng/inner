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
- symbolic link 是指向另一个文件的特殊文件，
  通过保存另一个文件的完整路径来实现。

# 2 File I/O

> Before a file can be read from or written to, it must be opened.

因为需要从 path 找到 inode。

> The kernel maintains a per-process list of open files, called the
> _file table_. This table is indexed via nonnegative integers known
> as _file descriptors_ (often abbreviated _fds_). Each entry in the
> list contains information about an open file, including a pointer to
> an in-memory copy of the file’s backing inode and associated
> metadata, such as the file position and access modes.

注意，每个 process 都有自己的 file table。
一个 fd 只有在某个 process 的语境下才有意义。

> By default, a child process receives a copy of its parent’s file
> table. The list of open files and their access modes, current file
> positions, and other metadata are the same, but a change in one
> process -- say, the child closing a file -- does not affect the
> other process’s file table.

TODO

# 3 Buffered I/O

TODO

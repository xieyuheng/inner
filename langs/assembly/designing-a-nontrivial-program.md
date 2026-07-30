---
title: Designing a Nontrivial Program
subtitle: An example from from "x64 Assembly Language Step-by-Step 4TH Edition"
author: Jeff Duntemann
---

# Defining the Problem

> At the very highest level, the problem to be solved here can be stated this way:
>
>    Convert any lowercase characters in a data file to uppercase.

> With that in mind, it’s a good idea to take notes on the
> problem. In particular, take notes on the limitations of any
> proposed solution. We used to call these notes the “bounds” of the
> solution, and they need to be kept in mind while thinking about the
> program that will solve the problem.
>
> - We’ll be working under Linux.
> - The data exists in disk files.
> - We do not know ahead of time how large any of the files will be.
> - There is no maximum nor minimum size for the files.
> - We will use I/O redirection to pass filenames to the program.
> - ...

> Note that these notes expand on what must be done, and to some
> extent put limits on the nature of the eventual solution, but do not
> attempt to say _how_ it must be done. That’s what we do in the next
> step.

# Starting with Pseudocode

> Once we understand the nature of the problem as thoroughly as
> possible, we can begin crafting a solution. At the outset, this much
> resembles the process I describe in Chapter 1, where someone makes a
> “do it” list of tasks for running the day’s errands.

> You state a solution in a broad form and in as few statements as
> possible. Then, little by little, you refine the stated solution by
> breaking down the larger steps into the smaller steps that the
> larger steps contain.

> In our case, the solution is fairly easy to state in broad terms. To
> get started, here’s one form that the statement might take:

```pseudocode
Read a character from the input file.
Convert the character to uppercase (if necessary)
Write the character to the output file.
Repeat until done.
```

> This really is a solution, if perhaps an extreme “view from a
> height.” It’s short on _details_, but not short on function. If we
> execute the steps listed, we’ll have a program that does what we
> need it to do. Note also that the statements given are not
> statements written in any programming language. They’re certainly
> not assembly language instructions. They’re descriptions of several
> actions, independent of any particular system for accomplishing
> those actions. Lists of statements like this, because they are
> deliberately _not_ written as code for a particular programming
> environment, are called _pseudocode_.

# Successive Refinement

> From our first complete but detail-challenged statement of the
> solution, we move toward a more detailed statement of the
> solution. We do this by refining the pseudocode statements so that
> each is more specific about how the action being described is to be
> done. We repeat this process, adding more details every time, until
> what we have can be readily translated into actual assembly language
> instructions. This process, called _successive refinement_, is not
> specific to assembly language. It’s used with all programming
> languages to one degree or another, but it works peculiarly well
> with assembly.

> ... we can begin adding details specific to the Linux way of doing
> such things. The next refinement might look like this:

```pseudocode
Read a character from standard input (stdin)
Test the character to see if it's lowercase.
If the character is lowercase, convert it to uppercase by subtracting 20h.
Write the character to standard output (stdout).
Repeat until done.
Exit the program by calling sys_exit.
```

> How do we know when the input file is out of characters? This may
> require some research, but in most operating systems (including
> Linux) the routine that you call to read data from a file returns a
> value. This value can indicate a successful read, a read error, or
> special-case results like “end of file” (EOF). The precise details
> can come later; what matters here is that we have to test for EOF
> when we read characters from the file. An expanded (and slightly
> rearranged) version of the solution pseudocode might look like this:

```pseudocode
Read a character from standard input (stdin)
Test if we have reached End Of File (EOF)
If we have reached EOF, we're done, so jump to exit
Test the character to see if it's lowercase.
If the character is lowercase, convert it to uppercase by subtracting 20h.
Write the character to standard output (stdout).
Go back and read another character.
Exit the program by calling sys_exit.
```

> And so we go, adding detail each time. Notice that this is starting
> to look a little more like program code now. So be it: As the number
> of statements increases, it helps to add labels to those statements
> that represent jump targets so that we don’t get the jump targets
> mixed up, even in pseudocode. It also helps to break the pseudocode
> up into blocks, with related statements grouped together. Sooner or
> later we’ll get to something like the following:

```pseudocode
Read:
Set up registers for the sys_read kernel call.
Call sys_read to read from stdin.
Test for EOF.
If we're at EOF, jump to Exit.

Test the character to see if it's lowercase.
If it's not a lowercase character, jump to Write.
Convert the character to uppercase by subtracting 20h.

Write:
Set up registers for the Write kernel call.
Call sys_write to write to stdout.
Jump back to Read and get another character.

Exit:
Set up registers for terminating the program via sys_exit.
Call sys_exit.
```

# Code

翻译成汇编代码：

```asm
section .bss
  buffer resb 1

section .data

section .text

global main

main:
  mov rbp, rsp   ; for correct debugging

read:
  mov rax, 0        ; specify sys_read call
  mov rdi, 0        ; specify file descriptor 0: standard input
  mov rsi, buffer   ; pass address of the buffer to read to
  mov rdx, 1        ; tell sys_read to read one char from stdin
  syscall           ; call sys_read

  cmp rax, 0        ; look at sys_read's return value in rax
  je exit           ; jump if equal to 0 (0 means eof) to exit:
                    ; or fall through to test for lowercase

  cmp byte [buffer], 61h  ; test input char against lowercase 'a'
  jb write                ; if below 'a' in ascii chart, not lowercase
  cmp byte [buffer], 7ah  ; test input char against lowercase 'z'
  ja write                ; if above 'z' in ascii chart, not lowercase

                          ; at this point, we have a lowercase character
  sub byte [buffer], 20h  ; subtract 20h from lowercase to give uppercase
                          ; and then write out the char to stdout:

write:
  mov rax, 1        ; specify sys_write call
  mov rdi, 1        ; specify file descriptor 1: standard output
  mov rsi, buffer   ; pass address of the character to write
  mov rdx, 1        ; pass number of chars to write
  syscall           ; call sys_write
  jmp read          ; the go to the beginning to get another char

exit:
  ret               ; end program
```

程序的说明书：

```sh
build:
  nasm -f elf64 to-upper-case.nasm -o to-upper-case.o
  gcc -m64 -no-pie to-upper-case.o -o to-upper-case.exe
usage:
  ./to-upper-case.exe > OUTPUT-FILE < INPUT-FILE
example:
  ./to-upper-case.exe < to-upper-case.nasm
```

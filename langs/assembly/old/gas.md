---
title: gas
---

## 寻址语法

displacement[base register, offset register, scalar multiplier)
+ in Intel syntax:
  [base register + displacement + offset register * scalar multiplier]
+ Either or both of the numeric
  and either of the register parameters may be omitted
```asm
movl    -4(%ebp, %edx, 4), %eax
           # Full example: load *(ebp - 4 + (edx * 4)) into eax

movl    -4(%ebp), %eax
           # Typical example: load a stack variable into eax

movl    (%ecx), %edx
           # No offset: copy the target of a pointer into a register

leal    8(,%eax,4), %eax
           # Arithmetic: multiply eax by 4 and add 8
leal    (%eax,%eax,2), %eax
           # Arithmetic: multiply eax by 2 and add eax (i.e. multiply by 3)
```

## 寻址模式

寄存器的名字也可以被当作特殊的地址
这样就能对下面的东西形成统一的理解了
交换下面的movl的两个变元的位置
所得到的新命令都是符合语法的

```asm
### direct addressing mode
        movl address, %eax

### indexed addressing mode
        movl string_start(,%ecx,1), %eax

### indirect addressing mode
        movl (%eax), %ebx

### base pointer addressing mode
        movl 4(%eax), %ebx
```

## 退出并返回状态码

注意: 注释程序的每个奇怪行为

:tangle ./play/exit.S

```asm
## PURPOSE:
##         Simple program that exits and returns a
##         status code back to the Linux kernel

## INPUT:
##         none

## OUTPUT:
##          returns a status code.  This can be viewed
##          by typing
##          echo $?
##          after running the program

## VARIABLES:
##          %eax holds the system call number
##          %ebx holds the return status


        .section .data
        .section .text
        .globl _start

_start:
        movl $1, %ebx
        movl %ebx, %ecx
        movl %ecx, %edx
        movl %edx, %edi
        movl %edi, %esi
        movl %esi, %eax

        movl $66, %ecx
        movl %ecx, %edx
        movl %edx, %edi
        movl %edi, %esi
        movl %esi, %ebp
        movl %ebp, %esp
        movl %esp, %ebx         # exit status is of 1 byte
        int $0x80

```

:tangle ./play/exit.S

```asm
        .section .data
        .section .text
        .globl _start
_start:
        movq $7, %rdi
        movq $60, %rax
        syscall
```

every program when it exits gives Linux an exit status code
which tells it if everything went all right

## 用状态码返回三个数中最大的数

:tangle ./play/max-of-three.S

```asm
        .section .data
var1:
        .int 66
var2:
        .int 20
var3:
        .int 30


        .section .text
        .globl _start
_start:
        movl  (var1), %ecx
        cmpl  (var2), %ecx
        jg    check_third_var
        movl  (var2), %ecx

check_third_var:
        cmpl  (var3), %ecx
        jg    _exit
        movl  (var3), %ecx

_exit:
        movl  $1, %eax
        movl  %ecx, %ebx
        int   $0x80
```

## 一个数组的数中最大的数

:tangle ./play/max-of-array.S

```asm
### PURPOSE:
###        finds the maximum number in data items array

### VARIABLES:
###        %edi - Holds the index of the data item being examined
###        %ebx - Largest data item found
###        %eax - Current data item
###        data_items: contains the item data, 0 is used to terminate the data


        .section .data
data_items:
        .long 3,67,34,222,45,75,54,34,44,33,22,11,66,0


        .section .text
        .globl _start
_start:
        movl $0, %edi
        movl data_items(,%edi,4), %eax
        movl %eax, %ebx

        ## 上面在于要准备好循环开启时的三个寄存器的状态
        ## 每次要进入循环的时候 都对三个寄存器的状态有约定:
        ## %edi : 当前指向的数的索引
        ## %eax : 当前指向的数
        ## %ebx : 在比较%eax之前 所知道的最大的数

start_loop:
        cmpl $0, %eax
        je loop_exit

        incl %edi
        movl data_items(,%edi,4), %eax

        cmpl %ebx, %eax
        ## %eax <= %ebx
        jle start_loop          # 这个谓词所问的序关系的第一个参数是 cmpl的第二个参数
                                # 这是为了让cmpl与subl一致 subl的第二个参数是被减数
        ## %eax > %ebx
        movl %eax, %ebx
        jmp start_loop


loop_exit:
        movl $1, %eax
        int $0x80
```

## 汇编中的函数

1. 函数名:
   就是代码段地址的标签
   也就是代码段的首地址
2. 输入与输出:
   这是通过约定一些存储器来保存输入数据和输出数据
   对输入与输出的保存应该使用统一的存储器约定
   因为一个函数的输出可能作为其他函数的输入
   使用栈是常用的方式
3. 调用约定(calling convention):
   上面的东西也叫做calling convention
   也就是说需要设计统一的接口来实现函数的调用
4. 副作用:
   在汇编中对辅佐用的使用是最自由的
   >< 如否可以像使用类型推到系统来管理输入输出一样
   以某种方式来系统化地管理副作用呢 ???
   副作用也是有类型的
   并且很多被函数被抽象成一个大函数之后
   也是可以推到副作用的
   但是为什么之前没有这种副作用管理系统呢 ???
   有什么困难的地方我没有注意到的码 ???
5. 继续(返回):
   需要有另一个代码段的地址作为返回地址
   这样这个被调用的代码段执行结束之后程序才能继续下去
   + x86中call和ret帮助人们来实现调用与返回
     但是很容易设计自己的调用和返回的方式
通过讨论汇编中的函数
我可以看到函数语义的本质
我发现我可以设计自己的方式去实现函数语义
下面是一种嵌入scheme中的伪汇编:
+ 比如在forth这中栈处理器之外
  我可以把sicp中的寄存器处理器也嵌入到scheme中
```scheme
'(:other-keys ...

  :><
   ><><><

  :other-keys ...)
```

## calling convention

1. C:
   注意这里就发现C对 %esp 和 %ebp 的使用方式就和forth不一样
   C 对它们的使用就像它们的名字一样是最典型的
   理解了这一点就理解了C中local-var static-var global-var的实现方式
   其中local-var通过%ebp而用栈实现
   而static-var是函数代码段内部的一片区域
   或者 只在这个代码段内保存那片内存的地址
   + 但是当函数调用嵌套时scope就断了 !!!
     因为每次%ebp只能帮你找到当前的召唤者的参数
     所以C中的函数定义不能嵌套 !!!
   + 注意:
     很容易发现C的calling convention是很低能的
     它所实现的函数根scheme中的函数根本就没法相提并论
2. forth:
   与C完全不同
3. scheme:
   scheme是怎么处理这一点的呢??

## a function example

:tangle ./play/power.S
```asm
        .macro PushStack reg
        leaq -8(%rsp),%rsp
        movq \reg,(%rsp)
        .endm

        .macro PopStack reg
        movq (%rsp),\reg
        leaq 8(%rsp),%rsp
        .endm


### PURPOSE:
###        Program to illustrate how functions work
###        This program will compute the value of
###        2^3 + 5^2

### Everything in the main program is stored in registers,
### so the data section doesn't have anything.


        .section .data
        .section .text
        .globl _start
_start:
        PushStack $3                  # push second argument
        PushStack $2                  # push first argument
        ## call power                # call the function
        movq BackToStartAfter_1st_CallingPower, %rcx
        PushStack %rcx
        jmp power
BackToStartAfter_1st_CallingPower:
        addq $16, %rsp             # move the stack pointer back
        PushStack %rax                # save the first answer before
                                  # calling the next function
        PushStack $2                  # push second argument
        PushStack $5                  # push first argument
        ## call  power               # call the function
        movq BackToStartAfter_2st_CallingPower, %rcx
        PushStack %rcx
        jmp power
BackToStartAfter_2st_CallingPower:
        addq  $16, %rsp            # move the stack pointer back
        PopStack  %rbx                # The second answer is already
                                  # in %rax. We saved the
                                  # first answer onto the stack,
                                  # so now we can just pop it
                                  # out into %rbx
        addq  %rax, %rbx          # add them together
                                  # the result is in %rbx
        movq  $60, %rax            # exit (%rbx is returned)
        syscall


### PURPOSE:
###        This function is used to compute
###        the value of a number raised to
###        a power.

### INPUT:
###        First argument - the base number
###        Second argument - the power to
###        raise it to

### OUTPUT:
###        Will give the result as a return value

### NOTES:
###        The power must be 1 or greater

### VARIABLES:
###        %rbx - holds the base number
###        %rcx - holds the power
###
###        -4(%rbp) - holds the current result
###
###        %rax is used for temporary storage
###
###        .type power, @function

power:
        PushStack %rbp                # save old base pointer
        movq  %rsp, %rbp          # make stack pointer the base pointer
        subq  $8, %rsp            # get room for our local storage
        movq  16(%rbp), %rbx       # put first argument in %rax
        movq  24(%rbp), %rcx      # put second argument in %rcx
        movq  %rbx, -8(%rbp)      # store current result
power_loop_start:
        cmpq $1, %rcx             # if the power is 1, we are done
        je    end_power
        movq -8(%rbp), %rax       # move the current result into %rax
        imulq %rbx, %rax          # multiply the current result by
                                  # the base number
        movq %rax, -8(%rbp)       # store the current result
        decq  %rcx                # decrease the power
        jmp   power_loop_start    # run for the next power
end_power:
        movq -8(%rbp), %rax       # return value goes in %rax
        movq %rbp, %rsp           # restore the stack pointer
        PopStack %rbp                 # restore the base pointer


        ## ret
        PopStack %rdx
        jmp *%rdx
```

## x86_32 and x86_64 assembler example

### x86_64

gcc -m64 -nostdlib -o x86_64 x86_64.S
:tangle ./play/x86_64.S
```asm
#include <asm/unistd_64.h>

STDOUT = 1

.data
program_name:           .string "X86_64 AT&T assembler example 1\n"
program_name_length   = 32
exit_code:              .long 0

.text
.globl _start
_start:
        movq $__NR_write, %rax
        movq $STDOUT, %rdi
        movq $program_name, %rsi
        movq $program_name_length, %rdx
        syscall

        popq %rcx                       # argc

argv:
        popq %rsi                       # argv
        test %rsi, %rsi
        jz exit                         # exit if last (NULL) argument string

        movq %rsi, %rdx

strlen:
        lodsb

        test %al, %al
        jnz strlen                      # continue if not end of string

        movb $0x0A, -1(%rsi)            # replace NUL-byte with \n


        subq %rdx, %rsi                 # calculate buffer size
        xchg %rdx, %rsi                 # reorder for syscall conventions
        movq $__NR_write, %rax
        movq $STDOUT, %rdi              # file descriptor
        syscall

        jmp argv                        # process next argument

exit:
        movq $__NR_exit, %rax
        movl exit_code, %edi
        syscall
```

### x86_32

```asm
/* Copyright (C) 2007 Mario Lang <mlang@delysid.org> */
/* Compile with "gcc -nostdlib -o x86_both x86_both.S" adding -m32 or -m64 */
#ifdef __x86_64__
  #include <asm-x86_64/unistd.h>
#elif __i386__
  #include <asm-i386/unistd.h>
#else
  #error "Unhandled architecture"
#endif
STDOUT = 1

.data
#ifdef __x86_64__
program_name:           .string "X86_64 AT&T assembler example 1\n"
program_name_length   = 32
#else
program_name:           .string "X86 AT&T assembler example 1\n"
program_name_length   = 29
#endif
exit_code:              .long 0

.text
.globl _start
_start:

#ifdef __x86_64__
        movq $__NR_write, %rax
        movq $STDOUT, %rdi
        movq $program_name, %rsi
        movq $program_name_length, %rdx
        syscall

#elif __i386__
        movl $__NR_write, %eax
        movl $STDOUT, %ebx
        movl $program_name, %ecx
        movl $program_name_length, %edx
        int $0x80
#endif


#ifdef __x86_64__
        popq %rcx                       # argc
#elif __i386__
        popl %ecx
#endif


argv:
#ifdef __x86_64__
        popq %rsi                       # argv
        test %rsi, %rsi
        jz exit                         # exit if last (NULL) argument string
#elif __i386__
        popl %ecx
        jecxz exit
#endif


#ifdef __x86_64__
        movq %rsi, %rdx
#elif __i386__
        movl %ecx, %ebx
        xorl %edx, %edx
#endif


strlen:
#ifdef __x86_64__
        lodsb
#elif __i386__
        movb (%ebx), %al
        inc %edx
        inc %ebx
#endif

        test %al, %al
        jnz strlen                      # continue if not end of string



#ifdef __x86_64__
        movb $0x0A, -1(%rsi)            # replace NUL-byte with \n

        subq %rdx, %rsi                 # calculate buffer size
        xchg %rdx, %rsi                 # reorder for syscall conventions
        movq $__NR_write, %rax
        movq $STDOUT, %rdi              # file descriptor
        syscall
#elif __i386__
        movb $0x0A, -1(%ebx)           # replace NUL-byte with \n

        movl $__NR_write, %eax
        movl $STDOUT, %ebx
        int $0x80
#endif

        jmp argv                        # process next argument


exit:
#ifdef __x86_64__
        movq $__NR_exit, %rax
        movl exit_code, %edi
        syscall

#elif __i386__
        movl $__NR_exit, %eax
        movl exit_code, %ebx
        int $0x80
#endif
```

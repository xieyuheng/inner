---
title: play with fasm
---

## about syscall

32-bits:
define sys_5_edi edi
define sys_4_esi esi
define sys_3_edx edx
define sys_2_ecx ecx
define sys_1_ebx ebx
define sys_n_eax eax
int 80h

64-bits:
1. syscall numbers:
   /usr/include/asm/unistd_64.h (in archlinux)
   /usr/include/asm/unistd_32.h (for x86)
2. syscall parameters order:
   r9    ; 6th parameter
   r8    ; 5th parameter
   r10   ; 4th parameter
   rdx   ; 3rd parameter
   rsi   ; 2nd parameter
   rdi   ; 1st parameter
   rax   ; syscall_number
   syscall
   return register:
   rax   ; 1st
   rdx   ; 2nd
   preserved accross function call: rbx rbp esp r12 r13 r14 r15
3. functions parameter (when linked with external libraries):
   r9    ; 6th param
   r8    ; 5th param
   rcx   ; 4th param
   rdx   ; 3rd param
   rsi   ; 2nd param
   rdi   ; 1st param
   call <library>

## kkk

:tangle ./fasm-play/kkk.fasm
```fasm
format elf64 executable 3

;; parameters order of linux syscall:
define sys_6_r8  r8
define sys_5_r9  r9
define sys_4_r10 r10
define sys_3_rdx rdx
define sys_2_rsi rsi
define sys_1_rdi rdi
define sys_n_rax rax

segment readable executable
entry display_massage

display_massage:

        push rdi
        mov sys_3_rdx,  massage_size
        mov sys_2_rsi,  massage
        mov sys_1_rdi,  1 ;; stdout
        mov sys_n_rax,  1 ;; sys_write
        syscall

        pop sys_1_rdi     ;; exit code argc
        mov sys_n_rax, 60 ;; sys_exit
        syscall

segment readable writeable
massage   db "kkk took my baby away !", 10
massage_size = ($ - massage)
```

## 用退出码返回数组的数中最大的数

要想打印数字就要作编码转换
:tangle ./fasm-play/max-of-array.fasm
```fasm
;;; purpose:
;;;        finds the maximum number in data items array
;;;
;;; variables:
;;;        edi - holds the index of the data item being examined
;;;        ebx - largest data item found
;;;        eax - current data item
;;;        data_items: contains the item data, 0 is used to terminate the data

format elf64 executable 3

;; parameters order of syscall:
define sys_6_r8  r8
define sys_5_r9  r9
define sys_4_r10 r10
define sys_3_rdx rdx
define sys_2_rsi rsi
define sys_1_rdi rdi
define sys_n_rax rax

segment readable executable
entry start
start:
        mov r8, 0
        mov rax, [data_items + r8*8]
        mov rbx, rax

        ;; 上面在于要准备好循环开启时的三个寄存器的状态
        ;; 每次要进入循环的时候 都对三个寄存器的状态有约定:
        ;; r8  : 当前指向的数的索引
        ;; rax : 当前指向的数
        ;; rbx : 在比较rax之前 所知道的最大的数

start_loop:
        cmp rax, 0
        je print_and_exit

        inc r8
        mov rax, [data_items + r8*8]

        cmp rax, rbx
        ;;  rax <= rbx
        jle start_loop
        ;;  rax > rbx
        mov rbx, rax
        jmp start_loop


print_and_exit:
        mov sys_1_rdi, rbx
        mov sys_n_rax, 60
        syscall

segment readable
data_items dq 3,67,34,222,45,75,54,34,44,33,22,11,66,0
```

## create_new_file.fasm

```fasm :tangle create-new-file.fasm
syscall_read  = 0
syscall_write = 1
syscall_open  = 2
syscall_close = 3
syscall_exit  = 60

STDIN  = 0
STDOUT = 1
STDERR = 2

open_read         = 0
open_write        = 1
open_readAndWrite = 2

open_creat      = 0100o
open_rewrite    = 1000o ;; rewrite if file exist
open_append     = 2000o

open_excl       = 0200o ;; ensure that THIS call creates the file
open_noctty     = 0400o
open_nonblock   = 4000o
open_nondelay   = open_nonblock
open_sync       = 10000o
open_async      = 20000o
open_direct     = 40000o
    ;; to minimize cache effects of the I/O to and from this file.
open_largefile  = 100000o
open_directory  = 200000o
open_nofollow   = 400000o ;; If pathname is a symbolic link, then the open fails.



format elf64 executable 3

;; parameters order of syscall:
define sys_6_r8  r8
define sys_5_r9  r9
define sys_4_r10 r10
define sys_3_rdx rdx
define sys_2_rsi rsi
define sys_1_rdi rdi
define sys_n_rax rax

;; parameters order of call <fun>:
define fun_6_r9  r9
define fun_5_r8  r8
define fun_4_rcx rcx
define fun_3_rdx rdx
define fun_2_rsi rsi
define fun_1_rdi rdi

segment readable executable
entry create_new_file

create_new_file:

        mov     sys_3_rdx, input_buffer_size
        mov     sys_2_rsi, input_buffer
        mov     sys_1_rdi, STDIN
        mov     sys_n_rax, syscall_read
        syscall

        mov     qword [input_size], rax

        mov     sys_3_rdx, 110100100b
        mov     sys_2_rsi, open_readAndWrite or open_creat or open_rewrite
        mov     sys_1_rdi, filename
        mov     sys_n_rax, syscall_open
        syscall

        mov     qword [file_handle], rax

        mov     sys_3_rdx, qword [input_size]
        mov     sys_2_rsi, input_buffer
        mov     sys_1_rdi, qword [file_handle]
        mov     sys_n_rax, syscall_write
        syscall

        mov     sys_1_rdi, qword [file_handle]
        mov     sys_n_rax, syscall_close
        syscall


        mov     sys_1_rdi, 0
        mov     sys_n_rax, syscall_exit
        syscall

segment readable writeable
filename     db 'kkk',0

input_buffer_size = 1000h
input_buffer rb input_buffer_size

input_size   rq 1
file_handle  rq 1
```

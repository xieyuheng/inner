---
title: real mode
---

## 指令和数据没有语法上的区别(都是二进制数) 只有语义上的区别

机器把某解二进制数理解为指令 把另一些二进制数理解为数据
就像你把我所说的某一些音节理解位动词 而把另一些音节理解为名词
我所说的音节流 有的被你理解为祈使句 有的被你理解疑问句
当然我所说的音节流 在你的理解中所可能形成的语义是非常丰富的
计算机对二进制数的理解未尝不是如此
但是对计算机而言要形成更丰富的语义就要用编译器来作一些抽象

算法和数据在更高层次上也能没有区别
比如对lisp而言算法是sexp数据也是sexp
比如对digrap而言算法是gexp数据也是gexp

## 总线 与 CPU存储器(内存)的读写

总线:
1. 地址总线
   64根地址总线所能寻址的内存大小为2^64 bytes
2. 数据总线
   其宽度决定了数据在CPU和内存之间的传送速度
3. 控制总线
   是很多控制总线的集合
   有多少根控制总线 CPU就提供了对其他硬件的多少种控制

## 设备

每个设备都有相应的存储器
至少有只读的存储器(ROM)来储存支持基本输入输出的软件(BIOS)
这些存储器被CPU一视同仁
CPU在控制硬件的时候把它们当作内存来待

## 十六位的限制带来有趣而无奈的寻址方式

### cs:ip

real mode:
1. 从CS:IP寻址以fetch当前需要被执行的指令到buffer
2. IP = IP + (指令长度)
3. 执行指令
4. loop
对8086而言CS:IP的初值如下:
f000:ffff

long mode:
1. RIP寻址以fetch当前需要被执行的指令到buffer
2. RIP = RIP + (指令长度)
3. 执行指令
4. loop

### jmp

为了简化程序员对CPU的控制
设计者规定程序员只能通过读写CPU中的寄存器来控制CPU
但是不能用mov来读写CS,DS这类段寄存器
jmp是最简单的修改CS:IP的指令了
jmp指令是别的(并不合法的)指令的指令糖

具体的各种转跳方式比较复杂

### loop <label>

(cx)代表循环数

```
loop <label>
==
(cx) = (cx) - 1
if (cx) =/= 0
jmp <label>
else
go next
```

### ret

```
ret
==
pop ip
==
(ip) = ((ss) * 16 + (sp))
(sp) = (sp) + 2
+ real mode中栈的单位是word
+ ((ss) * 16 + (sp)) = [ss:sp]
```

### retf

```
retf
==
pop ip
pop cs
==
(ip) = ((ss) * 16 + (sp))
(sp) = (sp) + 2
(cs) = ((ss) * 16 + (sp))
(sp) = (sp) + 2
```

+ real mode中栈的单位是word

### call

real mode:

```
call <label>
==
push ip
jmp near <label>
==
(sp) = (sp) - 2
((ss) * 16 + (sp)) = (ip)
(ip) = (ip) + <16位二进制符号数表示的位移(在编译时算出)>

call far <label>
==
push cs
push ip
jmp far <label>
==
(sp) = (sp) - 2
((ss) * 16 + (sp)) = (cs)
(sp) = (sp) - 2
((ss) * 16 + (sp)) = (ip)
(cs) = <label>的段地址
(ip) = <label>的偏移地址

call <16位寄存器>
==
push ip
jmp <16位寄存器>
==
(sp) = (sp) - 2
((ss) * 16 + (sp)) = (ip)
(ip) = (<16位寄存器>)

call word <内存单元地址>
```

### DS

```
DS = f000时
mov al, [f]
==
mov al, [f000f]
```

### SS:SP (栈顶地址)

1. 在real mode下栈的单位是word
   而在long mode下栈的单位是4 words == 8 bytes
2. 入栈之后栈顶地址减小
3. 栈是空的的时候SS:SP为栈顶的地址的下一个地址(栈顶)
4. 没有栈底寄存器来保护你
5. real mode:
   ```
   push <val>
   ==
   (sp) = (sp) - 2
   ((ss) * 16 + (sp)) = <val>

   pop <memory>
   ==
   (sp) = (sp) + 2
   <memory> = ((ss) * 16 + (sp))
   ```

## TODO

```
0xe9 / JMP

*(PDWORD)( Opcode + 1 ) = ( destination - origin - 5)

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Short jumps:

Opcodes:

0x70 / JO
0x71 / JNO
0x72 / JB/JC/JNAE
0x73 / JAE/JNB/JNC
0x74 / JE/JZ
0x75 / JNE/JNZ
0x76 / JBE/JNA
0x77 / JA/JNBE
0x78 / JS
0x79 / JNS
0x7a / JP/JPE
0x7b / JNP/JPO
0x7c / JL/JNGE
0x7d / JGE/JNL
0x7e / JLE/JNG
0x7f / JG/JNLE
0xeb / JMP

If the destination is lower than origin:

// ( *PBYTE( Opcode + 1 ) >= 0x80 )

*PBYTE( Opcode + 1 ) = ~( ( origin - destination ) + 1 );


If the is origin lower than destination:

// ( *PBYTE( Opcode + 1 ) <= 0x7f )

*PBYTE( Opcode + 1 ) = ( destination - origin ) - 2;

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Near jumps:

Opcodes:

0x0f 0x80 / JO
0x0f 0x81 / JNO
0x0f 0x82 / JB/JC/JNAE
0x0f 0x83 / JAE/JNB/JNC
0x0f 0x84 / JE/JZ
0x0f 0x85 / JNE/JNZ
0x0f 0x86 / JBE/JNA
0x0f 0x87 / JA/JNBE
0x0f 0x88 / JS
0x0f 0x89 / JNS
0x0f 0x8a / JP/JPE
0x0f 0x8b / JNP/JPO
0x0f 0x8c / JL/JNGE
0x0f 0x8d / JGE/JNL
0x0f 0x8e / JLE/JNG
0x0f 0x8f / JG/JNLE

If the destination is lower than origin:

// ( *(PDWORD)( Opcode + 2 ) < 0 )


*(PDWORD)( Opcode + 2 ) = ~( ( origin - destination ) + 5 );


If the is origin lower than destination:

// ( *(PDWORD)( Opcode + 2 ) > 0 )


*(PDWORD)( Opcode + 2 ) = ( ( destination - origin ) - 6 );

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Near Relative Jump:

Opcode:

0xe9 / JMP

*(PDWORD)( Opcode + 1 ) = ( destination - origin - 5);

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Near relative call:

Opcode:

0xe8 / CALL

*(PDWORD)( Opcode + 1 ) = ( ( destination - origin ) - 4 )
```

## and & or

可以看成是对二进制数的过滤:
1. and
   一个是筛子 另一个是被筛的数
   就筛子而言 1是可以漏过的孔
2. or
   一个是筛子 另一个是被筛的数
   就筛子而言 0是可以漏过的孔

## 设备端口 (port)

(CPU可以直接读写的三个地方之一)
0 -- 65535
in out
在保护模式下这些命令不能被直接使用

## 中断

中断是CPU所提供的全局例外机制
CPU接收到中断信息(中断类型码)之后
查中断向量表
找相应的中断处理程序(的第一条指令的地址)去处理

## CUP指令总结:

1. 数据传送指令:
   mov push pop pushf popf xchg
2. 算数运算指令:
   inc dec add sub cmp mul div
   adc sbb imul idiv aaa
3. 逻辑运算指令:
   not(not不影响flag)
   and or xor
   shl shr
   sal sar
   rol ror
   rcl rcr
   test
4. 转移指令:
   jmp
   jcxz je jb ja jnb jna
   loop
   call ret
   int iret
5. 处理机控制指令(flag控制指令):
   cld std cli sti nop clc cmc stc hlt wait esc lock
6. 串处理指令:
   对内存中的数据进行批量处理
   movsb movsw cmps scas lods stos
   + 相关的前缀指令:
     rep repe repne

## fasm的作者给出的自然数,整数,有理数,实数(能否作为实数的模型我还不确定)的补码模型

或者说是无穷0,1序列对自然数,整数,有理数,(可能还有实数)的编码
这种编码对小数的表示是不利用浮点的
1. 对自然数的表示,与加法运算的算法:
   只有有限个位置上不是0的0,1数列代表了所有自然数
   加法运算的算法是简单,只要注意进位就行了,当进位出现时进位数只能是1
2. 对整数的表示,与减法运算:
   定义减法运算为加法的逆运算
   + 减法作为加法的逆运算,就是加法所能形成的一种方程的一种解
     也就是说还是以这个模型中的加法运算的算法为核心的
   + 因为加法有交换性所以只有一个逆运算
   然后就减法在自然数集中的不完备性把自然数集扩展为整数集,这在于:
   1.
   在自然数集中不总是存在方程的解
   2. 当自然数集中不存在方程的解的时候
      加法运算的算法依然有效
      因为就这个模型而言加法算法的适用性本身就超出自然数的加法的语义
      + `相加'的可以是任何两个1,0序列间进行
      + 在1,0序列的集合内,方程总是存在唯一解
      不过这种断言某种程度上是先验的
   3. 既然使用的都是同样的加法算法
      那么很容易发现新数与旧数就运算而言相容
      还是以就原来模型的加法运算为核心的
   当有了负数的表示之后
   减法就可以用加法的算法来一致的计算
   这就相当于对于方程给出了求解公式
   + 当位数有限时 符号数的减法可以通过把减数化为负数然后再作加法来实现
     当位数有限时 对于无符号数减法是用机器提供的sub来作的
3. 对分数表示,与乘法运算和除法运算:
   这类模型下的乘法总要有乘法表,二进制下的乘法表是最简单的了
   因为乘法表非常简单,所以乘法是如何作为连加的在这里就表现的特别明显
   除法也用定义为逆运算,即用方程定义
   很容易就发现不考虑零时方程是可解的并且解是唯一的
4. >< 下面说一下这个模型的特点
   1. 最重要的是要明白
      这个模型的给出是为了帮助人们理解符号数在计算机中的表示
      首先要理解当位数有限的时候的进,借位
      其次要理解负数的这种特殊表示使得加法的运算可以以一致的进行
   2. 在这个模型下`1/2'+`1/2'不等于`1'
      或者说所得到的结果给出了1的另以中表示方式
      那么这样每个数就失去了表示上的唯一性了
   3. 分数之间的序关系是不能很好的被明显看出来的
      带浮点的分数模型就能很方便的看出序关系
      但是带一个分数线的序关系也是不容易看出来序关系
   4. 高次的代数方程可能是没有解的
      比如等号二就不能在这种模型下表示

## flag寄存器

real mode:
| 15 | 14 | 13 | 12 | 11 | 10 |  9 |  8 |  7 |  6 | 5 |  4 | 3 |  2 | 1 |  0 |
|----+----+----+----+----+----+----+----+----+----+---+----+---+----+---+----|
|    |    |    |    | of | df | if | tf | sf | zf |   | af |   | pf |   | cf |
flag总是记录CPU的算数运算指令和逻辑运算指令之后的结果
只影响flag的指令 和 条件转跳 共同形成着谓词语义
1. zf
   结果是否为0
2. pf
   结果作为二进制数其中的1的个数是否为偶数
   比如可以被用于奇偶校准
3. sf
   结果作为符号数是否为负数
   即最高位是否为1
4. cf
   无符号运算是否进位或借位
   不是一个简单的对运算结果的谓词了
   借位与负数的语义虽然不同 但是语法是重叠的
   要知道
   当一次减法运算最后还是需要借位的时候
   所得的结果就是负数了
5. of
   符号数运算是否溢出
   溢出就是把符号位给侵占了
   机器的计算总是对无符号数而言的 形成有符号数的语义就在于这个flag
6. df
   代表串传送指令的方向
   影响字符串操作

cmp(最常用的改变flag信息的指令):
因为两个整数在整数全序集中的大于小于关系可以被化归为
1. 两个整数的差对0的大于小于关系
2. 或者两个整数的商(已经出离整数集了)对1的大于小于关系
3. 等等
所以不保存值的减法cmp与zf与cf一起就构成了比较大小的谓词
+ 在作减法的时候intel的语法相比AT&T的语法的直观性就体现出来了
cmp rax, rbx
下面就可以用flag来区分语义了,非常有趣:
对无符号数的比较:
+ rax  == rbx : zf == 1
+ rax =/= rbx : zf == 0
+ rax  <  rbx : cf == 1
+ rax  >= rbx : cf == 0
+ rax  >  rbx : cf == 0 且 zf == 0 (利用'且',从上面的>=中剔除=的情况)
+ rax  <= rbx : cf == 1 或 zf == 1 (利用'或',给上面的<添加商=的情况)
对符号数的比较:
+ rax  == rbx : zf == 1
+ rax =/= rbx : zf == 0
+ rax  <  rbx :
  下面的例子中有sf == 1但是不足以说明rax  <  rbx
  rax = 00100010, rbx = 10100000
  rax - rbx = 10000010
  这个数的算出是解方程而得到的,可以带回去验算
  如果验算时很容易理解什么是符号数的溢出
  即 可以用验算来判断是否符号位溢出
  或者直接把符号位溢出简单的理解为所得的整数结果超出了可表示的范围
+ 所以稍微复杂的有:
  + of == 0 时:
    sf == 1 --> rax  <  rbx
    sf == 0 --> rax  >= rbx
  + of == 1 时:(相等的时候不可能溢出)
    sf == 1 --> rax  >  rbx
    sf == 0 --> rax  <  rbx

常用的利用到flag信息的指令(形成一些关键的语义全靠这些指令了):
1. adc sbb
   考虑逐位地(或逐段地,比如逐8位)对加法的计算就很容易理解
   即 对应的位上的每一对数在相加的同时还要加上或减去之前的进位与借位的1
   ```fasm
   1ef000h + 201000h
   mov ax, 001eh
   mov bx, 0f000h
   add bx, 1000h
   adc ax, 0020h
   ```
条件转移指令(修改cs:ip或rip的指令):
1. jcxz
   转移条件:cx(ecx,rcx) == 0
2. 针对无符号数的比较(检验zf,cf):
   | je  | equal     | ==  | zf == 1            |
   |-----+-----------+-----+--------------------|
   | jne | not equal | =/= | zf == 0            |
   |-----+-----------+-----+--------------------|
   | jb  | below     | <   | cf == 1            |
   |-----+-----------+-----+--------------------|
   | jnb | not below | >=  | cf == 0            |
   |-----+-----------+-----+--------------------|
   | ja  | above     | >   | cf == 0 且 zf == 0 |
   |-----+-----------+-----+--------------------|
   | jna | not above | <=  | cf == 1 或 zf == 1 |
3. 针对有符号数的比较(检验zf,sf,of):
   TODO

# differences between real-mode and protected-mode

|                      | Real Mode                  | 16-bit Protected Mode    | 32-bit Protected Mode             |
|----------------------|----------------------------|--------------------------|-----------------------------------|
| Segment base address | 20-bit (1M byte range)     | 24-bit (16M byte range), | 32-bit (4G byte range),           |
|                      | i.e. 16 * segment register | from descriptor          | from descriptor                   |
|----------------------|----------------------------|--------------------------|-----------------------------------|
| Segment size (limit) | 16-bit, 64K bytes (fixed)  | 16-bit, 1-64K bytes      | 20-bit, 1-1M bytes or 4K-4G bytes |
|----------------------|----------------------------|--------------------------|-----------------------------------|
| Segment protection   | no                         | yes                      | yes                               |
|----------------------|----------------------------|--------------------------|-----------------------------------|
| Segment register     | segment base address / 16  | selector                 | selector                          |
|----------------------|----------------------------|--------------------------|-----------------------------------|

In protected mode, besides the segment base address,
we also need the segment size (limit)
and some flags indicating what the segment is used for.
This information goes into an 8-byte data structure called a descriptor:

|--------------|------------|----------|-----------|------------|--------|--------------------|--------------|
| Lowest bytes | Byte 1     | Byte 2   | Byte 3    | Byte 4     | Byte 5 | Byte 6             | Highest byte |
|--------------|------------|----------|-----------|------------|--------|--------------------|--------------|
| Limit 7:0    | Limit 15:8 | Base 7:0 | Base 15:8 | Base 23:16 | Access | Flags, Limit 19:16 | Base 31:24   |
|--------------|------------|----------|-----------|------------|--------|--------------------|--------------|

This is a 32-bit ('386) descriptor.
For 16-bit ('286) descriptors,
the top two bytes (Limit 19:16, Flags, and Base 31:24) are zero.
The Access byte indicates segment usage (data segment, stack segment, code segment, etc.):

|-------------|-----------|-------|------------|--------------------------------|-------------------|------------|
| Highest bit | Bits 6, 5 | Bit 4 | Bits 3     | Bit 2                          | Bit 1             | Lowest bit |
|-------------|-----------|-------|------------|--------------------------------|-------------------|------------|
| Present     | Privilege | 1     | Executable | Expansion direction/conforming | Writable/readable | Accessed   |
|-------------|-----------|-------|------------|--------------------------------|-------------------|------------|

* What's a selector?
  In protected mode, the segment registers contain selectors,
  which index into one of the descriptor tables.
  Only the top 13 bits of the selector are used for this index.
  bit-2 choses between the GDT and LDT.
  bit-0 and bit-1 of the selector set a privilege value.

* How do I enter protected mode?
  Entering protected mode is actually rather simple, and is is described in many other tutorials. You must:
  1. Create a valid Global Descriptor Table (GDT), and create the 6-byte 'pseudo descriptor' pointing to the GDT
  2. Disable interrupts
  3. LGDT. The operand of this instruction points to the GDT pseudo-descriptor, which in turn points to the GDT
  4. Set the PE bit in the MSW register
  5. Load all data segment registers with valid selectors
  6. Do a far jump (load both CS and IP/EIP) to load CS and enter pmode

* What pitfalls have you encountered?
  1. You must pay extreme attention to detail here.
     One wrong bit will make things fail.
     Protected mode errors often triple-fault the CPU, making it reset itself.
     Be prepared to see this happen again and again.
  2. Most library routines probably won't work.
     printf(), for example, won't work
     because it evenutally calls either a DOS or BIOS service
     to put text on the screen.
     Unless you have a DOS extender,
     these services are unavailable in protected mode.
     I had good luck using sprintf() to put formatted text in a buffer,
     which I then wrote to the screen with my own protected-mode routine.
  3. Before clearing the PE bit,
     the segment registers must point to descriptors that are appropriate to real mode.
     This means a limit of exactly 0xFFFF (see other restrictions above).
     One of my demo programs had ES pointing to a text-video segment.
     With a limit of 0xFFFF, things worked well. With a limit of 3999 (80 * 25 * 2 - 1),
     the system froze up after returning to real mode and trying to use the ES register.
     - Actually, for DS, ES, FS and GS, the segment limit must be 0xFFFF or greater.
       If you give the segment a limit of 0xFFFFF and make it page-granular,
       you can access up to 4G of memory from real mode -- this is unreal mode.
       However, limits other than 0xFFFF (or page-granularity) for CS or SS cause big problems in real mode.
  4. You can not use the '286 LMSW instruction to clear the PE bit.
     Use MOV CR0, nnn.
     (On the '286 CPU, the only way to return to real mode is to reset the CPU!)
  5. Load all segment registers with valid selectors after entering protected mode.
     I forgot to do this with ES.
     A protected-mode routine pushed ES, loaded it with a valid selector, and used it.
     When it tried to pop the old, invalid (real-mode) selector back into ES, it crashed.
  6. The IDTR must also be reset to a value that is appropriate to real-mode
     before re-enabling interrupts (see above).
  7. Not all instructions are legal in real mode.
     If you attempt to use task state segments for multitasking,
     note that executing the LTR instruction in real-mode will cause an illegal instruction interrupt.
  8. Descriptor tables in ROM? Section 10.4.3 of 386INTEL.TXT states
     - The GDT (as well as LDTs) should reside in RAM,
       because the processor modifies the accessed bit of descriptors.
     However, one of my sources (thanks Vinay) states that
     later CPUs will not attempt to set the Accessed bit in a descriptor
     if that bit is already set.
     Check the docs for the CPU you are using.
  9. The naive code described here will crash if the PC is in Virtual 8086 (V86) mode.
     This is a fourth mode of operation found on the 386 CPU,
     with addressing similar to real mode but some of the protection mechanisms of protected mode.
     You may know that a Windows (or OS/2, or Linux) DOS box runs in V86 mode,
     but you may not realize that memory managers such as EMM386 also put the CPU in V86 mode.
* If you want to start simple, try these tips:
  1. Don't worry about returning to real mode. Use the reset button :)
  2. Leave interrupts disabled.
  3. Don't use an LDT.
  4. Put only four descriptors in the GDT: null, code, stack/data, and linear data (base address = 0).
  5. Set the segment bases to real-mode values
     i.e. 16 * real-mode segment register value.
     This lets you address variables in the same way in both real and protected modes.
  6. Set all segment limits to their maximum.
  7. Leave all privilege values set to 0 (Ring 0, highest privilege).
  8. Before each step of switching to pmode,
     poke a character into video memory, to see (literally!) how far the code gets.
     Text-mode VGA memory starts at address 0B8000h.

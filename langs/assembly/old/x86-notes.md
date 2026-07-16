---
title: x86-notes
---

## the 8 named general purpose registers

1. eax : accumulator register. used in arithmetic operations.
2. ecx : counter register. used in shift/rotate instructions.
3. edx : data register. used in arithmetic operations and i/o operations.
4. ebx : base register. used as a pointer to data (located in ds in segmented mode).
5. esp : stack pointer register. pointer to the top of the stack.
6. ebp : stack base pointer register. used to point to the base of the stack.
7. esi : source register. used as a pointer to a source in stream operations.
8. edi : destination register. used as a pointer to a destination in stream operations.

## the 6 segment registers

1. SS : Stack Segment. Pointer to the stack.
2. CS : Code Segment. Pointer to the code.
3. DS : Data Segment. Pointer to the data.
4. ES : Extra Segment. Pointer to extra data. ('E' stands for "Extra")
5. FS : F Segment. Pointer to more extra data. ('F' comes after 'E')
6. GS : G Segment. Pointer to still more extra data. ('G' comes after 'F')

Most applications on most modern operating systems
(like Linux or Microsoft Windows)
use a memory model that points nearly all segment registers
to the same place (and uses paging instead)
effectively disabling their use
Typically FS or GS is an exception to this rule
to be used to point at thread-specific data

## the EFLAGS Register (这个寄存器有特殊的读写方式)

0: CF: Carry Flag
Set if the last arithmetic operation
carried (addition) or borrowed (subtraction) a bit
beyond the size of the register
This is then checked when
the operation is followed with an add-with-carry or subtract-with-borrow to deal with
values too large for just one register to contain

2: PF: Parity Flag
Set if the number of set bits in the least significant byte is a multiple of 2

4: AF: Adjust Flag
Carry of Binary Code Decimal (BCD) numbers arithmetic operations

6: ZF: Zero Flag
Set if the result of an operation is Zero (0)

7: SF: Sign Flag
Set if the result of an operation is negative

8: TF: Trap Flag
Set if step by step debugging

9: IF: Interruption Flag
Set if interrupts are enabled.

10: DF: Direction Flag
Stream direction
If set, string operations will decrement their pointer
rather than incrementing it, reading memory backwards.

11: OF: Overflow Flag
Set if signed arithmetic operations result in a value too large
for the register to contain

12-13: IOPL: I/O Privilege Level field (2 bits)
I/O Privilege Level of the current process

14: NT: Nested Task flag
Controls chaining of interrupts. Set if the current process is
linked to the next process

16: RF: Resume Flag
Response to debug exceptions

17: VM: Virtual-8086 Mode
Set if in 8086 compatibility mode

18: AC: Alignment Check
Set if alignment checking in of memory references are done

19: VIF: Virtual Interrupt Flag
Virtual image of IF

20: VIP: Virtual Interrupt Pending flag
Set if an interrupt is pending

21: ID: Identification Flag
Support for CPUID instruction if can be set

Table 2.1  Conditions
|----------+-----------------------+-----------------------|
| Mnemonic | Condition tested      | Description           |
|----------+-----------------------+-----------------------|
| o        | OF = 1                | overflow              |
|----------+-----------------------+-----------------------|
| no       | OF = 0                | not overflow          |
|----------+-----------------------+-----------------------|
| c        |                       | carry                 |
| b        | CF = 1                | below                 |
| nae      |                       | not above nor equal   |
|----------+-----------------------+-----------------------|
| nc       |                       | not carry             |
| ae       | CF = 0                | above or equal        |
| nb       |                       | not below             |
|----------+-----------------------+-----------------------|
| e        | ZF = 1                | equal                 |
| z        |                       | zero                  |
|----------+-----------------------+-----------------------|
| ne       | ZF = 0                | not equal             |
| nz       |                       | not zero              |
|----------+-----------------------+-----------------------|
| be       | CF or ZF = 1          | below or equal        |
| na       |                       | not above             |
|----------+-----------------------+-----------------------|
| a        | CF or ZF = 0          | above                 |
| nbe      |                       | not below nor equal   |
|----------+-----------------------+-----------------------|
| s        | SF = 1                | sign                  |
|----------+-----------------------+-----------------------|
| ns       | SF = 0                | not sign              |
|----------+-----------------------+-----------------------|
| p        | PF = 1                | parity                |
| pe       |                       | parity even           |
|----------+-----------------------+-----------------------|
| np       | PF = 0                | not parity            |
| po       |                       | parity odd            |
|----------+-----------------------+-----------------------|
| l        | SF xor OF = 1         | less                  |
| nge      |                       | not greater nor equal |
|----------+-----------------------+-----------------------|
| ge       | SF xor OF = 0         | greater or equal      |
| nl       |                       | not less              |
|----------+-----------------------+-----------------------|
| le       | (SF xor OF) or ZF = 1 | less or equal         |
| ng       |                       | not greater           |
|----------+-----------------------+-----------------------|
| g        | (SF xor OF) or ZF = 0 | greater               |
| nle      |                       | not less nor equal    |
|----------+-----------------------+-----------------------|

## flags

### Adjust flag

The Adjust flag (also known as the Auxiliary flag)
is a flag stored in the FLAGS register on all x86 compatible CPUs.
It is bit 4.
It is used to indicate when an arithmetic carry or borrow
has been generated out of the 4 least significant bits.
It is primarily used in BCD arithmetics.

Auxiliary flag is set (AF=1)
if there is a carry from low nibble to high nibble
or a borrow from a high nibble to low nibble
of the low order 8-bit of a 16-bit number.
(for example, in BCD addition or subtraction.)

### The CARRY flag and OVERFLOW flag in binary arithmetic

Do not confuse the "carry" flag with the "overflow" flag in integer
arithmetic.  Each flag can occur on its own, or both together.  The CPU's
ALU doesn't care or know whether you are doing signed or unsigned
mathematics; the ALU always sets both flags appropriately when doing any
integer math.  The ALU doesn't know about signed/unsigned; the ALU just
does the binary math and sets the flags appropriately.  It's up to you,
the programmer, to know which flag to check after the math is done.

If your program treats the bits in a word as unsigned numbers, you
must watch to see if your arithmetic sets the carry flag on, indicating
the result is wrong.  You don't care about the overflow flag when doing
unsigned math.  (The overflow flag is only relevant to signed numbers, not
unsigned.)

If your program treats the bits in a word as two's complement signed
values, you must watch to see if your arithmetic sets the overflow flag
on, indicating the result is wrong.  You don't care about the carry
flag when doing signed, two's complement math.  (The carry flag is only
relevant to unsigned numbers, not signed.)

In unsigned arithmetic, watch the carry flag to detect errors.
In unsigned arithmetic, the overflow flag tells you nothing interesting.

In signed arithmetic, watch the overflow flag to detect errors.
In signed arithmetic, the carry flag tells you nothing interesting.

**English**

Do not confuse the English verb "to overflow" with the "overflow flag"
in the ALU.  The verb "to overflow" is used casually to indicate that
some math result doesn't fit in the number of bits available; it could be
integer math, or floating-point math, or whatever.  The "overflow flag"
is set specifically by the ALU as described below, and it isn't the same
as the casual English verb "to overflow".

In English, we may say "the binary/integer math overflowed the number
of bits available for the result, causing the carry flag to come on".
Note how this English usage of the verb "to overflow" is *not* the same as
saying "the overflow flag is on".  A math result can overflow (the verb)
the number of bits available without turning on the ALU "overflow" flag.

**Carry Flag**

The rules for turning on the carry flag in binary/integer math are two:

1. The carry flag is set if the addition of two numbers causes a carry
   out of the most significant (leftmost) bits added.

   1111 + 0001 = 0000 (carry flag is turned on)

2. The carry (borrow) flag is also set if the subtraction of two numbers
   requires a borrow into the most significant (leftmost) bits subtracted.

   0000 - 0001 = 1111 (carry flag is turned on)

Otherwise, the carry flag is turned off (zero).
* 0111 + 0001 = 1000 (carry flag is turned off [zero])
* 1000 - 0001 = 0111 (carry flag is turned off [zero])

In unsigned arithmetic, watch the carry flag to detect errors.
In signed arithmetic, the carry flag tells you nothing interesting.

**Overflow Flag**

The rules for turning on the overflow flag in binary/integer math are two:

1. If the sum of two numbers with the sign bits off yields a result number
   with the sign bit on, the "overflow" flag is turned on.

   0100 + 0100 = 1000 (overflow flag is turned on)

2. If the sum of two numbers with the sign bits on yields a result number
   with the sign bit off, the "overflow" flag is turned on.

   1000 + 1000 = 0000 (overflow flag is turned on)

Otherwise, the overflow flag is turned off.
* 0100 + 0001 = 0101 (overflow flag is turned off)
* 0110 + 1001 = 1111 (overflow flag is turned off)
* 1000 + 0001 = 1001 (overflow flag is turned off)
* 1100 + 1100 = 1000 (overflow flag is turned off)

Note that you only need to look at the sign bits (leftmost) of the three
numbers to decide if the overflow flag is turned on or off.

If you are doing two's complement (signed) arithmetic, overflow flag on
means the answer is wrong - you added two positive numbers and got a
negative, or you added two negative numbers and got a positive.

If you are doing unsigned arithmetic, the overflow flag means nothing
and should be ignored.

The rules for two's complement detect errors by examining the sign of
the result.  A negative and positive added together cannot be wrong,
because the sum is between the addends. Since both of the addends fit
within the allowable range of numbers, and their sum is between them, it
must fit as well.  Mixed-sign addition never turns on the overflow flag.

In signed arithmetic, watch the overflow flag to detect errors.
In unsigned arithmetic, the overflow flag tells you nothing interesting.

**How the ALU calculates the Overflow Flag**

This material is optional reading.

There are several automated ways of detecting overflow errors in two's
complement binary arithmetic (for those of you who don't like the manual
inspection method).  Here are two:

**Calculating Overflow Flag: Method 1**

Overflow can only happen when adding two numbers of the same sign and
getting a different sign.  So, to detect overflow we don't care about
any bits except the sign bits.  Ignore the other bits.

With two operands and one result, we have three sign bits (each 1 or
0) to consider, so we have exactly 2**3=8 possible combinations of the
three bits.  Only two of those 8 possible cases are considered overflow.
Below are just the sign bits of the two addition operands and result:

```
ADDITION SIGN BITS
num1sign num2sign sumsign
------------------------
0 0 0
*OVER* 0 0 1 (adding two positives should be positive)
0 1 0
0 1 1
1 0 0
1 0 1
*OVER* 1 1 0 (adding two negatives should be negative)
1 1 1
```

We can repeat the same table for subtraction.  Note that subtracting
a positive number is the same as adding a negative, so the conditions that
trigger the overflow flag are:

```
SUBTRACTION SIGN BITS
num1sign num2sign sumsign
:---------------------------
0 0 0
0 0 1
0 1 0
*OVER* 0 1 1 (subtracting a negative is the same as adding a positive)
*OVER* 1 0 0 (subtracting a positive is the same as adding a negative)
1 0 1
1 1 0
1 1 1
```

A computer might contain a small logic gate array that sets the overflow
flag to "1" iff any one of the above four OV conditions is met.

A human need only remember that, when doing signed math, adding
two numbers of the same sign must produce a result of the same sign,
otherwise overflow happened.

**Calculating Overflow Flag: Method 2**

When adding two binary values, consider the binary carry coming into
the leftmost place (into the sign bit) and the binary carry going out
of that leftmost place.  (Carry going out of the leftmost [sign] bit
becomes the CARRY flag in the ALU.)

Overflow in two's complement may occur, not when a bit is carried out
out of the left column, but when one is carried into it and no matching
carry out occurs. That is, overflow happens when there is a carry into
the sign bit but no carry out of the sign bit.

The OVERFLOW flag is the XOR of the carry coming into the sign bit (if
any) with the carry going out of the sign bit (if any).  Overflow happens
if the carry in does not equal the carry out.

Examples (2-bit signed 2's complement binary numbers):

```
11
+01
===
00
```

- carry in is 1
- carry out is 1
- 1 XOR 1 = NO OVERFLOW

```
01
+01
===
10
```

- carry in is 1
- carry out is 0
- 1 XOR 0 = OVERFLOW!

```
11
+10
===
01
```

- carry in is 0
- carry out is 1
- 0 XOR 1 = OVERFLOW!

```
10
+01
===
11
```

- carry in is 0
- carry out is 0
- 0 XOR 0 = NO OVERFLOW

Note that this XOR method only works with the *binary* carry that goes
into the sign *bit*.  If you are working with hexadecimal numbers, or
decimal numbers, or octal numbers, you also have carry; but, the carry
doesn't go into the sign *bit* and you can't XOR that non-binary carry
with the outgoing carry.

Hexadecimal addition example (showing that XOR doesn't work for hex carry):

```
8Ah
+8Ah
====
14h
```

The hexadecimal carry of 1 resulting from A+A does not affect the
sign bit.  If you do the math in binary, you'll see that there is
*no* carry *into* the sign bit; but, there is carry out of the sign
bit.  Therefore, the above example sets OVERFLOW on.  (The example
adds two negative numbers and gets a positive number.

### sign flag

In a computer processor the negative flag or sign flag is
a single bit in a system status (flag) register
used to indicate
whether the result of the last mathematical operation
resulted in a value whose most significant bit was set.
In a two's complement interpretation of the result,
the negative flag is set if the result was negative.

The negative flag is set according to the result
in the x86 series processors
by the following instructions: (referring to the Intel 80386 manual)
1. All arithmetic operations except multiplication and division
2. compare instructions
3. Logical instructions - XOR, AND, OR
4. TEST instructions

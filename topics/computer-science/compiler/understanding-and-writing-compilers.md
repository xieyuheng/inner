---
title: understanding and writing compilers
author: Richard Bornat
---

# I Modular Organisation of Compilers

## 1 Phases and Passes

- the parse tree :
  shows the structure of the program text,
  how the fragments of program join together
  to make up larger fragments,
  how those join to make still larger fragments and so on.

- the symbol table :
  provides a correlation between
  all the different occurrences of each name
  throughout the program
  and hence provides a link
  between each name and its declaration,
  whether implicit or explicit, in the source text.

- These two information structures
  are the most important milestones in the compilation process.

- Compilation as a whole is an activity
  which first builds up information structures
  which describe how the program may be broken into fragments
  and how these fragments inter-relate,
  then extracts from these structures
  the information which is required
  in order to translate the program.

## 2 Introduction to Translation

## 3 Introduction to Syntax Analysis

## 4 Lexical Analysis and Loading

# II Translation and Crucial Code Fragments

## 5 Translating Arithmetic Expressions

## 6 Translating Boolean Expressions

## 7 Translating Statements and Declarations

## 8 Creating and Using the Symbol Table

## 9 Accessing an Element of a Data Structure

## 10 Code Optimisation

# III Run-time Support

## 11 Procedure Call and Return

## 12 Arguments and Parameters

## 13 Environments and Closures

## 14 Efficiency, Heaps and Lifetimes

# IV Parsing Algorithms

## 15 Notation and Formal Language Theory

## 16 Top-down Syntax Analysis

## 17 Operator-Precedence Analysis of Expressions

## 18 LR(1) Syntax Analysis

# V Interpreting and Debugging

## 19 Interpreters and Interpretation

## 20 Run-time Debugging Aids

# A The BCPL language

# B Assembly code used in examples

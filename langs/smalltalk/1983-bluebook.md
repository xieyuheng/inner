---
title: bluebook
year: 1983
---

# info

Smalltalk-80
The Language and its Implementation
by Adele Goldberg and David Robson

# intro

part one
semantic and syntax
the smalltalk approach to information representation and manipulation.
object, message, class, instance, method.

part two
objects already in smalltalk

part three
an example application

part four
how the smalltalk-80 virtual machine can be implemented.

# part one

## 1

### objects and messages

### classes and instances

- A class describes the implementation of a set of objects
  that all represent the same kind of system component.

  The individual objects described by a class are called its instances.

  A class describes the form of its instances' private memories
  and it describes how they carry out their operations.

  Even an object that represents a unique system component
  is implemented as the single instance of a class.

- An object's private properties
  are a set of instance variables that make up its private memory,

### an example application

### system classes

### summary of terminology

## 2

### expression syntax

four types of expressions
1. literals such as numebr and string
2. each variables remembers a single object
3. messages
4. blocks are used to implement control structures

## 3

### classes and instances

### protocol descriptions

### implementation descriptions

### variable declarations

### methods

### primitive methods

### summary of terminology

## 4

### subclasses

### subclass descriptions

### an example subclass

### method determination

### abstract superclasses

### subclass framework messages

### summary of terminology

## 5

### metaclasses

With a single metaclass, all classes respond to the message new
by returning an instance whose instance variables all refer to nil.

### initialization of instances

### an example metaclass

### metaclass inheritance

### initialization of class variables

### summary of method determination

### summary of terminology

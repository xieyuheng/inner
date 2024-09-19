---
title: 4. State
---

> Sure, how you specify behavior is most important in coding Smalltalk
> objects, but you still won’t get anywhere without state.  Before
> you start a computation, something has to record what the problem
> is. While you are computing, you often need to store intermediate
> results. When you finish a computation, you have to remember the
> answer.

对于 generic function 的编程风格而言，
也许这一章比上一章更重要。

> Most state-related decisions have more to do with modeling and less
> with coding, so the patterns here don’t tell anything like the
> whole story. However, the tactical decisions you make about
> representation will have an important impact on how well your code
> communicates with others.

> This section talks about two kinds of state: instance variables and
> temporary variables.  Of the two, temporary variables are covered
> much more thoroughly because they are a complete artifact of coding,
> living only as long as a method is computing.  Instance variables
> also have an important role to play in coding, however, so their
> role in coding, and even some of their roles in modeling, is covered
> here.

虽然都是 variables，
但是 instance variables 在于建模，
而 temporary variables 在于编写代码的过程。

# INSTANCE VARIABLES

## Common State
## Variable State
## Explicit Initialization
## Lazy Initialization
## Default Value Method
## Constant Method
## Direct Variable Access
## Indirect Variable Access
## Getting Method
## Setting Method
## Collection Accessor Method
## Enumeration Method
## Boolean Property Setting Method
## Role Suggesting Instance Variable Name

# TEMPORARY VARIABLES

## Temporary Variable
## Collecting Temporary Variable
## Caching Temporary Variable
## Explaining Temporary Variable
## Reusing Temporary Variable
## Role Suggesting Temporary Variable Name

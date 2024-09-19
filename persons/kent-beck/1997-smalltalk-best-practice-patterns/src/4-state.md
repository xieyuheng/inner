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

> I wrote the section on Temporary Variables before I wrote this
> section. I was pleased with how the section on temps came out. I
> expected this section to turn out to be the same sort of
> cut-and-dry, "Here's how it goes" list of patterns. It didn't.

> The problem is that temporary variables really are all about
> coding. They are a tactical solution to a tactical problem. Thus,
> they fit very well in the scope of this book.

可见写书的过程就是思考的过程，
在这个过程中可以学到很多新的东西。

> Most uses of instance variables are not tactical. Along with the
> distribution of computational responsibility, the choice of how to
> represent a model is at the core of modeling. The decision to create
> an instance variable usually comes from a much different mind-set
> and in a different context than the decision to create a temp.

> I leave this section here because there are still important coding
> reasons to create instance variables, and there are some practical,
> tactical techniques to be learned when using instance variables.

越是读下去，越是觉得这本书重要。
又有了「有必要把这本书读完，再写下下一行代码」的感觉。

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

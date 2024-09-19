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

> In the dawn of computing time, state was all there was.

这句话又让我想到了用 resource 的概念来理解所有 HTTP API，
又让人想实现 fidb 这种项目，
外加以 fidb 为基础来实现别的 web app。

> The first thing electronic computing did was make state virtual. No
> longer did it only exist in physical form, that physical form was
> turned into electrons so it could be more easily and quickly
> manipulated. The manipulations were still physically manifested in
> the form of patch cords, but they were getting easier to change.

这也是很不错的见识。

electronic computing 对人类社会的冲击在于，
让 state 变成 virtual 的，
从此人类可以快速操纵 state。

可以说这是对冯诺依曼计算模型的总结。
但是即便是 propagator model，
还是可以理解为快速操纵 state，
只不过 state 分布在 cell 中，
并且操纵是通过预先设定的诸多 propagators 完成的，
并且操纵的方式只能是 patch。

<question>
  How do you represent state,
  different values for which will exist
  in all instances of a class?

  <answer>
    Common State

    Declare an instance variable in the class.

    Be sure to declare instance variables
    in order of importance in the class definition.
  </answer>
</question>

## Variable State

<question>
  TODO

  <answer>
    Variable State

    TODO
  </answer>
</question>

## Explicit Initialization

<question>
  TODO

  <answer>
    Explicit Initialization

    TODO
  </answer>
</question>

## Lazy Initialization

<question>
  TODO

  <answer>
    Lazy Initialization

    TODO
  </answer>
</question>

## Default Value Method

<question>
  TODO

  <answer>
    Default Value Method

    TODO
  </answer>
</question>

## Constant Method

<question>
  TODO

  <answer>
    Constant Method

    TODO
  </answer>
</question>

## Direct Variable Access

<question>
  TODO

  <answer>
    Direct Variable Access

    TODO
  </answer>
</question>

## Indirect Variable Access

<question>
  TODO

  <answer>
    Indirect Variable Access

    TODO
  </answer>
</question>

## Getting Method

<question>
  TODO

  <answer>
    Getting Method

    TODO
  </answer>
</question>

## Setting Method

<question>
  TODO

  <answer>
    Setting Method

    TODO
  </answer>
</question>

## Collection Accessor Method

<question>
  TODO

  <answer>
    Collection Accessor Method

    TODO
  </answer>
</question>

## Enumeration Method

<question>
  TODO

  <answer>
    Enumeration Method

    TODO
  </answer>
</question>

## Boolean Property Setting Method

<question>
  TODO

  <answer>
    Boolean Property Setting Method

    TODO
  </answer>
</question>

## Role Suggesting Instance Variable Name

<question>
  TODO

  <answer>
    Role Suggesting Instance Variable Name

    TODO
  </answer>
</question>

# TEMPORARY VARIABLES

看这里给 temporary variable 分类的架势，
让人想到了亚里士多德给 be 分类，
同样是人们觉得再简单不过的概念，
其分类中都藏着很多知识。

## Temporary Variable

<question>
  TODO

  <answer>
    Temporary Variable

    TODO
  </answer>
</question>

## Collecting Temporary Variable

<question>
  TODO

  <answer>
    Collecting Temporary Variable

    TODO
  </answer>
</question>

## Caching Temporary Variable

<question>
  TODO

  <answer>
    Caching Temporary Variable

    TODO
  </answer>
</question>

## Explaining Temporary Variable

<question>
  TODO

  <answer>
    Explaining Temporary Variable

    TODO
  </answer>
</question>

## Reusing Temporary Variable

<question>
  TODO

  <answer>
    Reusing Temporary Variable

    TODO
  </answer>
</question>

## Role Suggesting Temporary Variable Name

<question>
  TODO

  <answer>
    Role Suggesting Temporary Variable Name

    TODO
  </answer>
</question>

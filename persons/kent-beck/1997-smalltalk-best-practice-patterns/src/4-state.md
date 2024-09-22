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

Kent 在此批判某些 lisper 用 assositive list 作为 object 的行为。

也许只有在实现某个语言的解释器的时候，
这种实现方式才是合理的。

<question>
  How do you represent state whose presence
  varies from instance to instance?

  <answer>
    Variable State

    Put variables that only some instances will have in a Dictionary
    stored in an instance variable called "properties." Implement
    "propertyAt: aSymbol" and "propertyAt: aSymbol put: anObject"
    to access properties.
  </answer>
</question>

## Explicit Initialization

<question>
  How do you initialize instance variables to their default value?

  <answer>
    Explicit Initialization

    Implement a method "initialize"
    that sets all the values explicitly.
    Override the class message "new"
    to invoke it on new instances.
  </answer>
</question>

对于 cicada-lisp 来说，
就是 `(create <class> :key value ...)`。

静态类型语言，一般会直接从语言层面上禁止未初始化的 instances variables 之存在。

## Lazy Initialization

<question>
  How do you initialize an instance variable to its default value?

  <answer>
    Lazy Initialization

    Write a Getting Method for the variable.
    Initialize it if necessary with a Default Value Method.
  </answer>
</question>

```smalltalk
Timer>>count
  count isNil ifTrue: [count := self defaultCount].
  ^count
```

```scheme
(define (count (self timer))
  (if (nil? (self:count))
    (assign! self :count (self:default-count)))
  (self:count))
```

## Default Value Method

<question>
  How do you represent the default value of a variable?

  <answer>
    Default Value Method

    Create a method that returns the value.
    Prepend "default" to the name of the variable
    as the name of the method.
  </answer>
</question>

## Constant Method

> I would rather limit the visibility of constants to a single
> class. Then, that class can provide meaningful behavior to other
> objects.

<question>
  How do you code a constant?

  <answer>
    Constant Method

    Create a method that returns the constant.
  </answer>
</question>

```smalltalk
ListPane>>singleSelect
  self selectionPolicy: 15

"If other methods also needed to know that 15 was the magic constant
for single selection, we create a method for it—a method that just
returns 15."

ListPane>>singleSelectPolicy
  ^15
ListPane>>singleSelect
  self selectionPolicy: self singleSelectPolicy


SelectorFigure>>textColor
  ^Color darkGray
ExpressionFigure>>textColor
  ^Color chartreuse
```

```scheme
(define (single-select (self list-pane))
  (self:selection-policy 15))

;; If other methods also needed to know that 15 was the magic constant
;; for single selection, we create a method for it—a method that just
;; returns 15.

(define (single-select (self list-pane))
  (self:selection-policy (single-select-policy self)))
(define (single-select-policy (self list-pane))
  15)


(define (text-color (self selector-figure))
  color-dark-gray)
(define (text-color (self expression-figure))
  color-chartreuse)
```

## Direct Variable Access

> Accessing state is a topic much like initialization. There are two
> good answers. One is more readable. One is more flexible. Also like
> initialization, you will find dogmatic adherents of both approaches.

我几乎只用 Direct Variable Access 这种风格，
Indirect Variable Access 被我视为是迂腐的。
我得承认我是两个 dogma 中 direct 的一派。

只有当有非平凡的行为的时候，
才需要 explicit getter and setter，
并且必须是 explicit，所以 JS 那种定义 getter 和 setter 的语法，
在我看来也是不可取的。

<question>
  How do you get and set an instance variable's value?

  <answer>
    Direct Variable Access

    Access and set the variable directly.
  </answer>
</question>

## Indirect Variable Access

注意，对于 smalltalk 来说，
direct 与 indirect 之分，
是就一个 class 自身的 scope 而言的，
此时有 `x` 和 `self x` 是有区别的，
前者是 instance variable，
后者是 self message passing。

而我所倾向于的 direct 风格，
是让其他 class 也能读写这个 class 的 instance variables。

<question>
  How do you get and set an instance variable's value?

  <answer>
    Indirect Variable Access

    Access and set its value only through
    a Getting Method and a Setting Method.
  </answer>
</question>

## Getting Method

<question>
  How do you provide access to an instance variable?

  <answer>
    Getting Method

    Provide a method that returns the value of the variable.
    Give it the same name as the variable.
  </answer>
</question>

## Setting Method

<question>
  How do you change the value of an instance variable?

  <answer>
    Setting Method

    Provide a method with the same name as the variable.
    Have it take a single parameter, the value to be set.
  </answer>
</question>

如果能 overload arity，
也许在 generic function 中也可以用这个 pattern。

```smalltalk
Book>>author
  ^author
Book>>title
  ^title

Book>>author: aString
  author := aString
Book>>title: aString
  title := aString
```

```scheme
(define (author (self book))
  (self:author))
(define (title (self book))
  (self:title))

(define (author (self book) (a-string string))
  (assign! self :author a-string))
(define (title (self book) (a-string string))
  (assign! self :title a-string))


(define a-book
  (create book
    :author "Xie Yuheng"
    :title "Cicada Monologues"))

(author a-book)
(author a-book "xyh")

(a-book:author)
(assign! a-book :author "xyh")
```

## Collection Accessor Method

<question>
  How do you provide access to an instance variable
  that holds a collection?

  <answer>
    Collection Accessor Method

    Provide methods that are implemented with
    Delegation to the collection.
    To name the methods, add the name of the collection
    to the collection messages.
  </answer>
</question>

```smalltalk
Department
  superclass: Object
  instance variables: employees totalSalary

totalSalary
  totalSalary isNil ifTrue: [totalSalary := self computeTotalSalary].
  ^totalSalary

computeTotalSalary
  ^employees
    inject: 0
    into: [:sum :each | sum + each salary]

clearTotalSalary
  totalSalary := nil


...aDepartment employees remove: anEmployee...


addEmployee: anEmployee
  self clearTotalSalary.
  employees add: anEmployee

removeEmployee: anEmployee
  self clearTotalSalary.
  employees remove: anEmployee
```

```scheme
(define-class department ()
  :employees (list employee)
  :total-salary (union number nil))

(define (total-salary (self department))
  (if (nil? (self:total-salary))
    (assign! self :total-salary
      (compute-total-salary self)))
  (self:total-salary))

(define (compute-total-salary (self department))
  (reduce
    (lambda (sum an-employee) (add sum (an-employee:salary)))
    0
    (self:employees)))

(define (clear-total-salary (self department))
  (assign! self :total-salary nil))

...
(remove (a-department:employees) an-employee)
...

(define (add-employee (self department) (an-employee employee))
  (clear-total-salary self)
  (cons (self:employees) an-employee))

(define (remove-employee (self department) (an-employee employee))
  (clear-total-salary self)
  (remove (self:employees) an-employee))
```

> Don't just blindly name a Collection Accessor Method after the
> collection message it delegates. See if you can find a word from the
> domain that makes more sense. For example, I prefer:

```smalltalk
employs: anEmployee
  ^employees includes: anEmployee
```

```scheme
(define (employs? (self department) (an-employee employee))
  (includes? (self:employees) an-employee))
```

to:

```smalltalk
includesEmployee: anEmployee
  ^employees includes: anEmployee
```

```scheme
(define (includes-employee? (self department) (an-employee employee))
  (includes? (self:employees) an-employee))
```

## Enumeration Method

<question>
   How do you provide safe, general access to collection elements?

  <answer>
    Enumeration Method

    Implement a method that executes a Block for each element of
    the collection. Name the method by concatenating the name of
    the collection and "Do:".
  </answer>
</question>

```smalltalk
Department>>employeesDo: aBlock
  employees do: aBlock
```

```scheme
(define (foreach-employee (self department) (f (-> employee any)))
  (foreach employees f))
```

## Boolean Property Setting Method

<question>
   How do you set a boolean property?

  <answer>
    Boolean Property Setting Method

    Create two methods beginning with "be".
    One has property name, the other the negation.
    Add "toggle" if the client doesn't
    want to know about the current state.
  </answer>
</question>

```smalltalk
beVisible / beInvisible / toggleVisible
beDirty / beClean
```

```scheme
be-visible / be-invisible / toggle-visible
be-dirty / be-clean
```

## Role Suggesting Instance Variable Name

> The two important pieces of information to communicate about any
> variable are:
>
> - What is its purpose?
> - How is it used?

> Typically, when you read code you have a purpose in mind.  If you
> understand the role of a variable and it is unrelated to your
> purpose, you can quickly skim over irrelevant code that uses that
> variable.  Likewise, if you see a variable that is related to your
> purpose, you can quickly narrow your reading to relevant code by
> looking for that variable.

> How a variable is used and the messages it is sent are its "type".
> Smalltalk doesn't have declared types, but that doesn't mean they
> aren't important. Understanding the messages sent to a variable
> tells you what objects can be safely placed as values in that
> variable. Substitution of objects is the heart of disciplined
> maintenance and reuse.

这个 pattern 就是我读这本书的主要原因，
在之前我经常是用 type 来命名 instance variable，
我怀疑这并不是最好的选择，现在 Kent 的解释让我更明白了这一点。

- 我也想到了这两种区分，但是我没能像 Kent 一样，
  把我的思考总结成一个 pattern，并且给它一个好名字。
  在之后学习和工作中，我要努力学习 Kent 捕捉 pattern 的知识论。

  - 可以称 pattern 为一个知识论/认识论（Epistemology）吗？

> Different variables appear in different contexts, so what you need
> to communicate with their names is different. The context for
> instance variables is most similar to the context for temporary
> variables. The only way you have for communicating the role of an
> instance variable is through its name. If the variables in Point
> were called "t1" and "t2" instead of "x" and "y", you'd have a lot
> of reading to do before you could tell which was the horizontal
> component and which the vertical. Naming the variables by their
> roles gives you that information directly.

> On the other hand, the type of an instance variable is easily
> discovered from the code in which it resides. It is easy to find
> where the variable is used and from there discover what messages it
> is sent. You also get hints from Creation Parameter Setting Methods
> or Setting Methods that set the value of the variable.

在静态类型的语言中，情况更是如此，更没必要在变量名中加上类型信息了。
用「冠词 + 类型」的方式来命名变量只是在找不到更好的
role suggesting name 时才需要做的事。

<question>
  What do you name an instance variable?

  <answer>
    Role Suggesting Instance Variable Name

    Name instance variables for the role they play in the computation.
    Make the name plural if the variable will hold a Collection.
  </answer>
</question>

之所以有上面关于 "plural" 的建议，
在于英语有这样的变形，可以用来区分 plural。

比如：

- 如果人们用来编程的是拉丁语，
  就会有更多的变形相关的 convention 了。

- 假如人们编程用的是 Esperanto，
  使用冠词可能就更自然了，
  因为 Esperanto 没有不定冠词（不用不定冠词也能形成泛指），
  只有定冠词 la，可用于所有的性、数、格。

# TEMPORARY VARIABLES

> Temporary variables let you store and reuse the value of
> expressions.  They can be used to improve the performance or
> readability of methods.  The following patterns motivate and guide
> the use of temporary variables. The examples are taken from
> Smalltalk, but the discussion applies to any language with
> procedure-scoped variables.

看这里给 temporary variable 分类的架势，
让人想到了亚里士多德给 be 分类，
同样是人们觉得再简单不过的概念，
其分类中都藏着很多知识。

## Temporary Variable

<question>
  How do you save the value of an expression
  for later use within a method?

  <answer>
    Temporary Variable

    Create a variable whose scope and extent is a single method.
    Declare it just below the method selector.
    Assign it as soon as the expression is valid.
  </answer>
</question>

Temporary variables are good at helping you understand a computation
that is halfway towards its goal. Thus, you can more easily read:

```smalltalk
Rectangle>>bottomRight
  | right bottom |
  right := self left + self width.
  bottom := self top + self height.
  ^right @ bottom
```

```scheme
(define (bottom-right (self rectangle))
  (let ((right (add (self:left) (self:width)))
        (bottom (add (self:top) (self:height))))
    (create point :x right :y bottom)))
```

than you can:

```smalltalk
Rectangle>>bottomRight
  ^self left + self width @ (self top + self height)
```

```scheme
(define (bottom-right (self rectangle))
  (create point
    :x (right (add (self:left) (self:width)))
    :y (bottom (add (self:top) (self:height)))))
```

## Collecting Temporary Variable

<question>
  How do you gradually collect values
  to be used later in a method?

  <answer>
    Collecting Temporary Variable

    When you need to collect or merge objects over a complex enumeration,
    use a temporary variable to hold the collection or merged value.
  </answer>
</question>

## Caching Temporary Variable

<question>
  How do you improve the performance of a method?

  <answer>
    Caching Temporary Variable

    Set a temporary variable to the value of the expression
    as soon as it is valid. Use the variable instead of the expression
    in the remainder of the method.
  </answer>
</question>

```smalltalk
self children do: [:each | ...self bounds...]

| bounds |
bounds := self bounds.
self children do: [:each | ...bounds...]
```

```scheme
(foreach (self:children)
  (lambda (child)
    ... (compute-bounds self) ...))

(let ((bounds (compute-bounds self)))
 (foreach (self:children)
   (lambda (child)
     ... bounds ...)))
```

## Explaining Temporary Variable

> In the passion of the moment, you can write expressions within
> methods that are quite complex.  ...  Coming back to such a method
> in six months is quite a different experience.

这里的意思是说，即便不是为了效率而 extract a variable，
也可以为了 explaining 而 extract a variable。

<question>
  How do you simplify a complex expression within a method?

  <answer>
    Explaining Temporary Variable

    Take a subexpression out of the complex expression.
    Assign its value to a temporary variable
    before the complex expression.
    Use the variable instead in the complex expression.
  </answer>
</question>


```smalltalk
LinearHashTable>>findKeyIndex: element for: client
  | index indexedObject lastIndex |
  lastIndex := self size.
  ...
```

```scheme
(define (find-key-index-for (self linear-hash-table) ...)
  (let ((last-index (sub1 (self:size))))
    ...))
```

## Reusing Temporary Variable

JS 的 `Date.now()` 就是最典型的例子。

<question>
  How do you use an expression several places in a method
  when its value may change?

  <answer>
    Reusing Temporary Variable

    Execute the expression once and set a temporary variable.
    Use the variable instead of the expression
    in the remainder of the method.
  </answer>
</question>

## Role Suggesting Temporary Variable Name

> There are two important dimensions to communicate about a variable.
> The first is its type. Readers wishing to modify code need to know
> what responsibilities are assumed for an object occupying a
> variable.  The second important dimension is role, that is, how the
> object is used in the computation.  Understanding the role is
> important to understanding the method in which the variable is used.
> Different kinds of variables require different naming treatments to
> communicate type and role.

重复了前面关于 Role Suggesting Instance Variable Name 的讨论。

<question>
  What do you call a temporary variable?

  <answer>
    Role Suggesting Temporary Variable Name

    Name a temporary variable for the role it plays in the computation.

    Use variable naming as an opportunity to communicate
    valuable tactical information to future readers.
  </answer>
</question>

---
title: 6. Classes
---

> There is probably no coding decision with more effect on the quality
> of your code than names you give your classes. If computers were the
> only audience for code, we could just call them C1, C2, C3, and have
> done. The expensive reader is not the computer, it is other people.

> Naming classes is your biggest billboard for communicating about
> your system. The first thing readers will look at when they look at
> your code is the names of the classes. Those names will go beyond
> your code. Insidiously, they leak into everyday conversation -- and
> not just for developers. Ten years down the road, you will hear
> users who know nothing about programming using the class names you
> chose.

这是个令人震惊的事实，
虽然人们有很强大的、表达能力很强的程序语言来编程，
但是当想要表达程序中最重要的东西的时候，
还是要依赖人类语言的词法与词源。

> Good class names provide insight into the purpose and design of a
> system. They reveal underlying metaphors. They communicate themes
> and variations. They break the system into parts and show how the
> parts get put back together.

> Great naming is an art and will always remain so. Good naming is in
> the reach of everyone. Avoiding obvious mistakes and producing names
> that work together will boost your class names above average.

# Simple Superclass Name


> All naming decisions have several constraints in common. You want
> names that are as short as possible, so they are easy to type,
> format, and say.  At the same time, you want to convey as much
> information as possible in each name, so readers will not have to
> carry as much knowledge in their heads. You want names that are
> familiar, to take advantage of knowledge readers already have via
> metaphor or analogy. However, you want names that are unique, so
> that others who are also choosing names will not accidentally choose
> names that interfere with yours.

这是对命名这个难题的很不错的总结。

> The first rule I follow is no abbreviations. Abbreviations optimize
> typing (a 10-100 times in 20 years task) over reading (a 1000-10000
> times in 20 years task). Abbreviations make the interpretation of a
> name a two step process -- what do the letters stand for and then
> what do those words mean.  The class and method naming patterns here
> will produce names that you should never have to abbreviate.

在读 propagator model 的论文时，
我拒绝 TMS（Truth Maintenance Systems）这个缩写，
而使用 BeliefSystem 的依据找到了！

> Unfortunately, many people get all formal when they go to name a
> superclass. Just calling it what it is isn’t enough. They have to
> tack on a flowery, computer science-y, impressive sounding, but
> ultimately meaningless word, like Object, Thing, Component, Part,
> Manager, Entity, or Item.

哈哈哈。

> You’re creating a vocabulary, not writing a program. Be a poet for
> a moment. The simple, the punchy, the easily remembered will be far
> more effective in the long run than some long name that says it all,
> but in such a way that no one wants to say it at all.

既然命名是写程序的程中最重要的问题，
这么说来想要成为一个优秀的程序员，
首先要成为一个诗人。

- 正如想要成为一个数学家，
  首先要成为一个想象力丰富的幻想家。

<question>
  What do you call a class that is expected
  to be the root of an inheritance hierarchy?

  <answer>
    Simple Superclass Name

    Name a superclass with a single word
    that conveys its purpose in the design.
  </answer>
</question>

# Qualified Subclass Name

<question>
  TODO

  <answer>
    Qualified Subclass Name

    TODO
  </answer>
</question>

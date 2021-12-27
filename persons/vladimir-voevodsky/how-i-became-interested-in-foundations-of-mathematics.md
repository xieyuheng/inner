---
title: How I became interested in foundations of mathematics
author: Vladimir Voevodsky
year: 2015
venue: The 9th Asian Science Camp, Thailand
video: https://www.youtube.com/watch?v=55yZE3IoAy8
---

When I was studying in Moscow University,
foundations of mathematics is a subject is extremely unfashionable.

There were different groups,
a group of algebra, a group of topology,
a group of analysis, a group of differential equations ...,
there was also a group of foundations of mathematics.

At the end of second year every student must choose his or her group.
It was an important choice, and we discuss it a lot,
and different group has different reputation.

At my time the group of algebra is very cool, there were famous people in the group.
the group of topology and geometry was also cool, there were also famous people in the group.

Some other groups are less cool,
and the group of foundations of mathematics was almost the least group of all.
(Maybe except for the history of mathematics.)

Nobody who think themself a good mathematician will considered going there.

Back then I went to the group of algebra,
I still consider myself an algebraist,
I am applying methods of algebra to other area of mathematics.

But now I working full time on foundations of mathematics.
Why?
How did I get here?
Am I becoming uncool?

I am here to explain why the way we are developing the foundations of mathematics
will make it a very cool subject again.
(I said "again", because it was a very cool subject during 1910s and 1920s.)

------

Many of us do mathematics that is a little like the Rubik Cube.

There is a problem.
And there is the search for a solution.
And when the solution is found, it is certain that it is a solution.

But the mathematics which earned me the Fields Medal of 2002 in Beijing is very different.

There is a problem.
And there is the search for a solution.
But when the solution is found, it is not certain at all that it is a solution.

(The Fields Medal was awarded to me for the proof of Milnor's Conjecture.)

------

Suppose you want to solve a special algebraic equation of degree 5.
You found the solution by some means,
then you can substitution the root into the equation to check that it is a solution.

But if you want to solve a group of 20 equations of degree 20
or something as complicated like that.
You found the solution by some means,
then you have to do a lot of calculation to check your solution.

Nowaday no one will do it by hand,
you will use some software to help you verify your solution is a solution.

Now imagine your problem is to prove some theorem.
And your solution is a proof.
How do you verify the proof is really a solution?

There are some conventions you learned from your teachers or professors,
about what constitutes a rigorous proof,
but you can not feed it to a computer to let the computer check it yet.

------

Back to the Milnor's Conjecture.

The problem is to find a proof of the conjecture.

The search for a solution took me about two years, from 1993 to 1995.

The solution was a proof.

In 1995 I started to work on "writing the proof down".
I had the first preprint available in June of 1995.

But it was only the beginning of the story of my proof of Milnor's Conjecture.

- You can see how the dynamic of the work of real mathematics unfold in time.
  Ideally a mathematician prove theorems, but you will see how it really goes.

  Remember that I got the proof at 1995, but got my Fields Medal in 2002.

------

The proof that I found depend on another conjecture.
That conjecture was in itself very cool and connected two areas of mathematics
which were at that time very far apart.

I was also sure that I know how to prove this conjecture,
but I know it will take long time.

Then I started to look for a modification of the first proof,
to make it not depend on the new conjecture.
About a year later I found it.
I wrote preprint with the new proof in December 1996.
The proof in the preprint contained all the main ideas
but many details were left out.

And then it took me 7 years to work out these details
and to publish a paper with a complete proof ...

------

And I was lucky!

The ideas which the proof was based on turned out to be solid
and the results of other people which I relied on turned out to be correct.

This is not always the case.

Let me tell you the story of another of my proofs which turned out very differently.

------

In 1987, I was introduced to Mikhail Kapranov.
I was an undergrad at Moscow University and he was a graduate student.

We immediately discovered that we are both dreaming of
developing new "higher dimensional" mathematics
inspired by the concepts of higher category theory.

We started to work together.
It was great fun.
Doing mathematics with someone from whom you can learn,
while discovering together things which are new both for you and for the world
is an amazing and powerful experience.

------

In the summer of 1990, by recommendation of Kapranov,
I was accepted to the graduate school of Harvard without having to apply.
In the Fall of 1990 I left the Soviet Union and the american period of my life began ...

------

Kapranov and I wrote a paper about the key conjecture of the new "higher dimensional" mathematics
that was due to Alexander Grothendieck,

> the intuition appeared that infinity-groupoids should constitute particularly adequate models for homotopy types,
> the n-groupoids corresponding to truncated homotopy types (with `pi(i) = 0` for `i > n`).
> -- Esquisse d'un Programme 1984

------

The conjecture was not a precise one
since what should be the definition of an infinity-groupoid remained open.

Kapranov and I decided that we know what the definition should be
and how to prove the conjecture with this definition.

We wrote a paper with a sketch of the proof
and published it in one of the best Russian mathematical journals
and the paper with the complete proof was published
in the proceedings of a conference that we have been invited to.

------

We felt that the issue with this conjecture is closed
and that this important element of "higher dimensional" mathematics had been understood.

Then in 2003, twelve years after our proof was published in English,
a preprint appeared on the web in which Carlos Simpson,
very politely, claimed that he has constructed a counter-example to our theorem.

I was busy with the work on the motivic program
and very sure that our proof is correct
and ignored the preprint.

------

Then the motivic period of my life was completed
and I started to work on computer proof verification
and new foundations of mathematics.

The correspondence between the infinity-groupoids and homotopy types
re-emerged as the cornerstone of the Univalent Foundations.

And then in the Fall of 2013,
some sort of a block in my mind collapsed
and I suddenly understood that Carlos Simpson was correct
and that the proof which Kapranov and I published in 1991 is wrong.

Not only the proof was wrong but the main theorem of that paper was false!.

------

In this story I got lucky again.

The theorem was false with the particular definition of infinity groupoids
which Kapranov and I have used.
There were by now various other definitions
with which the statement of the theorem became correct.

The use of the Grothendieck correspondence,
as it became known, in the Univalent Foundations
was not endangered.

But belief in the correctness of our false theorem
played an important and negative role
in how I perceived, for all these years, the subject area of multidimensional category theory.

- Because I believed in something which was false,
  I could not believe other things which were true,
  because they contradict each other.

  So I could not understand the work of others in this area.

------

When I recognized that the theorem of the paper is false
I contacted Kapranov to tell him that
we need to do something about the paper
and to tell Carlos Simpson that his preprint from 2003 is correct.

An interesting feature of this story is that
Carlos Simpson did not point out where in the proof,
which was about 10 pages long, the mistake was.
He only showed that it can not be correct
by building a counter-example to the final statement.

It took me several weeks to find which particular lemma in the paper is incorrect
and to find counterexamples to that lemma.

There no ending to this story yet.
The question that we originally wanted to answer
-- how to find an algebraic definition of infinity groupoids
that would satisfy the Grothendieck correspondence,
remains open...

------
Now let us look at this story again.
Kapranov and I have found a solution to the problem which we worked on
-- the proof of the theorem.

If the problem was to solve an equation
and we would have found a solution
we would have checked that it is a solution before publishing it, right?

And if it were a complex equation
we would probably have checked it on a computer.

So why can not we check a solution which is a proof of a theorem?

------

I started to ask myself this question more than 10 years ago
when the solutions, proofs, which I was inventing
were becoming more and more complex
and I was getting more and more worried that they may contain mistakes.

And trying to answer this question
led me to my current interest in Foundations of Mathematics.

Let me explain how.

------

A solution to an equation would probably be a number or a collection of numbers.

Verification in this case would consist in performing some computations with these numbers
and comparing the result of these computations with some other numbers.

But what should we do when the solution is a proof of a statement?

------

A hint can be seen from looking at the case
when the problem was to solve an equation in symbolic form.
For example, to find a formula for solving
a general equation of the form `x^3+a*x+b=0`.

How would we check the solution in this case?
We would probably use some software for symbolic computation
which can compute not only with numbers
but also with expressions which have variables in them.

So in order to check a solution which is a proof of a statement
we need to write both the statement and the proof as some kind of symbolic expressions
let's say `T` for the statement, and `A` for the proof,
and then use some software which can compute with these
expressions in such a way as to check that `A` is indeed a proof of `T`.

------

Encoding of statements and proofs
which exist in our thoughts
into symbolic expressions
is called **formalization**.

Formalization is, just like programming, first of all a tool
that we can use to pass on to computers
some of the mental tasks which we need to perform.

But at the moment it is much less developed than programming
and when I started to search, in 2003, for a formalization system
that I could use to help me check my proofs I could not find any.

I decided that I need to create such a system.

The first question to answer was
what was it that prevented the creation of such a system earlier?

------

What is involved in the creation of a formalization systems
for use in mathematics?

First of all we need to have a some knowledge
about how to design formal deduction systems
which are for formalization what programming languages are for programming.

The theory of formal deduction systems originated, as far as I could find, with
an amazing paper by Gottlob Frege from 1879 which is called
"A formula language, modeled upon that of arithmetic, for pure thought".

Today it is studied mainly in Computer Science "Theory B".

By the way, it is "Theory B"
not because it is less important than "Theory A"
but because of a Handbook of Theoretical Computer Science
which was published in two volumes "A" and "B"
and the theory of formal deduction systems was discussed in the second volume.

- The "Theory A", is much better known
  in many country of Asia, and in America, and in Israel.
  It is mostly concerned with complexity and algorithms.

  The "Theory B" is concerned with the theory and design of programming languages.

  The "B" here actually means it is more difficult,
  because more simple things are discussed in the first volume,
  and more complex things are discussed in the second volume.

------

But the theory of formal deduction systems
is only one part of what we need
to formalize mathematical statements and proofs.

This theory studies all possible formal deduction and computation systems.
Whether a given system formally represents some actual system of reasoning
which is used in the world of thought
is of no concern to this theory.

- It is like the theory of general differential equations,
  so it studies all differential equations,
  and does not care whether some differential equations
  describe some actual process in real world or not.

For proof verification we need to construct a **particular** formal deduction system
and explain how it corresponds with the mathematical objects
and forms of reasoning which exist in our thoughts.

------

Constructing such systems and correspondences between their formal components
and objects and actions in the world of our mathematical thoughts
is the main task of the field which is called Foundations of Mathematics.

- This is what Foundations of Mathematics is about,
  it is about connecting the world of our thoughts to objects in formal systems,
  which will help us to manipulate our thoughts.

A formal deduction system together with a correspondence
between its components and objects and actions in the world of mathematical thoughts
which can be used to formalize all subject areas of mathematics
is called a foundational system for mathematics or "Foundations of Mathematics".

- Of course this is not how "Foundations of Mathematics" is formulated
  back when I was in Moscow University.
  It was formulated very differently,
  if it was formulated like this,
  I will think it would be very very cool,
  but it was not.

  And this is how we look at it now.

------

I you have a system which can give you formulae
corresponding to your thoughts about numbers,
that would be a numbers system.

The earliest things like Foundations of Mathematics were the number systems,
these were the systems which allow people to write numbers,
which were objects of their simple mathematical thoughts.

They write numbers as symbols and to manipulate symbols
to compute practical things about numbers in the world.

For example, How to compute "How many bricks is needed to build a wall of some size".

That was mathematics at that time,
and the Foundations of Mathematics at that time is number systems.

Today mathematics is much wider.

And the Foundations of Mathematics,
is supposed to be some analog of number systems,
but for the mathematical objects which are much more abstract that we are dealing with today.

------

The mainstream foundation of classical pure mathematics
is called Zermelo-Fraenkel Set Theory with the Axiom of Choice or ZFC,
by the name of the axiomatic system of predicate logic which it uses.

- ZFC is a formal deduction system,
  In which the way to establish the correspondence between mathematical objects and our thoughts
  is called set theoretical mathematics.

  That is what the Bourbaki group did for many years in some areas of mathematics.

  It is a very beautiful foundation when you understand it.

It was created in the first decades of the 20th century before computers came into existence
and before the problem of formalization of actual complex proofs became relevant.

- Before there were computers, no one thought about actually formalizing complex proofs,
  because it was too difficult to do without computers.

In part because of it was designed to be used with mathematics of that time
it is not well adapted to the mathematics of the 21st century.
It is not well adapted to the task of computer proof verification.

To be able to check my proofs
as one can check solutions to equations
I needed new foundations of mathematics.

And this is how I became interested in foundations of mathematics ...

# Developments

Since then the story developed as follows.

I came up with the main ideas of Univalent Foundations in 2006.
Only one element was missing and it took me three years to find it.

In the Fall of 2009 I gave the first public lecture about the "univalent model"
-- a mathematical construction which connects Martin-Löf's Type Theory to ZFC
in a new, unexpected, way.

By the Spring of 2010 I have recognized that
I had a working version of a new formalization system
based on a new foundational system that I called Univalent Foundations.

In the academic year 2012/13, Thierry Coquand, Steve Awodey and myself
organized a special program at the Institute for Advanced Study in Princeton where I work.

------

During that year the participants of the program wrote, together, a book
called "Homotopy Type Theory".

Type these words in Google
and you will be directed to a website
where you can learn more about this new subject
and also download the book for free.


The book is a truly collective effort
and as such it does not have an author.
The person who did most to make this book happen
and who continues to shape the content and the style of the book
is Michael Schulman.

------

On June 21, 2014 Univalent Foundations passed another important milestone.
Thierry Coquand gave a talk about Univalent Foundations at the Bourbaki Seminar in Paris.
Being chosen for a presentation on this seminar
is widely considered to be an important symbol of recognition
in the world of pure mathematics.

Thierry and his colleagues are also the authors of the most important advance
in the mathematics of Univalent Foundations since their invention.
They have constructed another model,
similar to the original "univalent model" of 2009
but based on constructive mathematics.

This model opens up the way
for wider Univalent Synthesis of classical
and constructive mathematics ...

# After Notes

Such amazing stories as this one do not happen often.
But little boring stories of small mistakes happen all the time.

They were happening in my life

------

These small mistakes waste our time
and embarrass us when discovered by others.

As we get older and more established the fear of mistakes grows.
We spend more time re-checking our results and become less daring in trying new things.

------

As I said, I am lucky that I don’t have in my mathematical life
a story of a mistake which destroyed an important part of my work.

I know people who are not so lucky.

And as mathematics becomes more complex
the weight of mistakes of the fear of making a mistake
is slowing the development of mathematics more.

# Q&A

**Questioner** asked about the used of proof assistants.

**Voevodsky**: Proof assistants can be extremely useful,
just like musical instrument,
you can use it to practise,
in the circumstance where you do not have a teacher.

**Questioner** asked about how to improve "mathematical sense".

**Voevodsky**: By doing a lots and lots and lots detailed proofs and computations,
for hours and hours, and days and days, and month and month, and years.
trying to examine different areas of mathematics,
then it all some how connected in your head in some general view.

To use such "mathematical sense" to find new proofs,
When I was younger I would spending lots and lots of hours
writing things on paper, thinking and writing again,
and drinking a lot of strong tea,
walking around and rotating these things in my head over and over again.

As I become older I do not do this much, or at least I try not to.
And I usually try to just formulate a question very precisely,
to make sure it only has one meaning,
to place it in my head, to have it there firmly for sometime,
then I forget about it and do other things,
and I wait for the answer to come.

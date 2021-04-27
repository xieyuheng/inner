# Domain Driven Design

Tackling complexity in the heart of software
by Eric Evans

# Part I Putting the Domain Model to Work

## Chapter 1: Crunching Knowledge

## Chapter 2: Communication and the Use of Language

- Human language is magical.

## Chapter 3: Binding Model and Implementation

- The following seems like
  the inference rules in type theory paper
  v.s. type system implementation.

This connection can break down in many ways, but the detach-
ment is often a conscious choice. Many design methodologies advo-
cate an analysis model, quite distinct from the design and usually
developed by different people. It is called an analysis model because
it is the product of analyzing the business domain to organize its con-
cepts without any consideration of the part it will play in a software
system. An analysis model is meant as a tool for understanding only;
mixing in implementation concerns is thought to muddy the waters.
Later, a design is created that may have only a loose correspondence
to the analysis model. The analysis model is not created with design
issues in mind, and therefore it is likely to be quite impractical for
those needs.

... But the second time around, if the developers per-
ceive analysis to be a separate process, modeling happens in a less
disciplined way. If the managers perceive analysis to be a separate
process, the development team may not be given adequate access to
domain experts.

Whatever the cause, software that lacks a concept at the founda-
tion of its design is, at best, a mechanism that does useful things
without explaining its actions.

- We should not only implement, but also
  make the implementation explains its actions.

The imperative to relate the domain model closely to the design
adds one more criterion for choosing the more useful models out of
the universe of possible models. It calls for hard thinking and usually
takes multiple iterations and a lot of refactoring, but it makes the
model relevant.

Therefore:

Design a portion of the software system to reflect the domain
model in a very literal way, so that mapping is obvious. Revisit the
model and modify it to be implemented more naturally in software,
even as you seek to make it reflect deeper insight into the domain.
Demand a single model that serves both purposes well, in addition
to supporting a robust UBIQUITOUS LANGUAGE.

Draw from the model the terminology used in the design and
the basic assignment of responsibilities. The code becomes an ex-
pression of the model, so a change to the code may be a change to
the model. Its effect must ripple through the rest of the project’s ac-
tivities accordingly.

To tie the implementation slavishly to a model usually requires
software development tools and languages that support a modeling
paradigm, such as object-oriented programming.

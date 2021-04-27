# Domain Driven Design

Tackling complexity in the heart of software
by Eric Evans

# Part I Putting the Domain Model to Work

## Chapter 1: Crunching Knowledge

## Chapter 2: Communication and the Use of Language

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

---
title: Software Design for Flexibility
subtitle: How to Avoid Programming Yourself into a Corner
authors: Chris Hanson, Gerald Jay Sussman
date: 2021
---

# Preface

We also wanted to teach students about dependencies -- how they can be tracked,
and how they can be used for explanation and to control backtracking.

# 1: Flexibility in Nature and in Design

**Additive programming**

Our goal in this book is to investigate how to construct
computational systems so that they can be easily adapted to
changing requirements. One should not have to modify a working
program. One should be able to add to it to implement new
functionality or to adjust old functions for new requirements.

We call this *additive programming*.

In order for additive programming to be possible, it is necessary
to minimize the assumptions about how a program works and how
it will be used. Assumptions made during the design and
construction of a program may reduce the possible future
extensions of the program. Instead of making such assumptions, we
build our programs to make just-in-time decisions based on the
environment that the program is running in. We will explore several
techniques that support this kind of design.

# 2: Domain-Specific Languages

# 3: Variations on an Arithmetic Theme

# 4: Pattern Matching

# 5: Evaluation

# 6: Layering

# 7: Propagation

# 8: Epilogue

---
title: How to organize classes into application?
date: 2021-10-12
---

We already have a lot of well organized, dependency injected classes.
How to organize them into an application?

We can use a facade class to provide:

- instances
- simplified constructor methods
- simplified factories

Facade class can only be used in interfacing classes,
where objects need to be injected.

(in normal class, we can simply use dependency injection.)

Interfacing classes includes:

- http routes
- console commands
- ...

---
title: Routing
---

# Basic Routing

Dispatching `Request` to functions (closure or controller action),
while the builder pattern for building `Route` allows features like `Middleware`.

Defining routes is like executing top-level statements in a language implemention,
which do side effects to something like a module.

- For example, just like the routes, the order of statements might matter.

# Rate Limiting

Define `RateLimiter` in `RouteServiceProvider`,
and apply them to routes as middleware.

# Look Back

Designing fluent API such (as builder pattern) is like designing DSL,
where the domain keywords are tightly scoped to the previous object.

Using named route to generate URL, is a very good API design,
which reused the informations in the route definitions.

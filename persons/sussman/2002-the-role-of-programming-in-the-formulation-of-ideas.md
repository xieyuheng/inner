---
title: The role of programming in the formulation of ideas
author: Gerald Jay Sussman
date: 2002
---

# tips

- a major obstacle to the understanding
  and teaching of physics is the use of variables
  whose meaning depends upon and changes with context.

- The traditional use of Leibnitz’s notation and Newton’s notation
  is convenient in simple situations,
  but in more complicated situations
  it can be a serious disadvantage to clear reasoning.

# lagrange-equation

- A mechanical system is described by a Lagrangian function
  of the system state (time, coordinates, and velocities).

  - what is the type of return value of Lagrangian function?

- A motion of the system is described by a path
  that gives the coordinate for each moment of time.

- A path is allowed if and only if
  it satisfies the Lagrange equations.

``` scheme
(note lagrange-equation is [{path} {lagrangian} lagrange-value == fun/0])

(+fun lagrange-value
  : (-> (-> time-t -- position-t) % path
        (-> time-t position-t velocity-t -- number-t) % lagrangian
     -- (-> time-t -- number-t))
  {path} gamma {lagrangian} 2 partial-derivative compose derivative
  {path} gamma {lagrangian} 1 partial-derivative compose fun/sub)

@note prefix-syntax
(+fun lagrange-value
  : (-> (% (-> time-t -- position-t) path)
        (% (-> time-t position-t velocity-t -- number-t)
           lagrangian)
     -- (-> time-t -- number-t))
  (fun/sub
   (derivative
    (compose (gamma path)
             (partial-derivative 2 lagrangian)))
   (compose (gamma path)
            (partial-derivative 1 lagrangian))))

(+fun gamma
  : (-> (-> time-t -- position-t) % path
     -- (-> time-t -- time-t position-t velocity-t))
  {(let time) time, time path, time {path} derivative apply})

@note prefix-syntax
(+fun gamma
  : (-> (-> time-t -- position-t) % path
     -- (-> time-t -- time-t position-t velocity-t))
  (lambda (time)
   (* time (path time) ((derivative path) time))))

(+fun derivative
  : (-> (-> number-t -- number-t)
     -- (-> number-t -- number-t))
  0 th-partial-derivative)

(+fun partial-derivative
  : (-> (-> ..., number-t -- number-t), nat-u % index
     -- (-> ..., number-t -- number-t))
  ><><><)

(+fun fun/sub
  : (-> (-> ..., number-t -- number-t) % f
        (-> ..., number-t -- number-t) % g
     -- (-> ..., number-t -- number-t))
  ><><><)
```

# >< total time derivative

- Lagrangians are not in one to one correspondence with physical systems.

- It is well known that
  a total time derivative of a function of coordinates and time
  can be added to a Lagrangian
  without changing the Lagrange equations.

# ><

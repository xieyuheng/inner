---
title: implementation of actor model
---

- components of an actor :
  1. process -- return-stack
  2. store -- data-stack
     and local binding
  3. communication -- message-queue

- axioms [interface] :
  1. send message to address
  2. create new actors
  3. specify how to handle next message

- address binding
  - address is not assigned by system,
    and can not be controlled by programmers.
  - the local store of a actor
    can only includes addresses,
    that are provided when it was created,
    or that have been recived in messages.

- [question]
  when sending a message to an address,
  will it be received by all actors bound to the address
  or only one of them?

  - x -
    we can event broadcast a message
    maybe the message also contains meta informations
    about how they should be sent.

- [question]
  how does the actor model relates to python distributed system solutions like celery.

- [question]
  use thread or process to implement actor?
  or something more light weight?

- x -
  the purpose of implementing actor model
  is to write web app in jojo,
  in which strong concurrency is needed.

- k -
  if so,
  we need to learn how web apps are written
  in existing actor model based languages
  such as io and erlang.

- interface :
  - send
  - recv

- one mail station which maintains a process pool.
  [should be thread pool, but python has no proper thread]
  each process [scheduler] in the pool maintains many actors.
  each actor has data-stack, return-stack and message-queue.

  when a actor needs to send message to other actors
  or create new actors,
  it send message to its scheduler
  which in turn send message to the station.

  a scheduler schedules its actors,
  and catches exceptions.

- address and mailing :
  actor-id
  bind actor to address
  is an address a channel?
  a channel can be implemented a another actor.

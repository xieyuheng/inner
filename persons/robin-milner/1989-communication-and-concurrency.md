---
title: communication and concurrency
author: robin milner
year: 1989
---

# intro

## a theory of communicating systems

- built from a few primitive ideas.

- a general mathematical model,
  representing real systems by the expressions of the model,
  and manipulating them in order to analyse
  the behaviour of the systems.

- The work arose from an earlier experiment.
  Where I tried to apply semantic ideas
  [known from work on sequential programming]
  to a concurrent programming language
  and I found them insufficient.

- the 'functional' theory is no longer pertinent.

- in the presence of concurrency or interference
  the memory is no longer under the control of a single program,
  but instead it interacts with programs.
  from being a slave, the memory has become an independent agent;
  he who serves two masters, serves none.

## indivisible interaction

- Around 1977 I learned that
  Tony Hoare had chosen the same primitive notion
  [the idea of indivisible interaction]
  for a strikingly different reason.

  from this single programming primitive
  one can derive many others,
  such as semaphores and monitors,
  which had previously been taken as
  primitive constructions themselves.

- Hoare's Communicating Sequential Processes [CSP]
  published in the Communications of the ACM in 1978
  describes aprogramming language [CSP]
  beautifully designed around the primitive
  of indivisible interaction.

## observation equivalence of processes

- the notion of observation equivalence of processes,
  whose behaviour is described by transition rules,
  has pleasant algebraic characteristics;

  moreover, it expresses the equivalence of processes
  whose external communications follow the same pattern
  but whose internal behaviour may differ widely.

  This abstraction from internal differences
  is essential for any tractable theory of processes.

- The theory of observation equivalence was recorded
  in 'A Calculus of Communicating Systems' [CCS]
  published by Springer-Verlag in 1980.

- David Park addressed a serious shortcoming
  in my formulation of observation equivalence.

  and I had missed a notion and a proof technique
  which, thanks to Park's discovery,
  now form a cornerstone of the theory.

## bisimulation

- The notion is bisimulation.
  a kind of invariant holding between a pair ofdynamic systems,
  and the technique is to prove two systems equivalent
  by establishing such an invariant,

  much as one can prove correctness
  of a single sequential program
  by finding an invariant property.

## Carl Petri's Net theory

- Carl Petri's Net theory
  was the first general theory of concurrency,
  and dates from the early 1960s.
  Net theory is a generalisation of the theory of automata,
  to allow for the occurrence of several actions
  (state-transitions) independently.

- the structural properties of systems
  composed from many interacting parts,
  is not treated well in Net theory.

- Net theorists pay particularly close attention
  to causality among actions.
  the present theory is by contrast observational [extensional].
  and does not deal with causality because it is not observable,
  at least in the way we conceive observation in this book.

## the term 'process calculus'

- the term 'process calculus' is coined
  because I want to be more abstract than
  a concrete programming language.

- The Dutch researchers
  J.A.Bergstra and J.W.Klop
  use the term 'process algebra'
  for a closely related approach.

- the more generous term 'calculus' is used
  to include the use of logic
  and other mathematical disciplines.
  [for example, temporal or tense logic]

# 1 modelling communication

## intro

- complex dynamic system
  communication
  concurrency

- x -
  to understand and describe a complex dynamic system
  we view the system as been composed by parts
  and say events happen among parts

- each action of an agent is either an interaction
  with its neighbouring agents, and then it is a communication,
  or it occurs independently of them and then it may occur concurrently with their actions.

  but often these independent actions of an agent are
  themselves nothing but communications among the components of that agent.

  - x -
    to use the communicating net itself,
    to encode information and program.

- it is even plausible to imagine that
  all independent actions are internal communications.

- an essential part of a theory of complex systems
  is a precise and tractable notion of behaviour.

## 1.1 communication media

### intro

- sender -> medium -> receiver

- ether as medium
  an ether is just that which contains an unordered set of messages
  and enables them to move from source to destination.

### type of medium

#### ETHER

- The Sender may always send a message.
- The Receiver may always receive a message,
  provided the mediumis not empty.
- The order of receiving messages
  may differ from the order of sending messages.

#### BOUNDED ETHER

- The Sender may always send a message, provided the medium isnot full.
- (as for ETHER)
  The Receiver may always receive a message,
  provided the medium is not empty.
- (as for ETHER)
  The order of receiving messages
  may differ from the order of sending messages.

#### BUFFER

- (as for ETHER)
  The Sender may always send a message.
- (as for ETHER)
  The Receiver may always receive a message,
  provided the medium is not empty.
- The order of receiving messages
  is equal to the order of sending messages.

#### BOUNDED BUFFER

- (as for BOUNDED ETHER)
  The Sender may always send a message,
  provided the medium is not full.
- (as for ETHER)
  The Receiver may always receive a message,
  provided the medium is not empty.
- (as for BUFFER)
  The order of receiving messages
  is equal to the order of sending messages.

#### SHARED MEMORY

- The Sender may always write an item to a register.
- The Receiver may always read an item from a register.
- Writing and reading may occur in any order.

- here an item which is sent (written) once
  may be received (read) many times,
  so messages have no identity.

- a buffer, for example,
  is often implemented in terms of a shared memory.

### to eliminate the concept of medium

- medium only occurs when we view them as so

- sender ----> receiver
  can be break into :
  sender --> medium --> receiver
  can be break again into :
  sender -> m -> medium -> m -> receiver

  thus, let's not break the first one at all.

- by refusing to admit channels as entities
  distinct from agents
  we hope to keep the primitive notions of our theory
  as few as possible.

  - x -
    but how about
    "meaningful distinctions deserve to be maintained."

- let's view communication between agents as indivisible,
  and experienced simultaneously by both participants.

  let's only say
  sender -> receiver
  and not to break it again

## 1.2 simple examples

### intro

- agent expressions

### prefix

- C := input(x).output^(x).C

- x -
  language language language

  - C := input(x).output^(x).C
    ```jojo
    C := input :x! :x !output C
    ```

  - A := input(x).input(y).output^(x).output^(y).A
    ```jojo
    A := input :x! input :y! :x !output :y !output A
    ```

- We may loosely think of agent expressions like C and C'(x)
  as standing for the different possible states of an agent;
  in general there will be many states which an agent may traverse.

  both 'agent' and 'state' will always
  be understood to mean an agent in some state.

### summation

- (+)

### composition

- (|)

### restriction

- (\)

### relabelling

- (/)

## 1.3 a larger example: the jobshop
## 1.4 equality of agents

# 2 Basic Definitions

## 2.1 Synchronisation

- if no value passes in either direction,
  There is no directionality in such communications,
  it is therefore appropriate to call them synchronisations.

- in our calculus that synchronisation and Summation,
  working together,
  give the power to express
  the communication of values of anykind!

- It means that
  for the purposes of precise definition
  and theoretical development
  we may restrict ourselves to a basic calculus
  of pure synchronisations,
  in which value variables and expressions are entirely absent.

## 2.2 Action and transition

## 2.3 The pre-emptive power of internal action

## 2.4 The basic language

## 2.5 Transitional semantics

## 2.6 Derivatives and derivation trees

## 2.7 Sorts

## 2.8 The value-passing calculus

## 2.9 Recursion expressions

## 2.10 Transition induction

# 3 Equational Laws and Their Application

## 3.1 Classification of combinators and laws

## 3.2 The dynamic laws

## 3.3 The expansion law

## 3.4 The static laws

# 4 Strong Bisimulation and Strong Equivalence

## 4.1 Experimenting upon agents

## 4.2 Strong bisimulation

## 4.3 Some properties of strong equivalence

## 4.4 Strong congruence

## 4.5 Unique solution of equations

## 4.6 Strong bisimulation as a fixed-point

# 5 Bisimulation and Observation Equivalence

## 5.1 The definition of bisimulation

## 5.2 Basic properties of bisimilarity

## 5.3 Further properties of bisimilarity

## 5.4 Specifying a simple scheduler

## 5.5 Implementing the scheduler

## 5.6 Proving the jobshop correct

## 5.7 A further technique for bisimulation

# 6 Further Examples

## 6.1 Systems with evolving structure

## 6.2 Systems with inductive structure

## 6.3 The alternating-bit protocol

## 6.4 Proof of the protocol

# 7 The Theory of Observation Congruence

## 7.1 Experiments and substitutivity

## 7.2 Equality and its basic properties

## 7.3 Unique solutions of equations

## 7.4 Axioms for finite agents

## 7.5 Axioms for finite-state agents

# 8 Defining a Programming Language

## 8.1 Introduction

## 8.2 Some derived operators

## 8.3 The language M0 and its translation

## 8.4 Adding concurrent procedures to M0

# 9 Operators and Calculi

## 9.1 Definition by equations

## 9.2 Definition by inference

## 9.3 A synchronous calculus

## 9.4 Equivalence relations

# 10 Specifications and Logic

## 10.1 Examples of specifications

## 10.2 Logic for specifications

## 10.3 Logic for imperative programs

## 10.4 Stratification of bisimilarity

## 10.5 Stratification of process logic

# 11 Determinacy and Confluence

## 11.1 Determinacy

## 11.2 Preserving determinacy

## 11.3 Confluence

## 11.4 Preserving confluence

# 12 Sources and Related Work

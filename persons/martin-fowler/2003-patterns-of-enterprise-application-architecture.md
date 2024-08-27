---
title: Patterns of Enterprise Application Architecture
author: Martin Fowler
year: 2003
---

# Preface

## What is Enterprise application?

Enterprise applications are about
the display, manipulation, and storage
of large amounts of often complex data
and the support or automation
of business processes with that data.

## What is iterative development?

At the heart of iterative development
is the notion that you should deliver software
as soon as you have something useful to the user,
even if it's not complete.

# Introduction

## What are examples of Enterprise applications?

- payroll
- patient records
- shipping tracking
- cost analysis
- credit scoring
- insurance
- supply chain
- accounting
- customer service
- foreign exchange trading

## What are NOT examples of Enterprise applications?

- automobile fuel injection
- word processors
- elevator controllers
- chemical plant controllers
- telephone switches
- operating systems
- compilers
- games

## About Enterprise applications

Enterprise applications usually involve {persistent} data.

Usually many people {access} data {concurrently}.

Usually they need to {integrate} with other enterprise applications scattered around the enterprise.

If meanings of data are not well managed, as it changes,
we will run into problems of {conceptual dissonance} with the data.

{Differences in business process} will bring conceptual dissonance to the data.

One of the best things you can do to a large system is turn it into a small one
by {simplifying its architecture and process}.

# Chapter 1: Layering

# Chapter 2: Organizing Domain Logic

# Chapter 3: Mapping to Relational Databases

## Row Data Gateway

- to be used with [Transaction Scripts]
- one instance per row
- without domain logic

## Table Data Gateway

- to be used with [Transaction Scripts]
- one instance per table
- without domain logic
- return logic-less record

## Active Record

- [Row Data Gateway] with domain logic -- to dry or replace [Transaction Scripts]
- The one-to-one match of domain classes to tables starts to fail as you factor domain logic into smaller classes.

## Data Mapper

- like [Table Data Gateway] but with only simple methods -- such as `create`, `load` and `save`
- can not return logic-less record, because it need to maintain [Identity Map]

## Unit of Work

- TODO

## Identity Map

As you load objects, you have to be wary about loading the same one twice.
If you do that, you’ll have two in-memory objects that correspond to a single
database row. Update them both, and everything gets very confusing.

To deal with this you need to keep a record of every row you read in an Identity Map (195).
Each time you read in some data, you check the Identity Map (195) first
to make sure that you don’t already have it.
If the data is already loaded, you can return a second reference to it.

As a benefit you may also be able to avoid a database call since the
Identity Map (195) also doubles as a cache for the database.

Don’t forget, however, that the primary purpose of an Identity Map (195)
is to maintain correct identities, not to boost performance.

## Lazy Load

- to bring back just enough from the database with each call.

# Chapter 10: Data Source Architectural Patterns

## Table Data Gateway

## Row Data Gateway

## Active Record

## Data Mapper

# Chapter 9: Domain Logic Patterns

## Service Layer

Defines an application’s boundary with a layer of services that
establishes a set of available operations and coordinates the
application’s response in each operation.

A Service Layer defines an application’s boundary [Cockburn PloP] and its
set of available operations from the perspective of interfacing client layers.

- Service Layer is like the view-model of MVVM,
  a view-model prepares the models for its view.
  a service prepares the models for its interfacing clients (such as http and command line).
  - note that, view-model and service both have the responsibility of setting up boundary,
    but view-model also has the responsibility of been the target of double binding,
    and service has the responsibility of logging and so on.
  - service use Layer Supertype, but view-model often not.

Identifying the operations needed on a Service Layer boundary is pretty straightforward.
They’re determined by the needs of Service Layer clients,
the most significant (and first) of which is typically a user interface.
Since a user interface is designed to support the use cases that actors want to perform with an application,
the starting point for identifying Service Layer operations
is the use case model and the user interface design for the application.

# Chapter 18: Base Patterns

## Gateway

An object that encapsulates access to an external system or resource.

Gateway 应该保持简单，负责的逻辑应该由 Gateway 的 clinet 实现

Gateway 又可以再次分层，分为 frontend 和 backend。

因为这里其实有两个职责：
- backend -- wrapping of the external service
  - 不去简化 resource 的 API
- frontend -- adaptation to your needs，
  - 可以简化 resource 的 API，来方便我们的应用代码使用

Gateway 与 Mapper 的区别在于，
Mapper 要依赖被的两端，
Mapper 更复杂，因为它的依赖更多（它的责任也更多），
而 Gateway 只依赖外部一端。

Gateway 与 Facade 的区别在于，
Facade 通常是 service 的作者简化自己 API 用的，
而 Gateway 是 service 的 client 为自己的应用场景而做的。

Gateway 与 Adapter 的区别在于，
Adapter 是为了适应某个已经存在的 inference，
而使用 Gateway 的时候，
可能并不需要用 Gateway inference，
而只实现一个 Gateway class。

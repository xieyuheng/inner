---
title: GraphQL
---

# links

- repo: https://github.com/facebook/graphql
- tut: https://www.howtographql.com
- cheat-sheet: https://raw.githubusercontent.com/sogko/graphql-shorthand-notation-cheat-sheet/master/graphql-shorthand-notation-cheat-sheet.png

# notes

## constraints on type constructors

graphQL does not have list of list
we can not write [ [ Object ] ]
only [ Object ] can occur
thus query syntax of Object is overloaded to [ Object ]

it is funny how constraints
on the usage of type constructors can be used

## why graphQL is better than REST

if only one field of information is required,
then only one field of information will be downloaded.

if many fields of one json is required,
two downloadings will be combined,
and only one request will be issued.

## schema

a schema is a file of types
with root interfaces [imp-ed as types] like
Query, Mutation, Subscription

## schema definition language

use simple type system as contract
between client and server.

- **Xie**:
  can we add dependent type to it?
  can we add full-featured contract system to it?

the concept of relation
is modeled by links in both objects.

``` graphql
type Person {
  name: String!
  age: Int!
  posts: [Post!]!
}

type Post {
  title: String!
  author: Person!
}
```

## server-side caching

- x -
  can we use Exo here?

## object fields

- object fields are conceptually functions which yield values.

# sexp

## (enum)

``` cicada
(+enum Episode NEWHOPE EMPIRE JEDI)

(+interface Character
  id : String!
  name : String
  friends : [Character]
  appearsIn : [Episode])

(+type Human implements Character
  id : String!
  name : String
  friends : [Character]
  appearsIn : [Episode]
  homePlanet : String)

(+type Droid implements Character
  id : String!
  name : String
  friends : [Character]
  appearsIn : [Episode]
  primaryFunction: String)

(+query-fun
  (hero episode: Episode) : Character
  (human id: String!) : Human
  (droid id: String!) : Droid)

(+query FetchLukeQuery
  {(human id: 1000)
   {name}})

(+query HeroNameQuery
  {hero {name}})

{hero: {name: R2-D2}}

(+query HeroNameAndFriendsQuery
  {hero
   {id
    name
    friends
    {id
     name}}})

{hero:
 {id: 2001
  name: "R2-D2"
  friends: [{id: 1000
             name: "Luke Skywalker"}
            {id: 1002
             name: "Han Solo"}
            {id: 1003
             name: "Leia Organa"}]}}

(+query NestedQuery
  {hero {name
         friends {name
                  appearsIn
                  friends {name}}}})
```

## (interfaces)

## (type)

# graphql specification

## link

- http://facebook.github.io/graphql

## 1 overview
## 2 language
## 3 type system
## 4 introspection
## 5 validation
## 6 execution
## 7 response

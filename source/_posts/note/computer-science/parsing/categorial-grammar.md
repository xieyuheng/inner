# Categorial grammar

The mathematics of sentence structure -- joachim lambek

composition

## Forth with types

postfix and composition

## Syntactic types

postfix, prefix and composition

theory about syntactic type (or say part of speech)

primitive types

s -- sentences
n -- names

poor : n/n
poor john : n
n/n n = n

works : n \ s
john works : s
n n\s = s

x/y y = x
y y\x = x

## Type list for a fragment of English

john works
n n\s

(poor john) works
(n/n n) n\s

(john works) here
(n n\s) s\s

john (never works)
n ((n\s)/(n\s) n\s)

- C. C. Fries, The Structure of English, New York, 1952.

## Formal systems

TODO

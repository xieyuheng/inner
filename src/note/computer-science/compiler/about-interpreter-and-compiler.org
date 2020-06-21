about interpreter and compiler

when you want to design a new computer programming language,
all you need to do is just imagine it in your mind.

it is when you want to implement that new programming language,
you need to do some hard work about coding
-- to write a interpreter or a compiler of that language.

some simple "facts" about these two kinds of thing are as following :

- an interpreter is relatively easier to write then a compiler.
  (except for some very bad designed languages
  (not worth to mention them by names),
  it is hard to write compiler for them,
  and it is almost not impossible to write interpreter for them.)
- when you have written some code,
  it will be easier and quick to test your code in an interpreter of the language you use,
  then to test your code in a compiler of the language you use.
- a compiler of a language may have better performance then an interpreter of that language,
  in the sense that
  it is easir to optimize the performance of the code outputed by a compiler (not the performance of the compiler itself),
  then to optimize the performance of the interpreter itself.
- when you use interpreter,
  you can NOT "sale" you program,
  for whenever you let other persons run your program,
  you have to give others your source code;
  while when you use compiler,
  you do can "sale" you program,
  by just give the person who buys your program the binary code outputed by compiler.
- ...

the following are some "facts" about the interpreter of Forth language (and its dialects) :

- it is an interpreter that almost does NOT interpret anything.
  for it does not parse expressions as many other interpreters do.
- you can compile (or more oftenly cross-compile) code in the interpreter.
- ...

in my proud opinion (IMPO),
the above common knowledges of interpreter and compiler are all NOT important,
for they are too unconcrete to be meaningful,
and they just sound like some advertisements of one's interpreter or compiler.

(sorry to waste your time, but as Chinese say (literally) "eat a loss, get a wise.")

-------------

the following are important,
and they can be viewed as an example about the different semantics between interpreter and compiler :

1. in assembly code of cicada virtual machine
   (or classical Forth virtual machine),
   1. every function should be defined only once.
   2. and the order of function definition can be arbitrary.
2. in cicada interpreter (or classical Forth interpreter),
   1. every function can be redefined,
   2. the later definition will override the former definition.
   3. so, the order of function definition is very important.
   4. and, without defining a function before,
      one can not use it in a definition of another function's body.

you see the problem?
the semantics are different!
"naturally!" one might says,
for, after all,
interpreter and compiler are using different ways to "understand" the syntax of the language.
but when semantic inconsistency arise (same for syntax),
as a language designer, he or she has to try to solve it!

1. in a classical scheme interpreter,
   for example, without defining a function which is called "help:k" first,
   one can use the following expression to define a function which is called "k" :
   (define k (lambda () (help:k)))
   and only when you use the following expression to call "k" : (k)
   if the "help:k" is not defined,
   the interpreter will inform you and rise an error.
2. scheme is using closure to achieve this.
   the "help:k", which get defined into the function body, is a "symbol".
   when one uses expression (k) to execute the funciton "k",
   the interpreter looks up a table (usually hash-table) to find out what the value of the funciton is.
3. the interpreter of Forth (or Forth dialects) will always faster then the interpreter of scheme.
   for it looks up the necessary table (hopefully hash-table)
   when the user first type the function definition into it.
   and when the function get executed,
   without the need of table-lookup anymore,
   it will just be executed as a compiled fucntion.
   (without many fancy optimizations)
   (actually well written (or well factored) Forth (or Forth dialects) functions
   can be viewed as already optimized for minimized code size (footprint)).
4. you actually can see compiler always have the advantage,
   for when a compiler is trying to "understand" the code, it has ALL the code.
   and it also feels better to do not have to worry about the order of function definitions.
   so, can I achieve the good compiler-semantic in cicada's interpreter?
5. I will achieve the following in cicada language's interpreter,
   to fix the semantic inconsistency problem a little
   (no one can fix all, for "different things are different",
    no one shall against the natural's low) :
   1. when executing function, do not has to do table-lookup.
   2. the order of function definitions are no important anymore,
      you can place a helper function after the function it helps.
   3. functions can be redefined.
   4. you can use the same code in your interpreter and your cross-compiler.

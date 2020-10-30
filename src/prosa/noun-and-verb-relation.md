# Noun and Verb Relation

2020-10-30

http://weblog.raganwald.com/2007/10/too-much-of-good-thing-not-all.html

"Not all "verbs" have a clear separation between
a single entity that is the subject or active entity that ought to own the verb's definition
and the secondary, passive subject entities that should not own the verb’s definition."


If a noun's relation to a verb is passive,
the noun should not own the verb's definition.

For example, "checker check expression",
expression is passive,
the code should not be `Exp.check(..., exp, ...)`,
the code should be `Checker.check(..., exp, ...)`.

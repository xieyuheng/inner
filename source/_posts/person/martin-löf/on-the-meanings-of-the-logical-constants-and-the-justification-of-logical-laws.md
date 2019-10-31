# On the meanings of the logical constants and the justification of logical laws

------
- Author: Martin-Löf
- Date: 1996
------

## First lecture

- **[Xie]**
  A study of the history of the use of the word "proposition"
  and the word "judgement" in logic and philosophy.

  And demonstrating how the lecturer will use them.

- **[Xie]**
  The study not only can be applied to the development on formal language,
  but also has a much broader philosophical view.

- **[Xie]**
  The study and development of the author's theory
  is heavily based on etymology, i.e. the study of the origin of words.
  How would the development be different
  if we follow the etymology of Chinese instead of western language?

- **[Xie]**
  A proposition is an expression that is used as operand of logical operations.
  A judgement is a higher level expression that is used as premises or conclusion in inference rules.

  One deduction system might have many form of judgements.
  For example, in bidirectional type checking, we have "checking mode" and "infering mode".

  Modal logic is an example of pushing judgement level expression down to proposition level expression,
  For example, the judgement "A is necessary."
  is used as a proposition in "A is necessary is true."
  where "_ is necessary" denotes a logical operation.

- **[Xie]** The following definition of judgement is famous.

Now, the question, What is a judgement? is no small question,
because the notion of judgement is just about the first of all the notions of logic,
the one that has to be explained before all the others,
before even the notions of proposition and truth, for instance.
There is therefore an intimate relation between the answer to the question
what a judgement is and the very question what logic itself is.
I shall start by giving a very simple answer,
which is essentially right: after some elaboration, at least,
I hope that we shall have a sufficiently clear understanding of it.
And the definition would simply be that,
**when understood as an act of judging, a judgement is nothing but an act of knowing,
and, when understood as that which is judged, it is a piece or, more solemnly, an object of knowledge**.

The act of judging is the same as the act of knowing,
and that what is judged is the object of knowledge.

- **[Xie]**

  | context     | judgement            | definition             |
  |-------------|----------------------|------------------------|
  | extensional | act of judging       | an act of knowing      |
  | intensional | that which is judged | an object of knowledge |

- **[Xie]** Then, what is "knowing"? Which is even a deeper question.
  Maybe just the intuitions like
  "I am sure that _",
  "I am sure that possibly _",
  "I am sure that necessarily _",
  and so on.

The important thing to realize is
of course that to judge and to know,
and, correlatively, judgement and knowledge,
are essentially the same.
And, when the relation between judgement, or assertion, if you prefer,
and knowledge is understood in this way,
logic itself is naturally understood as the theory of knowledge,
that is, of demonstrative knowledge.
Thus logic studies, from an objective point of view, our pieces of knowledge
as they are organized in demonstrative science,
or, if you think about it from the act point of view,
it studies our acts of judging, or knowing, and how they are interrelated.

Is a judgement a judgement already before it is grasped, that is, becomes known,
or does it become a judgement only through our act of judging?
And, in the latter case, what should we call a judgement
before it has been judged, that is, has become known?

| before prove | after prove       |
|--------------|-------------------|
| judgement    | evident judgement |
| proposition  | true proposition  |

- **[Xie]** The author is cornered and come up with the word "enunciation",
  I would suggest the word "claim".

## Second lecture

So the condition for it to be right of me to affirm a proposition A,
that is, to say that A is true, is not that A is true, but that I know that A is true.

When you are forced into answering a yes or no question,
although you do not know the answer, and happen to give the right answer,
right as seen by someone else, or by you yourself when you go home and look it up,
then you make a blind judgement. Thus you err, although the teacher does not discover your error.
Not to speak of the fact that the teacher erred more greatly
by not giving you the option of giving the only the answer
which would have been honest, namely, that you did not know.

- **[Xie]**
  Reader can compare the above passage with Errett Bishop's Second Principle of Constructivism:
  "Do not ask whether a statement is true until you know what it means."

There is absolutely no question of a judgement being evident in itself,
independently of us and our cognitive activity.
That would be just as absurd as to speak of a judgement as being known,
not by somebody, you or me, but in itself.
To be evident is to be evident to somebody,
as inevitably as to be known is to be known by somebody.
That is what Brouwer meant by saying, in *Consciousness, Philosophy, and Mathematics*, that
**there are no nonexperienced truths**, a basic intuitionistic tenet.

The judgement "A is true." means "I know that A is true.",
the act of knowing is implicit in the judgement.

Thus the predicate "know" indicates the modality of the act,
that is, the way in which the subject relates to the object.
And "A is true" is the judgement or, in general, the object of the act,
which in this case is an object of knowledge,
but might have been an object of conjecture, doubt, wish, fear, etc.

The closest possible correspondence between the analysis that I am giving
and Frege’s notation for a judgement "|- A", is obtained by
thinking of the vertical, judgement stroke as carrying the epistemic force "I know _".

- **[Xie]** The word "force" is used, interpreter implementers
  can think of "expression acting upon context and environment".

To specify a form of judgement, one has to lay down, what you must know
in order to have the right to make a judgement of that form.

I think that I may make things a bit clearer
by showing again in a picture what is involved here.

```
       A                  : expression
       _ is a proposition : form of judgement
       A is a proposition : judgement
I know A is a proposition : evident judgement

```

Here is involved, first, an expression `A`, which should be a complete expression.
Second, we have the form "_ is a proposition", which is the form of judgement.
Composing these two, we arrive at A is a proposition, which is a judgement.
And then, third, we have the act in which I grasp this judgement,
and through which it becomes evident.
Thus it is my act of grasping which is the source of the evidence.
These two together, that is, the judgement
and my act of grasping it, become the evident judgement.

```
       A           : proposition
       _ is a true : form of judgement
       A is a true : judgement
I know A is a true : evident judgement
```

Such a judgement has the form "_ is true", but what fills the open place,
or hole, in the form is not an expression any longer, but a proposition.
And what is a proposition? A proposition is an expression
for which the previous judgement has already been grasped,
because there is no question of something being true
unless you have previously grasped it as a proposition.
But otherwise the picture remains the same here.

Now I must consider the discussion of the notion of judgement finished
and pass on to the notion of proof.

> A proof is what makes a judgement evident.

- **[Xie]**
  Maybe we want to say that evidence's type is judgement -- `evidence : judgement`,
  but membership itself is a judgement in Martin-Löf's theory.

- **[Xie]**
  Reader can compare the above definition with Errett Bishop's Third Principle of Constructivism:
  "A proof is any completely convincing argument."

A proof is, not an object, but an act.
Brouwer wanted to stress by saying that a proof is a mental construction,
because what is mental, or psychic, is precisely our acts,
and the word construction, as used by Brouwer, is but a synonym for proof.

> to prove = to get to know = to understand,
> comprehend, grasp, or see.

This means that prove is but another synonym for understand,
comprehend, grasp, or see. And, passing to the perfect tense,

> to have proved = to know = to have understood,
> comprehended, grasped, or seen.

It is now manifest, from these equations, that proof and knowledge are the same.
Thus, if proof theory is construed, not in Hilbert’s sense, as metamathematics,
but simply as the study of proofs in the original sense of the word,
then proof theory is the same as theory of knowledge,
which, in turn, is the same as logic in the original sense of the word,
as the study of reasoning, or proof, not as metamathematics.

Remember that the proof of a judgement is the very act of knowing it.
If this act is atomic, or indivisible, then the proof is said to be immediate.
Otherwise, that is, if the proof consists of
a whole sequence, or chain, of atomic actions, it is mediate.

We also speak of a judgement being immediately and mediately evident, respectively.
And an immediately evident judgement is what we call an axiom.
Thus an axiom is a judgement which is evident by itself,
not by virtue of some previously proved judgements, but by itself,
that is, a self-evident judgement, as one has always said.
And a mediately evident judgement is what we call a theorem, as opposed to an axiom.

Instead of applying the attributes immediate and mediate to proof, or knowledge,
I might have chosen to speak of intuitive and discursive proof, or knowledge, respectively.

The proof of an axiom can only be intuitive, which is to say
that an axiom has to be grasped immediately, in a single act.
Thus a discursive proof is one which
runs, from premises to conclusion, in several steps.
It is the opposite of an intuitive proof,
which brings you to the conclusion immediately, in a single step.
When one says that the immediate propositions in the old sense of the word proposition,
that is, the immediately evident judgements in my terminology,
are unprovable, what is meant is of course only that they cannot be proved discursively.
Their proofs have to rest intuitive.

This seems to be all that I have to say about the notion of proof at the moment,
so let me pass on to the next item on the agenda,
the forms of judgement and their semantical explanations.

I have to say what my forms of judgement are, and, for each one of those forms,
I have to explain (semantical explanations) what you must know
in order to have the right to make a judgement of that form.

Let us now consider the first form of judgement,

> A is a proposition,

or, as I shall continue to abbreviate it,

> A prop.

What I have just displayed to you is a linguistic form,
and I hope that you can recognize it.
What you cannot see from the form,
and which I therefore proceed to explain to you, is of course its meaning,
that is, what knowledge is expressed by, or embodied in, a judgement of this form.
The question that I am going to answer is, in ontological terms,

> What is a proposition?

This is the usual Socratic way of formulating questions of this sort.
Or I could ask, in more knowledge theoretical terminology,

> What is it to know a proposition?

or, if you prefer,

> What knowledge is expressed by a judgement
> of the form A is a proposition?

or, this may be varied endlessly,

> What does a judgement of the form A is a proposition mean?

- **[Xie]** From ontological mode of speaching to more practical mode of speaching,
  the above questions went not far enough.
  We can go further by taking a pragmatic step to ask about the usage.
  In the sense of the "Meaning is use." slogan of Wittgenstein,
  or in aline with Peirce's recipe for pragmatic thinking,

  > Consider what effects that might conceivably have practical bearings
  >   you conceive the objects of your conception to have.
  > Then, your conception of those effects is the whole of your conception of the object.

Now, one particular answer to this question, however it be formulated,
is that a proposition is something that is true or false.

An elaboration of the definition of a proposition as something that
is true or false is to say that a proposition is a truth value, the true or the false,
and hence that a declarative sentence is an expression
which denotes a truth value, or is the name of a truth value.
This was the explanation adopted by Frege in his later writings.
If a proposition is conceived in this way, that is, simply as a truth value,
then there is no difficulty in justifying the laws of the classical propositional calculus
and the laws of quantification over finite, explicitly listed, domains.
The trouble arises when you come to the laws for forming quantified propositions,
the quantifiers not being restricted to finite domains.
That is, the trouble is to make the two laws evident
when propositions are conceived as nothing but truth values.

``` js
A(x) prop
----------------------
(forall x) A(x) prop


A(x) prop
----------------------
(exists x) A(x) prop
```

To my mind, at least, they simply fail to be evident.
And I need not be ashamed of the reference to myself in this connection:
as I said in my discussion of the notion of evidence,
it is by its very nature subject related.
Others must make up their minds whether these laws are really evident to them
when they conceive of propositions simply as truth values.

It does not help to restrict the quantifiers,
that is, to consider instead the laws

``` js
(x in A)
B(x) prop
----------------------
(forall x in A) B(x) prop

(x in A)
B(x) prop
----------------------
(exists x in A) B(x) prop
```

unless we restrict the quantifiers so severely
as to take the set A here to be a finite set,
that is, to be given by a list of its elements.

Then, of course, there is no trouble with these rules.
But, as soon as A is the set of natural numbers, say, you have the full trouble already.

Since, as I said earlier, the law of the excluded middle,
indeed, all the laws of the classical propositional calculus,
are doubtlessly valid on this conception of the notion of proposition,
this means that the rejection of the law of excluded middle
is implicitly also a rejection of the conception of
a proposition as something which is true or false.

After the formal laws of intuitionistic logic were formulated,
Kolmogorov observed that the laws of the intuitionistic propositional calculus
become evident upon thinking of the propositional variables as ranging over problems, or tasks.

On the other hand, he explicitly said that  he did not want to
equate the notion of proposition with the notion of problem
and, correlatively, the notion of truth of a proposition
with the notion of solvability of a problem.
He merely proposed the interpretation of propositions as problems, or tasks,
as an alternative interpretation, validating the laws of the intuitionistic propositional calculus.



## Third lecture

TODO

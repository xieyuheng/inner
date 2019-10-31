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

## Third lecture

TODO

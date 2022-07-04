---
title: Deduction, Induction, and Hypothesis
author: Charles Sanders Peirce
year: 1878
---

# I

The chief business of the logician is to classify arguments;
for all testing clearly depends on classification.

- **Xie:** In textbooks of nowadays, influenced by model theoy, we say

  > Logic studies the relation between syntax and its models.

  But Peirce's definition is much more practical
  more direct more general and more powerful.

  > Logicians study how to classify arguments,
  > or say, logicians study how people argue.

  For examples, Aristotle's logic and Polya's "Patterns of Plausible Inference".

  This kind of clear definitions must come from the principles in
  ["How to Make Our Ideas Clear?"](./2-how-to-make-our-ideas-clear.md),
  we should study it to get the same skill.

The classes of the logicians are defined by
certain typical forms called syllogisms.
For example, the syllogism called _Barbara_ is as follows:

    S is M; M is P:
    Hence, S is P.

Or, to put words for letters—

    Enoch and Elijah were men; all men die:
    Hence, Enoch and Elijah must have died.

The “is P” of the logicians stands for any verb, active or neuter. It is
capable of strict proof (with which, however, I will not trouble the
reader) that all arguments whatever can be put into this form; but only
under the condition that the _is_ shall mean “_is_ for the purposes of
the argument” or “is represented by.” Thus, an induction will appear in
this form something like this:

    These beans are two-thirds white;
    But, the beans in this bag are (represented by) these beans;
    ∴ The beans in the bag are two-thirds white.

But, because all inference may be reduced in some way to _Barbara_, it
does not follow that this is the most appropriate form in which to
represent every kind of inference. On the contrary, to show the
distinctive characters of different sorts of inference, they must
clearly be exhibited in different forms peculiar to each. _Barbara_
particularly typifies deductive reasoning; and so long as the _is_ is
taken literally, no inductive reasoning can be put into this form.
_Barbara_ is, in fact, nothing but the application of a rule. The
so-called major premise lays down this rule; as, for example, _All men
are mortal._ The other or minor premise states a case under the rule;
as, _Enoch was a man._ The conclusion applies the rule to the case and
states the result: _Enoch is mortal._

All deduction is of this character; it is merely
the application of general rules to particular cases.

- **Xie:** Think about [proofs-as-programs and propositions-as-types](https://en.wikipedia.org/wiki/Curry-Howard_correspondence),

  > the application of general rules to particular cases.

  means application of function to argument,
  or composition of functions;
  and the phrase "particular cases"
  can even capture subtype relation.

Sometimes this is not very evident, as in the following:

    All quadrangles are figures,
    But no triangle is a quadrangle;
    Therefore, some figures are not triangles.

But here the reasoning is really this:

    Rule. - Every quadrangle is other than a triangle.
    Case. - Some figures are quadrangles.
    Result. - Some figures are not triangles.

Inductive or synthetic [v.s. analytic] reasoning,
being something more than the mere application
of a general rule to a particular case,
can never be reduced to this form.

If, from a bag of beans of which we know that 2/3 are white, we take one
at random, it is a deductive inference that this bean is probably white,
the probability being 2/3. We have, in effect, the following syllogism:

    Rule. - The beans in this bag are 2/3 white.

    Case. - This bean has been drawn in such a way that
    in the long run the relative number of white beans so drawn
    would be equal to the relative number in the bag.

    Result. - This bean has been drawn in such a way that
    in the long run it would turn out white 2/3 of the time.

If instead of drawing one bean we draw a handful at random and conclude
that about 2/3 of the handful are probably white, the reasoning is of
the same sort. If, however, not knowing what proportion of white beans
there are in the bag, we draw a handful at random and, finding 2/3 of
the beans in the handful white, conclude that about 2/3 of those in the
bag are white, we are rowing up the current of deductive sequence, and
are concluding a rule from the observation of a result in a certain
case. This is particularly clear when all the handful turn out one
color. The induction then is:

    Case. - These beans were in the bag.
    Result. - These beans are white.
    Rule. - All the beans in the bag were white.

Which is but an inversion of the deductive syllogism.

    Rule. - All the beans in the bag were white.
    Case. - These beans were in the bag.
    Result. - These beans are white.

So that induction is the inference of the _rule_
from the _case_ and _result_.

But this is not the only way of inverting a deductive syllogism so as to
produce a synthetic inference. Suppose I enter a room and there find a
number of bags, containing different kinds of beans. On the table there
is a handful of white beans; and, after some searching, I find one of
the bags contains white beans only. I at once infer as a probability, or
as a fair guess, that this handful was taken out of that bag. This sort
of inference is called _making an hypothesis_.[52] It is the inference
of a _case_ from a _rule_ and _result_. We have, then—

**DEDUCTION.**

    Rule. - All the beans from this bag are white.
    Case. - These beans are from this bag.
    ∴ Result. - These beans are white.

**INDUCTION.**

    Case. - These beans are from this bag.
    Result. - These beans are white.
    ∴ Rule. - All the beans from this bag are white.

**HYPOTHESIS.**

    Rule. - All the beans from this bag are white.
    Result. - These beans are white.
    ∴ Case. - These beans are from this bag.

We, accordingly, classify all inference as follows:

                     Inference.
               /———————^———————-|
    Deductive or Analytic.       Synthetic.
                               /————^—————|
                          Induction.      Hypothesis.

Induction is where we generalize from a number of cases of which
something is true, and infer that the same thing is true of a whole
class. Or, where we find a certain thing to be true of a certain
proportion of cases and infer that it is true of the same proportion of
the whole class. Hypothesis is where we find some very curious
circumstance, which would be explained by the supposition that it was a
case of a certain general rule, and thereupon adopt that supposition.
Or, where we find that in certain respects two objects have a strong
resemblance, and infer that they resemble one another strongly in other
respects.

I once landed at a seaport in a Turkish province; and, as I was walking
up to the house which I was to visit, I met a man upon horseback,
surrounded by four horsemen holding a canopy over his head. As the
governor of the province was the only personage I could think of who
would be so greatly honored, I inferred that this was he. This was an
hypothesis.

Fossils are found; say, remains like those of fishes, but far in the
interior of the country. To explain the phenomenon, we suppose the sea
once washed over this land. This is another hypothesis.

Numberless documents and monuments refer to a conqueror called Napoleon
Bonaparte. Though we have not seen the man, yet we cannot explain what
we have seen, namely, all these documents and monuments, without
supposing that he really existed. Hypothesis again.

As a general rule, hypothesis is a weak kind of argument. It often
inclines our judgment so slightly toward its conclusion that we cannot
say that we believe the latter to be true; we only surmise that it may
be so. But there is no difference except one of degree between such an
inference and that by which we are led to believe that we remember the
occurrences of yesterday from our feeling as if we did so.

- **Xie:** It would be interesting if the above analogy is
  not only on the aspect of the degree of belief,
  but also on the aspect of the structure of the belief.

# II

Besides the way just pointed out of inverting a deductive syllogism to
produce an induction or hypothesis, there is another. If from the truth
of a certain premise the truth of a certain conclusion would necessarily
follow, then from the falsity of the conclusion the falsity of the
premise would follow. Thus, take the following syllogism in _Barbara_:

    Rule. - All men are mortal.
    Case. - Enoch and Elijah were men.
    ∴ Result. - Enoch and Elijah were mortal.

Now, a person who denies this result may admit the rule, and, in that
case, he must deny the case. Thus:

    Denial of Result. - Enoch and Elijah were not mortal.
    Rule. - All men are mortal.
    ∴ Denial of Case. - Enoch and Elijah were not men.

This kind of syllogism is called _Baroco_, which is the typical mood of
the second figure. On the other hand, the person who denies the result
may admit the case, and in that case he must deny the rule. Thus:

    Denial of the Result. - Enoch and Elijah were not mortal.
    Case. - Enoch and Elijah were men.
    ∴ Denial of the Rule. - Some men are not mortal.

This kind of syllogism is called _Bocardo_, which is the typical mood of
the third figure.

_Baroco_ and _Bocardo_ are, of course, deductive syllogisms; but of a
very peculiar kind. They are called by logicians indirect moods, because
they need some transformation to appear as the application of a rule to
a particular case. But if, instead of setting out as we have here done
with a necessary deduction in _Barbara_, we take a probable deduction of
similar form, the indirect moods which we shall obtain will be—

         Corresponding to _Baroco_, an hypothesis;
    and, Corresponding to _Bocardo_, an induction.

For example, let us begin with this probable deduction in _Barbara_:

    Rule. - Most of the beans in this bag are white.
    Case. - This handful of beans are from this bag.
    ∴ Result. - Probably, most of this handful of beans are white.

Now, deny the result, but accept the rule:

    Denial of Result. - Few beans of this handful are white.
    Rule. - Most beans in this bag are white.
    ∴ Denial of Case. - Probably, these beans were taken from another bag.

This is an hypothetical inference. Next, deny the result, but accept the
case:

    Denial of Result. - Few beans of this handful are white.
    Case. - These beans came from this bag.
    ∴ Denial of Rule. - Probably, few beans in the bag are white.

This is an induction.

The relation thus exhibited between synthetic and deductive reasoning is
not without its importance. When we adopt a certain hypothesis, it is
not alone because it will explain the observed facts, but also because
the contrary hypothesis would probably lead to results contrary to those
observed. So, when we make an induction, it is drawn not only because it
explains the distribution of characters in the sample, but also because
a different rule would probably have led to the sample being other than
it is.

But the advantage of this way of considering the subject might easily be
overrated. An induction is really the inference of a rule, and to
consider it as the denial of a rule is an artificial conception, only
admissible because, when statistical or proportional propositions are
considered as rules, the denial of a rule is itself a rule. So, an
hypothesis is really a subsumption of a case under a class and not the
denial of it, except for this, that to deny a subsumption under one
class is to admit a subsumption under another.

_Bocardo_ may be considered as an induction, so timid as to lose its
amplificative character entirely. Enoch and Elijah are specimens of a
certain kind of men. All that kind of men are shown by these instances
to be immortal. But instead of boldly concluding that all very pious
men, or all men favorites of the Almighty, etc., are immortal, we
refrain from specifying the description of men, and rest in the merely
explicative inference that _some_ men are immortal. So _Baroco_ might be
considered as a very timid hypothesis. Enoch and Elijah are not mortal.
Now, we might boldly suppose them to be gods or something of that sort,
but instead of that we limit ourselves to the inference that they are of
_some_ nature different from that of man.

But, after all, there is an immense difference between the relation of
_Baroco_ and _Bocardo_ to _Barbara_ and that of Induction and Hypothesis
to Deduction. _Baroco_ and _Bocardo_ are based upon the fact that if the
truth of a conclusion necessarily follows from the truth of a premise,
then the falsity of the premise follows from the falsity of the
conclusion. This is always true. It is different when the inference is
only probable. It by no means follows that, because the truth of a
certain premise would render the truth of a conclusion probable,
therefore the falsity of the conclusion renders the falsity of the
premise probable. At least, this is only true, as we have seen in a
former paper, when the word probable is used in one sense in the
antecedent and in another in the consequent.

# III

A certain anonymous writing is upon a torn piece of paper. It is
suspected that the author is a certain person. His desk, to which only
he has had access, is searched, and in it is found a piece of paper, the
torn edge of which exactly fits, in all its irregularities, that of the
paper in question. It is a fair hypothetic inference that the suspected
man was actually the author. The ground of this inference evidently is
that two torn pieces of paper are extremely unlikely to fit together by
accident. Therefore, of a great number of inferences of this sort, but a
very small proportion would be deceptive. The analogy of hypothesis with
induction is so strong that some logicians have confounded them.
Hypothesis has been called an induction of characters. A number of
characters belonging to a certain class are found in a certain object;
whence it is inferred that all the characters of that class belong to
the object in question. This certainly involves the same principle as
induction; yet in a modified form. In the first place, characters are
not susceptible of simple enumeration like objects; in the next place,
characters run in categories. When we make an hypothesis like that about
the piece of paper, we only examine a single line of characters, or
perhaps two or three, and we take no specimen at all of others. If the
hypothesis were nothing but an induction, all that we should be
justified in concluding, in the example above, would be that the two
pieces of paper which matched in such irregularities as have been
examined would be found to match in other, say slighter, irregularities.
The inference from the shape of the paper to its ownership is precisely
what distinguishes hypothesis from induction, and makes it a bolder and
more perilous step.

The same warnings that have been given against imagining that induction
rests upon the uniformity of Nature might be repeated in regard to
hypothesis. Here, as there, such a theory not only utterly fails to
account for the validity of the inference, but it also gives rise to
methods of conducting it which are absolutely vicious. There are, no
doubt, certain uniformities in Nature, the knowledge of which will
fortify an hypothesis very much. For example, we suppose that iron,
titanium, and other metals exist in the sun, because we find in the
solar spectrum many lines coincident in position with those which these
metals would produce; and this hypothesis is greatly strengthened by our
knowledge of the remarkable distinctiveness of the particular line of
characters observed. But such a fortification of hypothesis is of a
deductive kind, and hypothesis may still be probable when such
reënforcement is wanting.

There is no greater nor more frequent mistake in practical logic than to
suppose that things which resemble one another strongly in some respects
are any the more likely for that to be alike in others. That this is
absolutely false, admits of rigid demonstration; but, inasmuch as the
reasoning is somewhat severe and complicated (requiring, like all such
reasoning, the use of A, B, C, etc., to set it forth), the reader would
probably find it distasteful, and I omit it. An example, however, may
illustrate the proposition: The comparative mythologists occupy
themselves with finding points of resemblance between solar phenomena
and the careers of the heroes of all sorts of traditional stories; and
upon the basis of such resemblances they infer that these heroes are
impersonations of the sun. If there be anything more in their
reasonings, it has never been made clear to me. An ingenious logician,
to show how futile all that is, wrote a little book, in which he
pretended to prove, in the same manner, that Napoleon Bonaparte is only
an impersonation of the sun. It was really wonderful to see how many
points of resemblance he made out. The truth is, that any two things
resemble one another just as strongly as any two others, if recondite
resemblances are admitted. But, in order that the process of making an
hypothesis should lead to a probable result, the following rules must be
followed:

1. The hypothesis should be distinctly put as a question, before making
   the observations which are to test its truth. In other words, we must
   try to see what the result of predictions from the hypothesis will be.

2. The respect in regard to which the resemblances are noted must be
   taken at random. We must not take a particular kind of predictions for
   which the hypothesis is known to be good.

3. The failures as well as the successes of the predictions must be
   honestly noted. The whole proceeding must be fair and unbiased.

Some persons fancy that bias and counter-bias are favorable to the
extraction of truth—that hot and partisan debate is the way to
investigate. This is the theory of our atrocious legal procedure. But
Logic puts its heel upon this suggestion. It irrefragably demonstrates
that knowledge can only be furthered by the real desire for it, and that
the methods of obstinacy, of authority, and every mode of trying to
reach a foregone conclusion, are absolutely of no value. These things
are proved. The reader is at liberty to think so or not as long as the
proof is not set forth, or as long as he refrains from examining it.
Just so, he can preserve, if he likes, his freedom of opinion in regard
to the propositions of geometry; only, in that case, if he takes a fancy
to read Euclid, he will do well to skip whatever he finds with A, B, C,
etc., for, if he reads attentively that disagreeable matter, the freedom
of his opinion about geometry may unhappily be lost forever.

How many people there are who are incapable of putting to their own
consciences this question, “Do I want to know how the fact stands, or
not?”

The rules which have thus far been laid down for induction and
hypothesis are such as are absolutely essential. There are many other
maxims expressing particular contrivances for making synthetic
inferences strong, which are extremely valuable and should not be
neglected. Such are, for example, Mr. Mill’s four methods. Nevertheless,
in the total neglect of these, inductions and hypotheses may and
sometimes do attain the greatest force.

# IV

Classifications in all cases perfectly satisfactory hardly exist. Even
in regard to the great distinction between explicative and ampliative
inferences, examples could be found which seem to lie upon the border
between the two classes, and to partake in some respects of the
characters of either. The same thing is true of the distinction between
induction and hypothesis. In the main, it is broad and decided. By
induction, we conclude that facts, similar to observed facts, are true
in cases not examined. By hypothesis, we conclude the existence of a
fact quite different from anything observed, from which, according to
known laws, something observed would necessarily result. The former, is
reasoning from particulars to the general law; the latter, from effect
to cause. The former classifies, the latter explains. It is only in some
special cases that there can be more than a momentary doubt to which
category a given inference belongs. One exception is where we observe,
not facts similar under similar circumstances, but facts different under
different circumstances—the difference of the former having, however, a
definite relation to the difference of the latter. Such inferences,
which are really inductions, sometimes present nevertheless some
indubitable resemblances to hypotheses.

Knowing that water expands by heat, we make a number of observations of
the volume of a constant mass of water at different temperatures. The
scrutiny of a few of these suggests a form of algebraical formula which
will approximately express the relation of the volume to the
temperature. It may be, for instance, that _v_ being the relative
volume, and _t_ the temperature, a few observations examined indicate a
relation of the form—

    v = 1 + a*t + b*t^2 + c*t^3.

Upon examining observations at other temperatures taken at random, this
idea is confirmed; and we draw the inductive conclusion that all
observations within the limits of temperature from which we have drawn
our observations could equally be so satisfied. Having once ascertained
that such a formula is possible, it is a mere affair of arithmetic to
find the values of _a_, _b_, and _c_, which will make the formula
satisfy the observations best. This is what physicists call an empirical
formula, because it rests upon mere induction, and is not explained by
any hypothesis.

Such formulæ, though very useful as means of describing in general terms
the results of observations, do not take any high rank among scientific
discoveries. The induction which they embody, that expansion by heat (or
whatever other phenomenon is referred to) takes place in a perfectly
gradual manner without sudden leaps or inummerable fluctuations,
although really important, attracts no attention, because it is what we
naturally anticipate. But the defects of such expressions are very
serious. In the first place, as long as the observations are subject to
error, as all observations are, the formula cannot be expected to
satisfy the observations exactly. But the discrepancies cannot be due
solely to the errors of the observations, but must be partly owing to
the error of the formula which has been deducted from erroneous
observations. Moreover, we have no right to suppose that the real facts,
if they could be had free from error, could be expressed by such a
formula at all. They might, perhaps, be expressed by a similar formula
with an infinite number of terms; but of what use would that be to us,
since it would require an infinite number of coefficients to be written
down? When one quantity varies with another, if the corresponding values
are exactly known, it is a mere matter of mathematical ingenuity to find
some way of expressing their relation in a simple manner. If one
quantity is of one kind—say, a specific gravity—and the other of another
kind—say, a temperature—we do not desire to find an expression for their
relation which is wholly free from numerical constants, since if it were
free from them when, say, specific gravity as compared with water, and
temperature as expressed by the Centigrade thermometer, were in
question, numbers would have to be introduced when the scales of
measurement were changed. We may, however, and do desire to find
formulas expressing the relations of physical phenomena which shall
contain no more arbitrary numbers than changes in the scales of
measurement might require.

When a formula of this kind is discovered, it is no longer called an
empirical formula, but a law of Nature; and is sooner or later made the
basis of an hypothesis which is to explain it. These simple formulæ are
not usually, if ever, exactly true, but they are none the less important
for that; and the great triumph of the hypothesis comes when it explains
not only the formula, but also the deviations from the formula. In the
current language of the physicists, an hypothesis of this importance is
called a theory, while the term hypothesis is restricted to suggestions
which have little evidence in their favor. There is some justice in the
contempt which clings to the word hypothesis. To think that we can
strike out of our own minds a true preconception of how Nature acts, in
a vain fancy. As Lord Bacon well says: “The subtlety of Nature far
exceeds the subtlety of sense and intellect: so that these fine
meditations, and speculations, and reasonings of men are a sort of
insanity, only there is no one at hand to remark it.” The successful
theories are not pure guesses, but are guided by reasons.

The kinetical theory of gases is a good example of this. This theory is
intended to explain certain simple formulæ, the chief of which is called
the law of Boyle. It is, that if air or any other gas be placed in a
cylinder with a piston, and if its volume be measured under the pressure
of the atmosphere, say fifteen pounds on the square inch, and if then
another fifteen pounds per square inch be placed on the piston, the gas
will be compressed to one-half its bulk, and in similar inverse ratio
for other pressures. The hypothesis which has been adopted to account
for this law is that the molecules of a gas are small, solid particles
at great distances from each other (relatively to their dimensions), and
moving with great velocity, without sensible attractions or repulsions,
until they happen to approach one another very closely. Admit this, and
it follows that when a gas is under pressure what prevents it from
collapsing is not the incompressibility of the separate molecules, which
are under no pressure at all, since they do not touch, but the pounding
of the molecules against the piston. The more the piston falls, and the
more the gas is compressed, the nearer together the molecules will be;
the greater number there will be at any moment within a given distance
of the piston, the shorter the distance which any one will go before its
course is changed by the influence of another, the greater number of new
courses of each in a given time, and the oftener each, within a given
distance of the piston, will strike it. This explains Boyle’s law. The
law is not exact; but the hypothesis does not lead us to it exactly.
For, in the first place, if the molecules are large, they will strike
each other oftener when their mean distances are diminished, and will
consequently strike the piston oftener, and will produce more pressure
upon it. On the other hand, if the molecules have an attraction for one
another, they will remain for a sensible time within one another’s
influence, and consequently they will not strike the wall so often as
they otherwise would, and the pressure will be less increased by
compression.

When the kinetical theory of gases was first proposed by Daniel
Bernoulli, in 1738, it rested only on the law of Boyle, and was
therefore pure hypothesis. It was accordingly quite naturally and
deservedly neglected. But, at present, the theory presents quite another
aspect; for, not to speak of the considerable number of observed facts
of different kinds with which it has been brought into relation, it is
supported by the mechanical theory of heat. That bringing together
bodies which attract one another, or separating bodies which repel one
another, when sensible motion is not produced nor destroyed, is always
accompanied by the evolution of heat, is little more than an induction.
Now, it has been shown by experiment that, when a gas is allowed to
expand without doing work, a very small amount of heat disappears. This
proves that the particles of the gas attract one another slightly, and
but very slightly. It follows that, when a gas is under pressure, what
prevents it from collapsing is not any repulsion between the particles,
since there is none. Now, there are only two modes of force known to us,
force of position or attractions and repulsions, and force of motion.
Since, therefore, it is not the force of position which gives a gas its
expansive force, it must be the force of motion. In this point of view,
the kinetical theory of gases appears as a deduction from the mechanical
theory of heat. It is to be observed, however, that it supposes the same
law of mechanics (that there are only those two modes of force) which
holds in regard to bodies such as we can see and examine, to hold also
for what are very different, the molecules of bodies. Such a supposition
has but a slender support from induction. Our belief in it is greatly
strengthened by its connection with the law of Boyle, and it is,
therefore, to be considered as an hypothetical inference. Yet it must be
admitted that the kinetical theory of gases would deserve little
credence if it had not been connected with the principles of mechanics.

The great difference between induction and hypothesis is, that the
former infers the existence of phenomena such as we have observed in
cases which are similar, while hypothesis supposes something of a
different kind from what we have directly observed, and frequently
something which it would be impossible for us to observe directly.
Accordingly, when we stretch an induction quite beyond the limits of our
observation, the inference partakes of the nature of hypothesis. It
would be absurd to say that we have no inductive warrant for a
generalization extending a little beyond the limits of experience, and
there is no line to be drawn beyond which we cannot push our inference;
only it becomes weaker the further it is pushed. Yet, if an induction be
pushed very far, we cannot give it much credence unless we find that
such an extension explains some fact which we can and do observe. Here,
then, we have a kind of mixture of induction and hypothesis supporting
one another; and of this kind are most of the theories of physics.

# V

That synthetic inferences may be divided into induction and hypothesis
in the manner here proposed,[53] admits of no question. The utility and
value of the distinction are to be tested by their applications.

Induction is, plainly, a much stronger kind of inference than
hypothesis; and this is the first reason for distinguishing between
them. Hypotheses are sometimes regarded as provisional resorts, which in
the progress of science are to be replaced by inductions. But this is a
false view of the subject. Hypothetic reasoning infers very frequently a
fact not capable of direct observation. It is an hypothesis that
Napoleon Bonaparte once existed. How is that hypothesis ever to be
replaced by an induction? It may be said that from the premise that such
facts as we have observed are as they would be if Napoleon existed, we
are to infer by induction that _all_ facts that are hereafter to be
observed will be of the same character. There is no doubt that every
hypothetic inference may be distorted into the appearance of an
induction in this way. But the essence of an induction is that it infers
from one set of facts another set of similar facts, whereas hypothesis
infers from facts of one kind to facts of another. Now, the facts which
serve as grounds for our belief in the historic reality of Napoleon are
not by any means necessarily the only kind of facts which are explained
by his existence. It may be that, at the time of his career, events were
being recorded in some way not now dreamed of, that some ingenious
creature on a neighboring planet was photographing the earth, and that
these pictures on a sufficiently large scale may some time come into our
possession, or that some mirror upon a distant star will, when the light
reaches it, reflect the whole story back to earth. Never mind how
improbable these suppositions are; everything which happens is
infinitely improbable. I am not saying that _these_ things are likely to
occur, but that _some_ effect of Napoleon’s existence which now seems
impossible is certain nevertheless to be brought about. The hypothesis
asserts that such facts, when they do occur, will be of a nature to
confirm, and not to refute, the existence of the man. We have, in the
impossibility of inductively inferring hypothetical conclusions, a
second reason for distinguishing between the two kinds of inference.

A third merit of the distinction is, that it is associated with an
important psychological or rather physiological difference in the mode
of apprehending facts. Induction infers a rule. Now, the belief of a
rule is a habit. That a habit is a rule active in us, is evident. That
every belief is of the nature of a habit, in so far as it is of a
general character, has been shown in the earlier papers of this series.
Induction, therefore, is the logical formula which expresses the
physiological process of formation of a habit. Hypothesis substitutes,
for a complicated tangle of predicates attached to one subject, a single
conception. Now, there is a peculiar sensation belonging to the act of
thinking that each of these predicates inheres in the subject. In
hypothetic inference this complicated feeling so produced is replaced by
a single feeling of greater intensity, that belonging to the act of
thinking the hypothetic conclusion. Now, when our nervous system is
excited in a complicated way, there being a relation between the
elements of the excitation, the result is a single harmonious
disturbance which I call an emotion. Thus, the various sounds made by
the instruments of an orchestra strike upon the ear, and the result is a
peculiar musical emotion, quite distinct from the sounds themselves.
This emotion is essentially the same thing as an hypothetic inference,
and every hypothetic inference involves the formation of such an
emotion. We may say, therefore, that hypothesis produces the _sensuous_
element of thought, and induction the _habitual_ element. As for
deduction, which adds nothing to the premises, but only out of the
various facts represented in the premises selects one and brings the
attention down to it, this may be considered as the logical formula for
paying attention, which is the _volitional_ element of thought, and
corresponds to nervous discharge in the sphere of physiology.

Another merit of the distinction between induction and hypothesis is,
that it leads to a very natural classification of the sciences and of
the minds which prosecute them. What must separate different kinds of
scientific men more than anything else are the differences of their
_techniques_. We cannot expect men who work with books chiefly to have
much in common with men whose lives are passed in laboratories. But,
after differences of this kind, the next most important are differences
in the modes of reasoning. Of the natural sciences, we have, first, the
classificatory sciences, which are purely inductive—systematic botany
and zoölogy, mineralogy, and chemistry. Then, we have the sciences of
theory, as above explained—astronomy, pure physics, etc. Then, we have
sciences of hypothesis—geology, biology, etc.

There are many other advantages of the distinction in question which I
shall leave the reader to find out by experience. If he will only take
the custom of considering whether a given inference belongs to one or
other of the two forms of synthetic inference given on page 134, I can
promise him that he will find his advantage in it, in various ways.

# Footnotes

- Footnote [51]:

  _Popular Science Monthly_, August, 1878.

- Footnote [52]:

  [Later Pierce called it _presumptive inference_.
  See Baldwin’s _Dictionary_ art. _Probable Inference_.]

- Footnote [53]:

  This division was first made in a course of lectures by the author
  before the Lowell Institute, Boston, in 1866, and was printed in the
  _Proceedings of the American Academy of Arts and Sciences_, for April
  9, 1867.

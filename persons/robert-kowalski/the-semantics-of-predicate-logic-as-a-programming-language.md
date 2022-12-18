---
title: The Semantics of Predicate Logic as a Programming Language
authors: [Maarten van Emden, Robert Kowalski]
year: 1976
---

# Abstract

Sentences in first-order predicate logic can be usefully interpreted
as programs In this paper the operational and fixpoint semantics of
predicate logic programs are defined, and the connections with the
proof theory and model theory of logic are investigated. It is
concluded that operational semantics is a part of proof theory and
that fixpoint semantics is a special case of model-theoretic
semantics.

# 1. Introduction

Predicate logic plays an important role in many formal models of computer programs 13,
14, 17]. Here we are concerned with the interpretation of predicate logic as a program-
ming language [5, 10]. The PROLOG system (for PROgramming in LOGic), based upon
the procedural interpretation, has been used for several ambitious programming tasks
(including French language question answering [5, 18], symbolic integration [91, plan
formation [24], theorem proving, speech recognition, and picture interpretation). In this
paper we ignore the practical aspects of programming in logic and investigate instead the
semantics of predicate logic regarded as a programming language. We compare the
resulting semantics with the classical semantics studied by logicians.
  Two kinds of semantics [22], operational and fixpoint, have been defined for program-
ming languages. Operational semantics defines the input-output relation computed by a
program in terms of the individual operations evoked by the program inside a machine.
The meaning of a program is the input-output relation obtained by executing the
program on the machine. As a machine independent alternative to operational seman-
tics, fixpoint semantics [1, 6, 17, 221 defines the meaning of a program to be the input-
output relation which is the minimal fixpoint of a transformation associated with the
program. Fixpoint semantics has been used [6, 7, 15, 17] to justify existing methods for
proving properties of programs and to motivate and justify new methods of proof.
  Logicians distinguish between the syntax and the semantics of formal languages.
Syntax deals with the formal part of language in abstraction from its meaning. It deals
not only with the definition of well-formed formulas, syntax in its narrow sense, but also
with the broader study of axioms, rules of inference, and proofs, which constitutes proof
theory. Semantics, on the other hand, deals with the interpretation of language and
includes such notions as meaning, logical implication, and truth. Church's Introduction to
Mathematical Logic [4] contains a thorough discussion of the respective roles of syntax
and semantics.

  We use the interpretation of predicate logic as a programming language in order to
compare the notions of operational and fixpoint semantics of programming languages
with the notions of syntax and semantics of predicate logic. We show that operational
semantics is included in the part of syntax concerned with proof theory and that fixpoint
semantics is a special case of model-theoretic semantics. With this interpretation of
operational semantics as syntax and fixpoint semantics as semantics, the equivalence of
operational and fixpoint semantics becomes a special case of GOdel's completeness
theorem .
  This paper is concerned with the analysis and comparison of some of the most basic
notions of logic and computation As a by-product it is virtually self-contained and
requires only a general knowledge of logic but no special familiarity with the operational
and fixpoint semantics of programming languages.

# 2. A Syntax of Well-Formed Formulas

It is convenient to restrict attention to predicate logic programs written in clausal form.
Such programs have an especially simple syntax but retain all the expressive power of the
full predicate logic.
  A sentence is a finite set of clauses.
  A clause is a disjunction L1 V - • • V L. of literals L„ which are atomic formulas
P(ti, . . . , t,„,) or the negations of atomic formulas P(t„ . . . , tin), where P is a predicate
symbol and t, are terms. Atomic formulas are positive literals. Negations of atomic
formulas are negative literals.
  A term is either a variable or an expression f(ti, . . . , tin) where f is a function symbol
and t, are terms. Constants are 0-ary function symbols.
  A set of clauses {C1, . . ., Cm} is interpreted as the conjunction, C1 and . . . and Cn. A
clause C containing just the variables xi, . . . , xn, is regarded as universally quantified:
                                 for all x1, . . . , x„„ C
For every sentence S1 in predicate logic there exists a sentence S2 in clausal form which is
satisfiable if and only if Si is. For this reason, all questions concerning the validity or
satisfiability of sentences in predicate logic can be addressed to sentences in clausal form.
Methods for transforming sentences into clausal form are described in 116].
  We have defined that part of the syntax of predicate logic which is concerned with the
specification of well-formed formulas. Aspects of syntax concerned with proof theory are
dealt with in the next two sections.

# 3. The Procedural interpretation

It is easiest to interpret procedurally sets of clauses which contain at most one positive
literal per clause. Such sets of clauses are called Horn sentences. We distinguish three
kinds of Horn clauses .
(1) ❑ the empty clause, containing no literals and denoting the truth value false, is
    interpreted as a halt statement.
(2) 13, V • • • V B„ a clause consisting of no positive literals and n 1 negative literals
    is interpreted as a goal statement.
(3) A Vi31V--• V Bn a clause consisting of exactly one positive literal and n _?... 0
    negative literals is interpreted as a procedure declaration. The positive literal A is the
    procedure name and the negative literals are the procedure body . Each negative
    literal B, in the procedure body is interpreted as a procedure call. When n — 0 the
    procedure declaration has an empty body and is interpreted as an unqualified
    assertion of fact
  In the procedural interpretation a set of procedure declarations is a program. Compu-
tation is initiated by an initial goal statement, proceeds by using procedure declarations

The Semantics of Predicate Logic as a Programming Language 735

to derive new goal statements from old goal statements, and terminates with the
derivation of the halt statement Such derivation of goal statements is accomplished by
resolution [20], which is interpreted as procedure invocation. Given a selected procedure
call A, inside the body of a goal statement
               A, v -- • vA,_, vA, vA,,i v • .• VA„
and given a procedure declaration
                    A'VPIV •-•V 13„„ m a- 0
whose name matches the selected procedure call (in the sense that some most general
substitution 0 of terms for variables makes A, and A' identical), resolution derives the
new goal statement
         (A , V • - •vA,_,\//3,v--•vi3,,vAi+,v•••vA,)0.
 In general, any derivation can be regarded as a computation and any refutation (i.e.
derivation of ❑) can be regarded as a successfully terminating computation. However
only goal oriented resolution derivations correspond to the standard notion of computa-
tion. Such a goal-oriented derivation from an initial set of Horn clauses A and from an
initial goal statement C, in A is a sequence of goal statements C,, ..., , such that each
C, contains a single selected procedure call and C,, is obtained from C, by procedure
invocation relative to the selected procedure call in C, using a procedure declaration in
A
 In model elimination [13], ordered linear resolution [19], and SL-resolution [12], the
selection of procedure calls is governed by the last to/first out rule: A goal statement is
treated as a stack of procedure calls. The selected procedure call must be at the top of the
stack. The new procedure calls which by procedure invocation replace the selected
procedure call are inserted at the top of the stack. The more general notion of goal
oriented derivation defined above corresponds to computation with coroutines [10].
Computation with asynchronous parallel processes is obtained by using the splitting rule
[2, 8, 23].
 Predicate logic is a nondeterministic programming language: Given a single goal
statement, several procedure declarations can have a name which matches the selected
procedure call. Each declaration gives rise to a new goal statement. A proof procedure
which sequences the generation of derivations in the search for a refutation behaves as an
interpreter for the program incorporated in the initial set of clauses. These and other
aspects of the procedural interpretation of Horn clauses are investigated in greater detail
elsewhere [10]
 The procedural interpretation has also been investigated for non-Horn clauses [11].
However, in this paper we restrict ourselves to Horn clauses.
 Example The following two clauses constitute a program for appending two lists.
The term cons(x,y) is interpreted as a list whose first element, the head, is x and whose
tail, y, is the rest of the list. The constant nil denotes the empty list. The terms u, x, y,
and z are variables. Append(x,y,z) denotes the relationship: z is obtained by appending
y to x.
 (1) Appen(nil,x,x).
 (2) Append(cons(x,y),z ,cons(x,u)) V Append(y,z,u)
To compute the result of appending the list cons(b ,nil) to the list cons(a ,nil), the
program is activated by the goal statement
 (3) Append(cons(a ,m1),cons(b ,nil),v),
where v is a variable and a and b are constants, the "atoms" of the lists With this goal
statement the program is deterministic. With a goal directed theorem prover as inter-
preter, the following computation ensues:
                 C, = Append(cons(a ,nil),cons(b ,m1),v),
                 C2 = Append(nil,cons(b ,nil),w) 0,,
                 C3 = ❑ 02,

where 0, is the substitution v -= cons(a,w) and 02 is w := cons(b ,ni1). The result of the
computation is the value of v in the substitution 0,02, which is v := cons(a,cons(b ,m1)).

# 4. Operational Semantics

To define an operational semantics [221 for a programming language is to define an
implementation independent interpreter for it For predicate logic the proof procedure
behaves as such an interpreter
  We regard the terms containing no variables which can be constructed from the
constants and other function symbols occurring in a set of clauses A as the data structures
which the program, incorporated in A, manipulates. The set of all such terms is called
the Herbrand universe H determined by A. Every n-ary predicate symbol P occurring in
A denotes an n-ary relation over the Herbrand universe of A We call the n-tuples which
belong to such relations input-output tuples and the relations themselves input-output
relations .
  Given a specific inference system, the operational semantics determines a unique
denotation for P: The n-tuple (6, . , . , t,,,) belongs to the denotation of P in A iff A I-
P(ti, . . ., t7,), where X F Y means that there exists a derivation of Y from X. For
resolution systems we employ the convention that X I- Y means that there exists a
refutation of the sentence in clausal form corresponding to X & Y. We use the notation
            DI (P) = {(t1, . . ., tn) : A F P(t„ . . ., tn)}
for the denotation of P in A as determined by operational semantics
  It needs to be emphasized that only goal oriented inference systems correspond to the
standard notion of operational semantics, where procedure calls are replaced by proce-
dure bodies. In theory, however, any inference system for predicate logic specifies,
implicitly at least, an abstract machine which generates exactly those derivations which
are determined by the given inference system
  Notice that in our treatment predicate logic programs compute relations. The relations
computed are denoted by predicate symbols in the defining set of clauses A. Those
special relations which are functions are also denoted by predicate symbols The function
symbols occurring in A do not denote functions computed by the program but construct
the data structures which are the input and output objects of the relations (or functions)
computed.
  It is a significant application of the proof theory of resolution systems to the computa-
tion theory of predicate logic programs that if A is consistent and A I- P(ti, . . ., tn) then
there exists a resolution refutation of A & P(xl, . . ., x„) in which the variables xi, • • • , xn
are eventually instantiated to terms which have t1, . . ., tn. as an instance. More generally,
if A F P(ti, . . ., t.), then for any subset of the arguments ti, . . ., t„ of P there exists a
computation which accepts those arguments of P as input and computes the remaining
arguments as output. A useful practical consequence of this fact is that a predicate logic
program can first be written to test that a given relationship holds among the members of
an n-tuple of objects but can later be used to generate, from some subset of objects in the
n-tuple given as input, the remaining objects in the n-tuple as output. See, for example,
the goal statement 3(a) below. Another important consequence is that variables occur-
ring in input or output can be used to represent incompletely specified data See, for
example, the goal statement 3(b) below. It is these considerations which motivate the
terminology "input-output relation" for the relation denoted by a predicate symbol in a
set of clauses.
  Given a consistent set of clauses A representing a program and given a goal statement
C, the Herbrand universe for A can be different from the Herbrand universe for the set
of clauses AU{C}. Although this is an interesting case to consider, we assume for
simplicity that it does not arise and that C contains only constant symbols and function
symbols occurring in A. Similarly we assume that A always contains at least one constant
symbol.


   Example. The program for appending lists can be activated by the goal statement:
   (3a) Append (x ,cons(a ,y),cons(a ,cons(b ,cons(a ,ni1)))),
where a, b, and nil are constants, and x and y are variables. With this goal statement the
program behaves nondeterministically: There are two computations, one ends with x : =
nil, y := cons(b ,cons(a,ni1)), and the other ends with x := cons(a ,cons(b ,ni1)), y := nil.
Activated by a goal statement with this pattern of constants and variables, the program
checks whether a particular item occurs in the given list and gives a different computation
for each different occurrence. For each occurrence of the item, it determines the list of
items preceding the given occurrence as well as the list following it.
   Example . The program for appending can also be activated by the goal statement:
   (3b) Append(cons(b ,m1),y,z),
where b and nil are constants and y and z are variables. Starting from this goal statement
there is one computation. It ends with z := cons(b,y), which can be interpreted as stating
that z is the list whose head is b and whose tail is the unspecified input y .

# 5. Model-Theoretic Semantics

There is general agreement among logicians concerning the semantics of predicate logic.
This semantics provides a simple method for determining the denotation of a predicate
symbol P in a set of clauses A:
                        D2(P) = {(t1, . . . , tn): A 1= P(ti, . . , t.)},
where X = Y means that X logically implies Y. D2(P) is the denotation of P as
determined by model-theoretic semantics.
   The completeness of first-order logic means that there exist inference systems such
that derivability coincides with logical implication; i.e. for such inference systems X I- Y
iff X 1= Y.
   The equivalence of operational and model-theoretic semantics D1(P) = D2(P) is an
immediate consequence of the completeness of the inference system which determines
D,
   In order to make a comparison of the fixpoint and model-theoretic semantics, we need
a more detailed definition of D2. For this purpose we define the notions of Herbrand
interpretation and Herbrand model.
   An expression (term, literal, clause, set of clauses) is ground if it contains no variables.
The set of all ground atomic formulas P(ti, . . t,,), where P occurs in the set of clauses
A and t„ . . t„ belong to the Herbrand universe H of A, is called the Herbrand base H
of A. A Herbrand interpretation I of A is any subset of the Herbrand base of A. A
Herbrand interpretation simultaneously associates, with every n-ary predicate symbol in
A, a unique n-ary relation over H. The relation {(t1, . tn) : P(4, tn) E I} is
associated by I with the predicate symbol P in A.
(1) A ground atomic formula A is true in a Herbrand interpretation I iff A E I.
(2) A ground negative literal A is true in I iff A E I.
(3) A ground clause L1 V • • V L. is true in I iff at least one literal L, is true in I.
(4) In general a clause C is true in I iff every ground instance Ca of C is true in I. (Ca is
     obtained by replacing every occurrence of a variable in C by a term in H. Different
     occurrences of the same variable are replaced by the same term.)
(5) A set of clauses A is true in I iff each clause in A is true in 1.
   A literal, clause, or set of clauses is false in I iff it is not true. If A is true in I, then we
say that I is a Herbrand model of A and we write 1=, A. It is a simple version of the
Skolem-LOwenheim theorem that a sentence A in clausal form has a model iff it has a
Herbrand model.
   We can now formulate an explicit definition of the denotation determined by the
model-theoretic semantics. Let M(A) be the set of all Herbrand models of A; then
nm(A), the intersection of all Herbrand models of A, is itself a Herbrand interpretation


of A. If A contains the predicate symbol P, then the denotation D2(P) is the relation
associated with P by the Herbrand interpretation nm(A). In symbols,
                 D2(P) = f(th . . ., tn) : P(ti, . . ., tn) E nm(A)}

for any set of clauses A.
  PROOF. (ti, .. ., tn) E D2(P)
         iff A 1= P(ti. . • •, tn),
         iff A U {P(t,, . . ., t,„)} has no model,
         iff A U {1)(4, . . ., tn)} has no Herbrand model,
         iffi)(th . . ., tn) is false in all Herbrand models of A,
         iff P(ti• . . ., tn) is true in all Herbrand models of A,
         iff P(t,, . . ., (-7,) E nm(A).
  Notice that the above equality holds for any set of clauses A even if A is inconsistent.
If A is a consistent set of Horn clauses then nm(A) is itself a Herbrand model of A.
More generally, Horn clauses have the model intersection property: If L is any nonempty
set of Herbrand models of A then ni., is also a model of A.
  PROOF. Assume nt, is not a model of A. Then ni, falsifies some ground instance Ccr
of a clause C E A.
  If C is a procedure declaration, then
    Ccr = A v A, v • • • v A7„, m..... 0, A (t nL, and A1, . . — A. E nL.
Therefore for some I E L, A (t I and At, . . . , A. E I. C is false in I, contrary to
assumption that I E L.
  If C is a goal statement, then
              Ccr = A, V • • • VA., m >0,      A1, . . . , Am E nL.
Therefore for all I E L, A1, . . . , A„, E I. C is false in I, contrary to assumption that
I E L.
  {P(a) V P(b)}, where a and b are constants, is an example of a non-Horn sentence
which does not have the model-intersection property: {{P(a)}, {P(b)}} is a nonempty set
of models, yet its intersection 0 is a Herbrand interpretation which is not a model.

# 6. Fixpoint Semantics

In the fixpoint semantics, the denotation of a recursively defined procedure is defined to
be the minimal fixpoint of a transformation associated with the procedure definition.
Here we propose a similar definition of fixpoint semantics for predicate logic programs.
In order to Justify our definition we first descpbe the fixpoint semantics as it has been
formulated for more conventionally defined recursive procedures. Our description fol-
lows the one given by de Bakker [6]
  Let P 4 B(P) be a procedure declaration in an Algol-like language, where the first
occurrence of P is the procedure name, where B(P) is the procedure body, and where the
occurrence of P in B(P) distinguishes all calls to P in the body of the procedure.
Associated with B is a transformation T which maps sets I of input-output tuples into
other such sets J = T(I). When the transformation T is monotonic (which means that
T(I1) C T(12) whenever /, C /2) the denotation of P is defined as
                                 fl {I : T(I) C 1},
which is identical to the intersection of all fixpoints of T,

                                 no : T(I) = I},

and which is itself a fixpoint (the least such) of T.                     .
  In a similar way a transformation T can be associated with a finite set of mutually
recursive procedure declarations







The minimal fixpoint of T, which exists when T is monotonic, can be decomposed into
components, the ith of which is the denotation of the procedure P,.
 By means of the procedural interpretation, the fixpoint semantics of predicate logic is
defined similarly. A set of Horn clauses of the form A v Ajv • • • V A., where m 0,
is interpreted as a set of mutually recursive, possibly nondeterministic, procedure
declarations We restrict the definition of the fixpoint semantics of predicate logic
programs to sentences A which are sets of such procedure declarations. Associated with
every such sentence A is a transformation T which maps Herbrand interpretations to
Herbrand interpretations. Suppose that P„ . . , P. are the predicate symbols occurring
in A. The transformation T can be defined in terms of individual transformations T,
associated with the individual predicate symbols P,. T, maps Herbrand interpretations I
to Herbrand interpretations./ = T,(I) which contain only atomic formulas beginning with
the predicate symbol P,:
 J, = T,(I) contains a ground atomic formula A E H iff A begins with the predicate
 symbol P, and, for some ground instance Co- of a clause C in A, Co- = A V A 1
 V • • -.V Am and Ai, . . . , Am E /, m 0.
The transformation T associated with A is defined by T(I) = Ti(I) U • • U T.(/).
 The input-output relation associated by J, = T,(I) with P, can be regarded as the
relation obtained by "substituting," for the procedure calls in the declarations of P, in A,
the appropriate input-output relations associated by I. This interpretation of T, is
analogous to the corresponding definition for conventionally defined recursive proce-
dures. A simpler definition of T, which is less directly analogous to the conventional
definition, is the following:
 T(I) contains a ground atomic formula A E H iff for some ground instance Co- of a
 clause C in A, Co- =A VAI V • • •V An, and Ai, . . . , E I, m O.
 Notice that, independently of I, T(1) always contains all ground instances Aci• of
unqualified assertions A in A (corresponding to the case m = 0 in the definition of T(I)).
 Let C(A) be the set of all Herbrand interpretations closed under the transformation T,
i.e. I E C(A) iff T(I) C I. The denotation of a predicate symbol P occurring in a set of
procedure declarations A, as determined by the fixpoint semantics, is
                D3(P) = {(t1, . . . , t„) : P(ti, .. , tn) E nc(A)}.
As a corollary of the theorem below, nc(A) is itself closed under T and therefore D3(P)
is the smallest set of input-output tuples closed under T. In conventional fixpoint theory
this fact is proved by using the monotonicity of T.

# 7. Model-Theoretic and Fixpoint Semantics

We shall show that for sets of procedure declarations A, model-theoretic and fixpomt
semantics coincide: D, = D3. It would be sufficient to show that nm(A) = nC(A), but it
is easy to prove that even M(A) = C(A).
  In other words, a Herbrand interpretation I of A is a model of A iff I is closed under
the transformation T associated with A.
 THEOREM. If A is a set of procedure declarations, then M(A) = C(A), i.e. b A iff
T(I) C I, for all Herbrand interpretations I of A
  PROOF. (1=/ A implies T(I) C I.) Suppose that / is a model of A. We waist to show./ =
T(I) C I, i.e. that if A E J then A E I.


   Assume that A E J; then by the definition of T, for some C E A and for some ground
instance Ccr of C,
                        Co- = A V iii V • • • V A. and Ai, . . . , A. E .1.
Because / is a model of A, Ca is true in I. But then A is true in I, because Al, . . . , and An
are false in I. Therefore A E I.
   (T(1) C I implies A). Suppose that I is not a model of A. We want to show that T(I)
ct I. But I falsifies some ground instance Co. of a clause C in A, where Cu = A V Al
V • ' • VAm, m .. 0. Because I falsifies Co•, A E I and Al, . . . , Am E I. But then, be-
cause A1, . . . , Am E I, it follows that A E T(I). Therefore T(I)C I.
   COROLLARY. If A is a set of procedure declarations, then nc(A) is closed under T.
   PROOF. nc(A) = nm(A) by the model-intersection property is a model of A and by
the theorem is therefore closed under T.

# 8. Operational and Fixpotnt Semantics, Hyperresolution

The equivalence D, = D3 between operational and fixpoint semantics, which follows
from the equivalences D, = D2 and D2 = D3, has different interpretations depending
upon the inference system which determines D,. Here we investigate the interpretation
associated with a particular inference system based upon hyperresolution 1_211.
   For ground procedure declarations the definition of hyperresolution is very simple:
   An atomic formula A is the hyperresolvent of ground clauses A VA 1 v • • • V A. and
   A „ . . . , A.. A is said to be obtained from AV A, V • • - V A. and A1, . . . , A. by
   hyperresolution.
The connection with fixpoint semantics is obvious: If T is the transformation associated
with the set of procedure declarations A and if I is a Herbrand interpretation of A, then
TV) is the set of all ground instances of assertions in A together with all hyperresolvents
derivable in one step from ground instances of clauses in A and from assertions in I. It
follows that
   A is derivable by means of a hyperresolution derivation from ground instances of
   clauses in A if A E U°4,..0 Tm (0) where P(0) = 0 and T"-"(0) = T(T"(0))•
Let Dr be the operational semantics associated with the two inferences rules of ground
instantiation of clauses in A and ground hyperresolution, i.e define
                    (4, . . . , t„) E DP(P) if P(t„ . . . , tn) e CI Tm(0)..--0
The equivalence of D r and the model-theoretic semantics D2 is the completeness, for
Horn clauses, of the inference system whose inference rules are ground instantiation of
input clauses and ground hyperresolution. Completeness can be proved using standard
resolution-theoretic arguments. Here we present an alternative direct proof that for any
set of declarations A with associated transformation T, t.4.0 r "(0) = nM(A).
   PROOF. Let U abbreviate Ll; -0 Tm(0).
   (U C CIM(A)). Suppose that A E U. Then A is derivable by means of a hyperresolu-
tion derivation from ground instances of clauses in A. By the correctness of hyperresolu-
tion and instantiation, A = A and therefore A E nm(A).
   (nm(A) c U). We show that U is closed under T, because then U E M(A), and
therefore nm(A) C U. Suppose that A E T(U). By the definition of T, either A is an
instance of an unqualified assertion in A or some clause A VA i V • • VA?, is an instance
of a clause in A and A1, . . — An E U. In the first case A E U, because A E Tm(0), m > 0.
In the second case A 1, . . . , A. E TN(0) for some N _._ 0, and therefore A E TN+1(0)and
A E U. Therefore U is closed under T.
   Therefore for sets of declarations, D' = D2.
   Because of the equivalence between model-theoretic and fixpoint semantics, we also
have that Dr = D3, i.e. tr„,-..0 rn(0) = ny : T(I) C /1.

The Semantics of Predicate Logic as a Programming Language 741

  This last fact is usually proved in the fixpoint theory by demonstrating the continuity of
the transformation T.

# 9. Conclusion

For arbitrary sentences X and Y of first-order predicate logic, proof theory determines
when X I- Y and model theory determines when X I= Y. We have argued that in the
procedural interpretation, operational semantics is proof theory and fixpoint semantics
is model theory. On the other hand, operational and fixpoint semantics only deal with the
case where Y is a set of ground atomic formulas. Moreover, fixpoint semantics only deals
with X, a set of procedure declarations. We believe that the added generality of proof
theory and model theory has useful consequences.
  The completeness theorem of first-order logic states that the relations I- of derivability
and of logical implication are equivalent. For goal oriented inference systems this
equivalence establishes that various computation rules compute the relation determined
by the fixpoint semantics. More generally, this equivalence can be used to justify various
rules (such as Scott's induction rule [61) for proving properties of programs.
  We have argued that various notions of the conventional theory of computing can be
understood in terms of the classical theory of predicate logic. We believe moreover that
the predicate logic theory has further contributions to make both to the theory and to the
practice of computing.

# Acknowledgments

We are indebted to Michael Gordon for his interest and useful
criticism of work leading to this paper. Thanks are due also to Keith Clark, Alain
Colmerauer, Gerard Huet, David Park, and Willem-Paul de Roever for their helpful
comments on earlier versions of the paper. Suggestions from the referees have also been
incorporated in the paper.

# References

 1 BEKIC, I-1 Definable operations in general algebra, and the theory of automata and flow charts IBM Res
   Rep , Vienna, 1971
 2 BLEDSOE, W W Splitting and reduction heuristics in automatic theorem proving Artif Intel 2 (1971),
   55-77
 3 BURSTALL, R M Formal description of program structure and semantics in first order logic. In Machine
   Intelligence 5, B Meltzer and D Michie, Eds , Edinburgh U Press, Edinburgh, 1969, pp 79-98
 4 CHURCH, A Introduction to Mathematical Logic, Vol 1 Princeton U Press, Princeton, N J , 1956
 5 COLMERAUER, A , KANOUI, H , PASERO, R., AND ROUSSEL, P. Un systeme de communication homme-
   machine en francais. Groupe d'Intelligence Artificielle, U E R de Luminy, Universite d'Aix-Marseille,
   Lummy, 1972
 6 DE BAKKER, J W Recursive procedures Tract No 24, Mathematical Centre, Amsterdam, 1971
 7 DE BAKKER, J W , AND DE ROEVER, W P A calculus of recursive program schemes. In Automata,
   Languages and Programming, M Nivat, Ed , North-Holland Pub Co , Amsterdam, 1973, pp 167-
   196
 8 ERNST, G W The utility of independent subgoals in theorem proving Inform Conti- 18, 3 (April 1971),
   237-252
 9 KANOUI, H Application de la demonstration automatique aux manipulations algebriques et a l'integration
   formelle sur ordmateur Groupe d'Intelligence Artificielle, U E R de Luminy, Universite d'Aix-
   Marseille, Luminy, 1973.
10 KOWALSKI, R Predicate logic as programming language Proc IFIP Cong 1974, North-Holland Pub
   Co , Amsterdam, 1974, pp 569-574
I t KOWALSKI, R Logic for problem-solving DCL Memo 75, Dep Artificial Intelligence, U. of Edinburgh,
   Edinburgh, 1974
12 KowAtsxi, R , AND KUEHNER, D Linear resolution with selection function Artif Intel 2 (1971), 227-
   260
13. LOVELAND, D.W A simplified format for the model elimination theorem-proving procedure. J. ACM 16,
   3 (July 1969), 349-363
14 MANNA, Z Properties of programs and the first-order predicate calculus J ACM 16, 2 (April 1969),
   244-255
15 MILNER, R Implementation and applications of Scott's logic for computable functions. Proc. ACM Conf
   on Proving Assertions About Programs, Jan 1972, pp 1-6

742                      M. H. VAN EMDEN AND R. A. KOWALSKI

16. NILSSON, N J Problem Solving Methods in Artificial Intelligence. McGraw-Hill, New York, 1971
17 PARK, D Fixpoint induction and proofs of program properties In Machine Intelligence 5, B Meltzer and
  D Michie, Eds , Edinburgh U Press, Edinburgh, 1969, pp 59-78
18 PASgRO, R. Representation du francais en logique du premier ordre en vue de dialoguer avec un
   orchnateur Group d'Intelligence AnthemIle, U.E R de Luminy, Universite d'Aix-Marseille, Luminy,
   1973.
19 REFFER, R Two results on ordering for resolution with merging and linear format J ACM 18, 4 (Oct
   1971), 630-646
20. ROBINSON, J A A machine-oriented logic based on the resolution principle. I ACM 12, 1 (Jan. 1965),
  23-41
21 ROBINSON, J A Automatic deduction with hyper-resolution Int J Comptr Math 1 (1965), 227-234
22 SCOTT, D Outline of a mathematical theory of computation Tech Monog PRG-2, Comptg Lab ,
   Oxford U , Oxford, England
23 SLAGLE, J R , AND KONIVER, P Finding resolution graphs and using duplicate goals in AND/OR trees
   Inform Sci 3 (1971), 315-342
24 WARREN, D H D WARPLAN A system for generating plans DCL Memo 76, Dep. of Artificial Intelli-
   gence, U of Edinburgh, Edinburgh, 1974

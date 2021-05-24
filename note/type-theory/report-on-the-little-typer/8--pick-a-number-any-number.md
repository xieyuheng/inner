# 8. Pick a Number, Any Number

2021-05-24

## Sameness is judgment that can be expressed by a type constructor

Sameness is indeed a judgment. But, Another sandwich?
with a new type constructor, types can
express a new idea called equality.

We can express two functions always find the same result.

This is a new perspective on types.

Types can be read as statements (propositions).

Xie: Recall that,
judgment is attitude person take towards expression,
judgment is attitude person take when thinking about expression,
maybe all form of judgment can be expressed by types (types are again expression),
but we should not mix the two terms "judgment" and "type",
because in some moment, maybe we do not know
how to express a form of judgment as type.

Judgments correspond to the concepts we used in our implementation of a language,
in a implementation these concepts will be implemented as functions like:
- `check`
- `check_type`
- `check_same`

But "forall" and "exists" are special, they do not correspond to functions in implementation.

Creating expressions that capture the ideas behind a form of judgment
is sometimes called internalizing the form of judgment.

## judging a statement to be true

If a type can be read as a statement,
then judging the statement to be true means that
there is an expression with that type.

Thus in this view,
truth means that we have evidenced,
and this evidence is called a proof.

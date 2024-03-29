---
title: inductive type
---

# W (well-founded type)

```cicada
W(A: type, B: (A) -> type) : type
W(A: type, B: (A) -> type) = datatype {
  sup(a: A, b: (B(a)) -> W(A, B)) : W(A, B)
}

// one name for both `:` and `=`

W(A: type, B: (A) -> type) : type = datatype {
  sup(a: A, b: (B(a)) -> W(A, B)) : W(A, B)
}

// without sugar

W : (A: type, B: (A) -> type) -> type
W = (A: type, B: (A) -> type) => datatype {
  sup : (a: A, b: (B(a)) -> W(A, B)) -> W(A, B)
}

// constrain by `datacons`

W : (A: type, B: (A) -> type) -> type
W = (A: type, B: (A) -> type) => datatype {
  sup : (a: A, b: (B(a)) -> W(A, B)) -> datacons {}
}
```

# nat_t by W

```cicada
// F(n) is finite set of n elements

f : (F(2)) -> type
f(1) = F(0)
f(2) = F(1)

nat_t : type
nat_t = W(F(2), f)

zero : nat_t
zero = sup(1, b: (F(0)) -> nat_t)
where
  b(_) = zero
  // does this definition make sense???
  //   what is elements of F(0)???


one : nat_t
one = sup(2, b: (F(1)) -> nat_t)
where
  b(1) = zero
```

# (coquand, paulin) inductively defined types

## 1.1 Generalized inductive types

Given an expression `F: (X: U) -> U`
`strictly_positive_p(F)` is defined to be true
if one of the following holds

1. not_occur(X, F(X))
2. F(X) = X
3. F(X) = (k: K) -> (G(X))
   and not_occur_p(X, K)
   and strictly_positive_p(G)

# (1999) (dybjer, setzer) a finite axiomatization of inductive recursive definitions

# (2000) (dybjer) a general formulation of simultaneous inductive-recursive definitions in type theory

# (2003) (dybjer, setzer) induction-recursion and initial algebras

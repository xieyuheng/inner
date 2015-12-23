module tutorial-of-agda where

data Bool : Set where
  true : Bool
  false : Bool

-- bool
--   (-> type)
-- true
--   (-> bool)
-- false
--   (-> bool)

not : Bool -> Bool
(not true) = false
(not false) = true

-- not
--   (bool -> bool)

or : Bool -> Bool -> Bool
(or false x) = x
(or true _)  = true

-- or
--   (bool bool -> bool)

and : Bool -> Bool -> Bool
(and true x) = x
(and false _)  = false

iff : Bool -> Bool -> Bool
(iff true true) = true
(iff true false) = false
(iff false true) = false
(iff false false) = true

xor : Bool -> Bool -> Bool
(xor a b) = (not (iff a b))

-- if for lazy eval
if : {n : _} {A : Set n} -> Bool -> A -> A -> A
(if true x y) = x
(if false x y) = y

data Natural : Set where
  zero : Natural
  succ : Natural -> Natural

-- natural
--   (-> type)
-- zero
--   (-> natural)
-- succ
--   (natural -> natural)


add : Natural -> Natural -> Natural
(add zero n) = n
(add (succ m) n) = (succ (add m n))

mul : Natural -> Natural -> Natural
(mul zero n) = zero
(mul (succ m) n) = (add n (mul m n))

equal? : Natural -> Natural -> Bool
(equal? zero zero) = true
(equal? zero (succ n)) = false
(equal? (succ n) zero) = false
(equal? (succ n) (succ m)) = (equal? n m)

data Sum (A : Set) (B : A -> Set) : Set where
  type-sum : (first : A) -> (second : B first) -> (Sum A B)

-- sum
--   (type #a (a -> type) #b -> type)
-- type-sum
--   (a #first
--    first b #second -> (sum a b))

example-1 : Sum Natural (λ x -> (if (equal? x zero) Natural Bool))
example-1 = (type-sum zero zero)

-- example-1 ><><><
--   (-> natural [] sum)

example-2 : Sum Natural (λ x -> (if (equal? x zero) Natural Bool))
example-2 = (type-sum (succ zero) false)

Either : Set -> Set -> Set
Either A B = (Sum Bool (λ x -> (if x A B)))

left : {A B : _} ->
       A -> Either A B
(left x) = (type-sum true x)

right : {A B : _} ->
        B -> Either A B
(right x) = (type-sum false x)

either : {A B C : Set} ->
         (A -> C) -> (B -> C) -> Either A B -> C
either f g (type-sum true x) = f x
either f g (type-sum false x) = g x

Pair : Set -> Set -> Set
(Pair A B) = (Sum A (λ _ -> B))

uncurry : {A B C : Set} ->
          (A -> B -> C) -> Pair A B -> C
(uncurry f (type-sum x y)) = (f x y)

compose : {A : Set}
          {B : A -> Set}
          {C : {x : A} -> (B x) -> Set} ->
          (f : {x : A} (y : B x) -> (C y)) ->
          (g : (x : A) -> (B x)) ->
          ((x : A) -> (C (g x)))
(compose f g) = (λ x -> (f (g x)))

-- compose
--   ({type #a}
--    {(a -> type) #b}
--    {({a #x} x b #y -> type) #c}
--    ({a #x} x b #y -> y c) #f
--    (a #x -> x b) #g
--    -> (a #x -> x g c))

data Equal {A : Set} (q : A) : A -> Set where
  reflexive : (Equal q q)

-- equal
--   ({type #a} a a -> type)
-- reflexive
--   (-> {type #a a #q} q q equal)

cong : {A B : _} (f : A -> B)
       {x y : _} -> (Equal x y) -> (Equal (f x) (f y))
(cong f reflexive) = reflexive

-- cong
--   ({_ #a _ #b} (a -> b) #f
--    {_ #x _ #y} x y equal -> x f y f equal)

associative : (x y z : Natural) ->
              (Equal (add x (add y z)) (add (add x y) z))
(associative zero y z) = reflexive
(associative (succ x) y z) = (cong succ (associative x y z))

-- associative
--   (natural #x #y #z
--    z y add x add z y x add add equal)

-- commutative : (x y : Natural) -> (Equal (add x y) (add y x))
-- commutative zero zero = reflexive
-- commutative (succ x) zero = (cong succ (commutative x zero))
-- commutative zero y = {!!}
-- commutative (succ x) y = {!!}

-- data Vector (A : Set) : Natural -> Set where
--   vector:empty : Vector A zero
--   vector:cons : (n : Natural) -> A -> (Vector A n) -> (Vector A (succ n))

-- inner : (n : Natural) -> Vector Natural n -> Vector Natural n -> Natural
-- (inner zero vector:empty vector:empty) = zero
-- (inner (succ .n) (vector:cons .n x xs) (vector:cons n y ys)) = (add (mul x y) (inner n xs ys))

-- inner : (n : Natural) -> Vector Natural n -> Vector Natural n -> Natural
-- (inner ._ vector:empty vector:empty) = zero
-- (inner ._ (vector:cons ._ x xs) (vector:cons _ y ys)) = (add (mul x y) (inner _ xs ys))

data Vector (A : Set) : Natural -> Set where
  vector:empty : Vector A zero
  vector:cons : {n : Natural} -> A -> (Vector A n) -> (Vector A (succ n))

-- vector
--   (type natural -> type)
-- vector:empty
--   (-> {type #t} t 0 vector)
-- vector:cons
--   ({type #t} {natural #n}
--    t t n vector -> t n add1 vector)
-- vector:empty vector:empty
--   (-> {type #t} t 0 vector {type #t} t 0 vector)
-- vector:empty vector:empty vector:cons
--   (-> {type #t} t 0 vector 1 vector)

inner : {n : Natural} -> Vector Natural n -> Vector Natural n -> Natural
(inner vector:empty vector:empty) = zero
(inner (vector:cons x xs) (vector:cons y ys)) = (add (mul x y) (inner xs ys))

-- vector:inner
--   ({natural #n}
--    natural n vector natural n vector -> natural)

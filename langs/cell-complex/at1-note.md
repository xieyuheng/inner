# simple-space

- In a language, we always have primitive elements,
  and many ways to compose elements to new compound element,
  also many ways to derive new element from old one.

- Here, I describe a simple way to construct spaces,
  so constructed spaces will be called simple-spaces,
  which constitute the first kind of primitive space in my language.

  Before having other ways to construct spaces,
  I will simply call them 'space' instead of 'simple-space'.

  - In algebraic topology,
    our simple-space is called
    CW-complex, cell-complex or cellular polytopes.

- A space is constructed part by part.

- A part is of certain dimension.

  | dim | name     |
  |-----+----------|
  |   0 | point    |
  |   1 | interval |
  |   2 | disk     |
  |   3 | ball     |
  |   4 | 4-cell   |
  |   n | n-cell   |

- The way to construct a space from parts,
  goes from low dimension to high dimension,
  by attaching the boundary of a n-cell,
  to a (n-1)-sphere in the space.

  Our principle here is 'construction by attaching boundary'.

- As an example, let us construct a [solid] tetrahedron.

  - [advice for reader]
    Draw a picture by yourself, when trying to follow
    my formal description.

    And imagining how such picture can be dynamicly
    and automaticly drawn by a drawer that
    accompanies the main interpreter of the language.

  - In 0 dimension,

    we name all the points of the space.

    ```scheme
    (type space
      (: [a1, a2, a3, a4] (@ <>)))
    ```

  - In 1 dimension,

    we name all the intervals of the space,

    and for each interval,
    we attach its two end points to two points of the space
    [the two points of the space might be the same point],

    so that the boundary of the interval
    can be viewed as two points of the space.

    ```scheme
    (type space
      (: [a1, a2, a3, a4] (@ <>))
      (: b12 (0 a1 a2))
      (: b13 (0 a1 a3))
      (: b14 (0 a1 a4))
      (: b23 (0 a2 a3))
      (: b24 (0 a2 a4))
      (: b34 (0 a3 a4)))
    ```

    For example, the boundary of 'b12' are 'a1' and 'a2',
    the boundary of 'b13' are 'a1' and 'a3'.

    One can view 'b12' as a directed path pointing from 'a1' to 'a2',
    and 'b13' as a directed path pointing form 'a1' to 'a3'.

    We write the two points in the keyword '(0 ...)',
    and the order matters.

    Since, 'b12' and 'b13' have a common boundary -- 'a1',
    we view 'b12' and 'b13' as glued together by the common boundary.

    Our principle here is 'gluing by named common boundary'.

  - In 2 dimension,

    we name all the disk of the space,

    and for each disk,
    we attach its boundary circle to a circle in the space.

    ```scheme
    (type space
      (: [a1, a2, a3, a4] (@ <>))
      (: b12 (0 a1 a2))
      (: b13 (0 a1 a3))
      (: b14 (0 a1 a4))
      (: b23 (0 a2 a3))
      (: b24 (0 a2 a4))
      (: b34 (0 a3 a4))
      (: c123 (1 b12 b23 b13 rev))
      (: c124 (1 b12 b24 b14 rev))
      (: c134 (1 b13 b34 b14 rev))
      (: c234 (1 b23 b34 b24 rev)))
    ```

    I use the keyword '(1 ...)' to specify path in the space.

    For example, '(1 b12 b23)' denotes
    the path which goes through 'b12' forwardly and 'b23' forwardly.

    In the keyword '(1 ...)',
    the right end point of one interval must matches
    the left end point of the next interval.

    And '(1 b12 b23 b13 rev)' denotes the path which
    goes through 'b12' forwardly, 'b23' forwardly, and 'b13' backwardly.

    The boundary of 'c123' is attached to the circle '(1 b12 b23 b13 rev)'.

    We check whether a path is a circle,
    by checking whether the left end point of the first interval,
    is equal to the right end point of the last interval,
    i.e. whether the path is closed.

  - In 3 dimension,

    we name all the ball of the space,

    and for each ball,
    we attach its boundary sphere to a sphere in the space.

    ```scheme
    (type space
      (: [a1, a2, a3, a4] (@ <>))
      (: b12 (0 a1 a2))
      (: b13 (0 a1 a3))
      (: b14 (0 a1 a4))
      (: b23 (0 a2 a3))
      (: b24 (0 a2 a4))
      (: b34 (0 a3 a4))
      (: c123 (1 b12 b23 b13 rev))
      (: c124 (1 b12 b24 b14 rev))
      (: c134 (1 b13 b34 b14 rev))
      (: c234 (1 b23 b34 b24 rev))
      (: d1234 (2 c123
                  c124 (1 b14 b24 rev b23 b13 rev) as-remained-boundary
                  c134 (1 b34 b24 rev b23) as-remained-boundary
                  c234 (1) as-remained-boundary)))
    ```

    I use the keyword '(2 ...)' to specify polygons in the space.
    Note that, a polygon might be obtained by gluing many polygons together.

    For example :
    ```scheme
    (2 c123)
    ;; a polygon in a stack

    (2 c123
       c124)
    ;; two polygons in the stack

    (2 c123
       c124 (1 b14 b24 rev b23 b13 rev))
    ;; two polygons and a circle in the stack

    (2 c123
       c124 (1 b14 b24 rev b23 b13 rev) as-remained-boundary)
    ;; 'as-remained-boundary' is a function,
    ;;   which takes two polygons and a circle out from the stack,
    ;;   try cancel out part of the common boundary of 'c123' and 'c124',
    ;;   so that the remained boundary can be '(1 b14 b24 rev b23 b13 rev)'.
    ;; if there are no way or more then one way to do this,
    ;;   it reports to the user.
    ;; if there is only one way to do this,
    ;;   it puts a polygon back to the stack,
    ;;   whose boundary is '(1 b14 b24 rev b23 b13 rev)'.
    ```

    The boundary of 'd1234' is attached to the sphere :
    ```scheme
    (2 c123
       c124 (1 b14 b24 rev b23 b13 rev) as-remained-boundary
       c134 (1 b34 b24 rev b23) as-remained-boundary
       c234 (1) as-remained-boundary)
    ```

    We check whether a polygon is sphere [closed polygon],
    by checking whether the polygon is
    2-dimensional, closed, connected and orientable.

    Note that, we can implement more functions like 'as-remained-boundary',
    to help us get 2-dimensional polygons.

- Note that, syntax in (0 ...) (1 ...) (2 ...) are different,
  but syntax in (2 ...) (3 ...) (4 ...) ... are similar.

  - (0 ...) is special, in the sense that,
    only two 0-dimensional points can occur in it.

  - (1 ...) is special, in the sense that,
    there is not explict functions, like 'as-remained-boundary' in it.

  - [hesitation about syntax]
    Should these three distinctions be unified?
    If these distinctions are really meaningful,
    and ought not to be unified,
    Should we design distinct syntaxes for them,
    to maintain the distinctions,
    instead of using the seemingly unified syntax?

- Note that, 'as-remained-boundary' involves searching,
  which makes the specification of part of the space implicit,
  such implicitness is need, for when the dimension gets higher,
  the detail of high dimension information might be too complex
  to use an explict method.

- [hesitation about cobordism]
  Note that, not all closed spaces
  can be boundary of a 1-dim higher space.
  if two disjoint closed spaces, B1 and B2, are boundary of
  1-dim higher space C, then C is the cobordism of B1 and B2,
  classically expressed as (C; B1, B2),
  where B1 and B2 are called cobordant.

  Under what conditions, a closed space can be
  the boundary of a 1-dim higher space?
  This question should be thoroughly understood,
  before developing the formal semantics of the language.

- [summary of principles]
  - [principle 1] construction by attaching boundary
  - [principle 2] gluing by named common boundary

# (~~ bool-suspend sphere-1)

```scheme
(define sphere-1
  (type space
    (: b (-1 <>))
    (: loop (0 b b))))

(define bool
  (type space
    (: [#f, #t] (-1 <>))))

(define bool-suspend
  (type space
    (: [n, s] (-1 <>))
    (: m (-> bool (0 n s)))))

(define f
  (lambda (-> bool-suspend sphere-1)
    (with (-> (-1 bool-suspend) (-1 sphere-1))
      (-> n b)
      (-> s b))
    (with (-> (0 n s) (0 b b))
      (-> (1 #f m) (1 loop))
      (-> (1 #t m) (1 b refl)))))

(define g
  (lambda (-> sphere-1 bool-suspend)
    (with (-> (-1 sphere-1) (-1 bool-suspend))
      (-> b n))
    (with (-> (0 b b) (0 n n))
      (-> (1 loop) (1 #f m #t m rev)))))

(note
  [g f] is already id of sphere-1)

(define [g f]
  (lambda (-> sphere-1 sphere-1)
    (with (-> (-1 sphere-1) (-1 sphere-1))
      (-> b b))
    (with (-> (0 b b) (0 b b))
      (-> (1 loop) (1 loop)))))

(define [f g]
  (lambda (-> bool-suspend bool-suspend)
    (with (-> (-1 bool-suspend) (-1 bool-suspend))
      (-> n n)
      (-> s n))
    (with (-> (0 n s) (0 n n))
      (-> (1 #f m) (1 #f m #t m rev))
      (-> (1 #t m) (1 n refl)))))

(note
  'h' is to proof (~ [f g] [bool-suspend id]))

(define h
  (lambda (-> (* bool-suspend I) bool-suspend)
    (extend-from
      (lambda  (-> (* bool-suspend (-1 I)) bool-suspend)
        (-> (* :x i0) [:x f g])
        (-> (* :x i1) :x)))
    (with (-> (* (-1 bool-suspend) %:a (0 i0 i1))
              (0 (* :a i0) <> (* :a i1) <>))
      (-> (* n (1 i01)) (1 n refl)
          (:> (0 n n)))
      (-> (* s (1 i01)) (1 #t m)
          (:> (0 n s))))
    (with (-> (* (0 n s) %:b (0 i0 i1) %:i)
              (1 (* :b i0) <> (* s :i) <>
                 (* :b i1) <> rev (* n :i) <> rev))
      (-> (* (1 #f m) (1 i01)) (2)
          (:> (1 (1 #f m #t m rev) (1 #t m)
                 (1 #f m) rev (1 n refl) rev)))
      (-> (* (1 #t m) (1 i01)) (2)
          (:> (1 (1 n refl) (1 #t m)
                 (1 #t m) rev (1 n refl) rev))))))
```

# (~~ bool-suspend-suspend sphere-2)

```scheme
(define sphere-2
  (type space
    (: b2 (-1 <>))
    (: surf (1 b2 refl))))

(define bool-suspend-suspend
  (type space
    (: [n2, s2] (-1 <>))
    (: m2 (-> bool-suspend (0 n2 s2)))))

(: [n m2] (0 n2 s2))
(: [s m2] (0 n2 s2))
(: [#f m m2] (1 n m2 s m2 rev))
(: [#t m m2] (1 n m2 s m2 rev))

(define f
  (lambda (-> bool-suspend-suspend sphere-2)
    (with (-> (-1 bool-suspend-suspend) (-1 sphere-2))
      (-> n2 b2)
      (-> s2 b2))
    (with (-> (0 n2 s2) (0 b2 b2))
      (-> (1 n m2) (1 b2 refl))
      (-> (1 s m2) (1 b2 refl)))
    (with (-> (1 n m2 s m2 rev) (1 b2 refl))
      (-> (2 #f m m2) (2 surf))
      (-> (2 #t m m2) (2 b2 refl refl)))))

(define g
  (lambda (-> sphere-2 bool-suspend-suspend)
    (with (-> (-1 sphere-2) (-1 bool-suspend-suspend))
      (-> b2 n2))
    (with (-> (1 b2 refl) (1 n2 refl))
      (-> (2 surf) (2 #f m m2 (1 n m2 s m2 rev) as-remained-boundary
                      #t m m2 (1) as-remained-boundary)))))

(define [g f]
  (lambda (-> sphere-2 sphere-2)
    (with (-> (-1 sphere-2) (-1 sphere-2))
      (-> b2 b2))
    (with (-> (1 b2 refl) (1 b2 refl))
      (-> (2 surf) (2 surf)))))

(note
  (2 surf)
  g =>
  (2 #f m m2 (1 n m2 s m2 rev) as-remained-boundary
     #t m m2 (1) as-remained-boundary)
  f =>
  (2 (2 surf) (1 (1 b2 refl) (1 b2 refl) rev) as-remained-boundary
     (2 b2 refl refl) (1) as-remained-boundary)
  ==
  (2 surf))

(define [f g]
  (lambda (-> bool-suspend-suspend bool-suspend-suspend)
    (with (-> (-1 bool-suspend-suspend) (-1 bool-suspend-suspend))
      (-> n2 n2)
      (-> s2 n2))
    (with (-> (0 n2 s2) (0 n2 n2))
      (-> (1 n m2) (1 n2 refl))
      (-> (1 s m2) (1 n2 refl)))
    (with (-> (1 n m2 s m2 rev) (1 n2 refl))
      (-> (2 #f m m2) (2 #f m m2 (1 n m2 s m2 rev) as-remained-boundary
                         #t m m2 (1) as-remained-boundary))
      (-> (2 #t m m2) (2 n2 refl refl)))))

(note
  'h' is to proof (~ [f g] [bool-suspend-suspend id]))

(define h
  (lambda (-> (* bool-suspend-suspend I) bool-suspend-suspend)
    (extend-from
      (lambda (-> (* bool-suspend-suspend (-1 I)) bool-suspend-suspend)
        (-> (* :x i0) (* [:x f g]))
        (-> (* :x i1) (* :x))))
    (with (-> (* (-1 bool-suspend-suspend) %:a (0 i0 i1))
              (0 (* :a i0) <> (* :a i1) <>))
      (-> (* n2 (1 i01)) (1 n2 refl)
          (:> (0 n2 n2))
          (note
            an alternative might be :: (1 n m2 s m2 rev)))
      (-> (* s2 (1 i01)) (1 s m2)
          (:> (0 n2 s2))
          (note
            an alternative might be :: (1 n m2))))
    (with (-> (* (0 n2 s2) %:b (0 i0 i1) %:i)
              (1 (* (1 :b) i0) <> (* s2 (1 :i)) <>
                 (* (1 :b) i1) <> rev (* n2 (1 :i)) <> rev))
      (-> (* (1 n m2) (1 i01)) (2 #t m m2)
          (:> (1 (1 n2 refl) (1 s m2)
                 (1 n m2) rev (1 n2 refl) rev)
              (1 (1 s m2) (1 n m2) rev)))
      (-> (* (1 s m2) (1 i01)) (2)
          (:> (1 (1 n2 refl) (1 s m2)
                 (1 s m2) rev (1 n2 refl) rev)
              (1 (1 s m2)
                 (1 s m2) rev)
              (1)))      )
    (with (-> (* (1 n m2 s m2 rev) %:c (0 i0 i1) %:i)
              (2 (* (1 n m2) (1 :i)) <>
                 (1 (* n2 (1 i01)) <>
                    (* (1 n m2) i1) <>
                    (* s2 (1 i01)) <> rev
                    (* (1 n m2) i0) <> rev)
                 as-remained-boundary
                 (* (1 s m2) (1 :i)) <>
                 (1 (* (1 n m2) i1) <>
                    (* (1 n m2) i0) <> rev
                    (* (1 s m2) i1) <> rev
                    (* (1 s m2) i0) <>)
                 as-remained-boundary
                 (* (2 :c) i0) <>
                 (1 (* (1 n m2) i1) <>
                    (* (1 s m2) i1) <> rev)
                 as-remained-boundary
                 (* (2 :c) i1) <>
                 (1) as-remained-boundary))
      (-> (* (2 #f m m2) (1 i01)) (3)
          (:> (2 (2 #t m m2)
                 (1 (1 n2 refl)
                    (1 n m2)
                    (1 s m2) rev
                    (1 n2 refl) rev)
                 as-remained-boundary
                 (2)
                 (1 (1 n m2)
                    (1 n2 refl) rev
                    (1 s m2) rev
                    (1 n2 refl) rev)
                 as-remained-boundary
                 (2 #f m m2 (1 n m2 s m2 rev)
                    #t m m2 (1))
                 (1 (1 n m2)
                    (1 s m2) rev)
                 as-remained-boundary
                 (2 #f m m2)
                 (1) as-remained-boundary)))
      (-> (* (2 #t m m2) (1 i01)) (3)
          (:> (2 (2 #t m m2)
                 (2)
                 (2 n2 refl refl)
                 (2 #t m m2)
                 (1) as-final-boundary))))))
```

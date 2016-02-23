(define-syntax var
  (syntax-rules ()
    [(var w) (vector w)]))

(define-syntax var?
  (syntax-rules ()
    [(var? w) (vector? w)]))

(define walk
  ;; (term substitution -> term)
  (lambda (v s)
    (cond [(var? v)
           (cond [(assq v s) =>
                  (lambda (a)
                    ;; cdr as right-hand-side
                    (let ([v^ (cdr a)])
                      (walk v^ s)))]
                 [else v])]
          [else v])))

;; extend-substitution
(define ext-s
  ;; (var term substitution -> substitution)
  (lambda (x v s)
    (cons (cons x v) s)))

(define unify
  ;; (term term substitution -> (substitution or #f))
  (lambda (v w s)
    (let ([v (walk v s)]
          [w (walk w s)])
      (cond [(eq? v w) s]
            [(var? v) (ext-s v w s)]
            [(var? w) (ext-s w v s)]
            [(and (pair? v) (pair? w))
             (cond [(unify (car v) (car w) s) =>
                    (lambda (s)
                      (unify (cdr v) (cdr w) s))]
                   [else #f])]
            [(equal? v w) s]
            [else #f]))))

(define occurs-check
  ;; (var term substitution -> bool)
  (lambda (x v s)
    (let ((v (walk v s)))
      (cond [(var? v) (eq? v x)]
            [(pair? v)
             (or
              (occurs-check x (car v) s)
              (occurs-check x (cdr v) s))]
            [else #f]))))

(define ext-s-check
  ;; (var term substitution -> (substitution or #f))
  (lambda (x v s)
    (cond [(occurs-check x v s) #f]
          [else (ext-s x v s)])))

(define unify-check
  ;; (term term substitution -> (substitution or #f))
  (lambda (v w s)
    (let ([v (walk v s)]
          [w (walk w s)])
      (cond [(eq? v w) s]
            [(var? v) (ext-s-check v w s)]
            [(var? w) (ext-s-check w v s)]
            [(and (pair? v) (pair? w))
             (cond [(unify-check (car v) (car w) s) =>
                    (lambda (s)
                      (unify-check (cdr v) (cdr w) s))]
                   [else #f])]
            [(equal? v w) s]
            [else #f]))))

(define walk*
  ;; (term substitution -> term)
  (lambda (v s)
    (let ((v (walk v s)))
      (cond [(var? v) v]
            [(pair? v)
             (cons
              (walk* (car v) s)
              (walk* (cdr v) s))]
            [else v]))))

;; reify-substitution
(define reify-s
  (lambda (v s)
    (let ((v (walk v s)))
      (cond
       ((var? v) (ext-s v (reify-name (length s)) s))
       ((pair? v) (reify-s (cdr v) (reify-s (car v) s)))
       (else s)))))

(define reify-name
  (lambda (n)
    (string->symbol
     (string-append ":" (number->string n)))))

(define empty-s '())

(define reify
  (lambda (v)
    (walk* v (reify-s v empty-s))))

(define-syntax trunk
  ;; [_ -> [_]]
  (syntax-rules ()
    [(_ e) (lambda () e)]))

;; measure-zero
(define-syntax mzero
  ;; _ 0 stream
  (syntax-rules ()
    [(_) #f]))

(define-syntax unit
  ;; (_ -> _ 1 stream)
  (syntax-rules ()
    [(_ a) a]))

(define-syntax choice
  ;; (_ [_ _ stream] -> _ more stream)
  (syntax-rules ()
    [(_ a f) (cons a f)]))

(define-syntax case-inf
  (syntax-rules ()
    [(_ e
        [() on-zero]
        [(a^) on-one]
        [(a f) on-choice])
     (let ([a-inf e])
       (cond [(not a-inf)
              on-zero]
             [(not (and (pair? a-inf)
                        (procedure? (cdr a-inf))))
              (let ([a^ a-inf])
                on-one)]
             [else
              (let ([a (car a-inf)]
                    [f (cdr a-inf)])
                on-choice)]))]))

;; stream-concatenate
(define mplus
  ;; ([_ _ stream] [_ _ stream] -> [_ _ stream])
  (lambda (a-inf f)
    (case-inf a-inf
      [() (f)]
      [(a) (choice a f)]
      [(a f0) (choice a (trunk (mplus (f0) f)))])))

;; stream-interleave
(define mplusi
  ;; ([_ _ stream] [_ _ stream] -> [_ _ stream])
  (lambda (a-inf f)
    (case-inf a-inf
      [() (f)]
      [(a) (choice a f)]
      [(a f0) (choice a (trunk
                          ;; swap
                          ;; thus the next value
                          ;; would be taken form the second stream
                          (mplusi (f) f0)))])))

;; stream-map
(define bind
  ;; ([t1 _ stream] (t1 -> t2) -> [t2 _ stream])
  (lambda (a-inf g)
    (case-inf a-inf
      [() (mzero)]
      [(a) (g a)]
      [(a f) (mplus (g a) (trunk (bind (f) g)))])))

(define bindi
  ;; ([t1 _ stream] (t1 -> t2) -> [t2 _ stream])
  (lambda (a-inf g)
    (case-inf a-inf
      [() (mzero)]
      [(a) (g a)]
      [(a f) (mplusi (g a) (trunk (bindi (f) g)))])))

(define fail
  ;; 0 goal
  ;; (substitution -> substitution 0 stream)
  (lambda (s) (mzero)))

(define succeed
  ;; 1 goal
  ;; (substitution -> substitution 1 stream)
  (lambda (s) (unit s)))

(define ==
  ;; (term term -> (0 or 1) goal)
  ;; (term term -> (substitution -> substitution (0 or 1) stream))
  (lambda (v w)
    (lambda (s)
      (cond [(unify v w s) => succeed]
            [else (fail s)]))))

(define ==-check
  ;; (term term -> (0 or 1) goal)
  ;; (term term -> (substitution -> substitution (0 or 1) stream))
  (lambda (v w)
    (lambda (s)
      (cond [(unify-check v w s) => succeed]
            [else (fail s)]))))

(define-syntax all
  (syntax-rules ()
    [(_) succeed]
    [(_ g)
     (lambda (s)
       (g s))]
    [(_ g^ g ...)
     (lambda (s)
       (bind (g^ s) (all g ...)))]))

(define-syntax alli
  (syntax-rules ()
    [(_) succeed]
    [(_ g)
     (lambda (s)
       (g s))]
    [(_ g^ g ...)
     (lambda (s)
       (bindi (g^ s) (alli g ...)))]))

(define-syntax fresh
  (syntax-rules ()
    [(_ (x ...)
        g ...)
     (lambda (s)
       (let ([x (var 'x)]
             ...)
         ((all g ...) s)))]))

(define-syntax anye
  (syntax-rules ()
    [(_ g1 g2)
     ;; substitution -> substitution _ stream
     (lambda (s)
       (mplus (g1 s)
              (trunk (g2 s))))]))

(define-syntax conde
  (syntax-rules (else)
    [(_) fail]
    [(_ (else g0 g ...)) (all g0 g ...)]
    [(_ (g0 g ...) c ...)
     (anye (all g0 g ...) (conde c ...))]))

(define-syntax anyi
  (syntax-rules ()
    [(_ g1 g2)
     ;; substitution -> substitution _ stream
     (lambda (s)
       (mplusi (g1 s)
               (trunk (g2 s))))]))

(define-syntax condi
  (syntax-rules (else)
    ((_) fail)
    ((_ (else g0 g ...)) (all g0 g ...))
    ((_ (g0 g ...) c ...)
     (anyi (all g0 g ...) (condi c ...)))))

(define-syntax ifa
  (syntax-rules ()
    [(_ g0 g1 g2)
     ;; substitution -> substitution _ stream
     (lambda (s)
       (let ([s-inf (g0 s)]
             [g^ g1])
         (case-inf s-inf
           [() (g2 s)]
           [(s) (g^ s)]
           [(s f) (bind s-inf g^)])))]))

(define-syntax conda
  (syntax-rules (else)
    ((_) fail)
    ((_ (else g0 g ...)) (all g0 g ...))
    ((_ (g0 g ...) c ...)
     (ifa g0 (all g ...) (conda c ...)))))

(define-syntax ifu
  (syntax-rules ()
    ((_ g0 g1 g2)
     ;; substitution -> substitution _ stream
     (lambda (s)
       (let ([s-inf (g0 s)]
             [g^ g1])
         (case-inf s-inf
           [() (g2 s)]
           [(s) (g^ s)]
           [(s f) (g^ s)]))))))

(define-syntax condu
  (syntax-rules (else)
    [(_) fail]
    [(_ (else g0 g ...)) (all g0 g ...)]
    [(_ (g0 g ...) c ...)
     (ifu g0 (all g ...) (condu c ...))]))

;; run converts a stream of substitutions
;; to a list of values using map-inf

(define map-inf
  (lambda (n p a-inf)
    (case-inf a-inf
      [() '()]
      [(a)
       (cons (p a) '())]
      [(a f)
       (cons (p a)
             (cond [(not n) (map-inf n p (f))]
                   [(> n 1) (map-inf (- n 1) p (f))]
                   [else '()]))])))

(define-syntax run
  (syntax-rules ()
    [(_ n^ (x) g ...)
     (let ((n n^) (x (var 'x)))
       (if (or (not n) (> n 0))
         (map-inf n
                  (lambda (s) (reify (walk* x s)))
                  ((all g ...) empty-s))
         '()))]))

(define-syntax run*
  (syntax-rules ()
    [(_ (x) g ...)
     (run #f (x) g ...)]))

(define-syntax lambda-limited
  (syntax-rules ()
    ((_ n formals g)
     (let ([x (var 'x)])
       (lambda formals
         (ll n x g))))))

(define ll
  (lambda (n x g)
    ;; substitution -> substitution _ stream
    (lambda (s)
      (let ([v (walk x s)])
        (cond [(var? v) (g (ext-s x 1 s))]
              [(< v n) (g (ext-s x (+ v 1) s))]
              [else (fail s)])))))

(define-syntax project
  (syntax-rules ()
    [(_ (x ...) g ...)
     ;; substitution -> substitution _ stream
     (lambda (s)
       (let ((x (walk* x s)) ...)
         ((all g ...) s)))]))

(define-syntax ando+
  (syntax-rules ()
    [(_ name-list . body)
     (fresh name-list . body)]))

(define-syntax oro+
  (syntax-rules ()
    [(_ name-list
        a
        ...)
     (fresh name-list
       (conde
         [a]
         ...))]))

(define-syntax ando
  (syntax-rules ()
    [(_ . body)
     (ando+ () . body)]))

(define-syntax oro
  (syntax-rules ()
    [(_ . body)
     (oro+ () . body)]))

(define conso
  (lambda (a d p)
    (== (cons a d) p)))

(define caro
  (lambda (p a)
    (fresh (d)
      (== (cons a d) p))))

(define cdro
  (lambda (p d)
    (fresh (a)
      (== (cons a d) p))))

(define pairo
  (lambda (p)
    (fresh (a d)
      (== (cons a d) p))))

(define nullo
  (lambda (x)
    (== x '())))

(define listo
  (lambda (l)
    (oro
      (nullo l)
      (ando+ (d)
        (pairo l)
        (cdro l d)
        (listo d)))))

(define appendo
  (lambda (l s out)
    (oro (ando
           (nullo l)
           (== out s))
         (ando+ (a d rec)
           (conso a d l)
           (appendo d s rec)
           (conso a rec out)))))

(define unwarpo
  (lambda (x out)
    (oro
      (== x out)
      (ando+ (a)
        (pairo x)
        (caro x a)
        (unwarpo a out)))))

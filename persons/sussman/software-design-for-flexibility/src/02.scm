(define (compose f g)
  (lambda args
    (f (apply g args))))

(begin
  (display
   ((compose (lambda (x) (list 'foo x))
             (lambda (x) (list 'bar x)))
    'z))
  (newline))


(define ((iterate n) f)
  (if (= n 0)
    identity
    (compose f ((iterate (- n 1)) f))))

(define (identity x) x)

(define (square x) (* x x))

(begin
  (display
   (((iterate 4) square) 2))
  (newline))


(define (parallel-combine h f g)
  (define (the-combination . args)
    (h (apply f args) (apply g args)))
  the-combination)

(begin
  (display
   ((parallel-combine
     list
     (lambda (x y z) (list 'foo x y z))
     (lambda (x y z) (list 'bar x y z)))
    'a 'b 'c))
  (newline))

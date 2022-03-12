(define (compose f g)
  (lambda args
    (f (apply g args))))

(display
 ((compose (lambda (x) (list 'foo x))
           (lambda (x) (list 'bar x)))
  'z))

(define ((iterate n) f)
  (if (= n 0)
    identity
    (compose f ((iterate (- n 1)) f))))

(define (identity x) x)

(define (square x) (* x x))

(((iterate 10) square) 2)

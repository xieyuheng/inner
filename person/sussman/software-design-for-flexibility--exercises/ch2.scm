(define (compose f g)
  (lambda args
    (f (apply g args))))

(display compose)

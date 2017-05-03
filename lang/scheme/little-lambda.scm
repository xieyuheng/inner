(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x)))))

(define add1
  (lambda (x)
    (+ 1 x)))

(define sub1
  (lambda (x)
    (- x 1)))

(define first car)

(define second (lambda (x) (car (cdr x))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 (quote ())))))


(define new-entry build)

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help
     name
     (first entry)
     (second entry)
     entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond [(null? names) (entry-f name)]
          [(eq? (car names) name)
           (car values)]
          [else (lookup-in-entry-help
                 name
                 (cdr names)
                 (cdr values)
                 entry-f)])))

(define extend-table cons)

(define lookup-in-table
  (lambda (name table table-f)
    (cond [(null? table)
           (table-f name)]
          [else
           (lookup-in-entry
            name
            (car table)
            (lambda (name)
              (lookup-in-table name (cdr table) table-f)))])))

(define value
  (lambda (e)
    (meaning e (quote ()))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define expression-to-action
  (lambda (e)
    (cond
     [(atom? e)
      (atom-to-action e)]
     [else
      (list-to-action e)])))

(define atom-to-action
  (lambda (e)
    (cond
     [(number? e)             *const]
     [(eq? e #t)              *const]
     [(eq? e #f)              *const]
     [(eq? e (quote cons))    *const]
     [(eq? e (quote car))     *const]
     [(eq? e (quote cdr))     *const]
     [(eq? e (quote null?))   *const]
     [(eq? e (quote eq?))     *const]
     [(eq? e (quote atom?))   *const]
     [(eq? e (quote zero?))   *const]
     [(eq? e (quote add1))    *const]
     [(eq? e (quote sub1))    *const]
     [(eq? e (quote number?)) *const]
     [else                    *identifer])))

(define list-to-action
  (lambda (e)
    (cond [(null? e) *null]
          [(atom? (car e))
           (cond [(eq? (car e) (quote quote))  *quote]
                 [(eq? (car e) (quote lambda)) *lambda]
                 [(eq? (car e) (quote cond))   *cond]
                 [else                         *application])]
          [else *application])))

;; *const

(define *const
  (lambda (e table)
    (cond [(number? e) e]
          [(eq? e #t) #t]
          [(eq? e #f) #f]
          [else (build (quote primitive) e)])))


;; *identifer

(define *identifer
  (lambda (e table)
    (lookup-in-table e table notfound)))

(define notfound
  (lambda (name)
    "error: at least one name is unbound"))


;; *null

(define *null
  (lambda (e table)
    (quote ())))


;; *quote

(define text-of second)

(define *quote
  (lambda (e table)
    (text-of e)))


;; *lambda

(define *lambda
  (lambda (e table)
    (list (quote non-primitive)
          (cons table (cdr e)))))

(define table-of
  (lambda (non-primitive)
    (car non-primitive)))

(define formals-of
  (lambda (non-primitive)
    (cadr non-primitive)))

(define body-of
  (lambda (non-primitive)
    (caddr non-primitive)))


;; *cond

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)

(define evcon
  (lambda (lines table)
    (cond [(else? (question-of (car lines)))
           (meaning (answer-of (car lines)) table)]
          [(meaning (question-of (car lines)) table)
           (meaning (answer-of (car lines)) table)]
          [else
           (evcon (cdr lines) table)])))

(define question-of
  (lambda (x)
    (car x)))

(define answer-of
  (lambda (x)
    (cadr x)))

(define else?
  (lambda (x)
    (cond ((atom? x)
           (eq? x (quote else)))
          (else
           #f))))


;; *application

(define *application
  (lambda (e table)
    (myapply
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))

(define evlis
  (lambda (args table)
    (cond [(null? args) (quote ())]
          [else (cons (meaning (car args) table)
                      (evlis (cdr args) table))])))

(define function-of car)

(define arguments-of cdr)

(define myapply
  (lambda (fun vals)
    (cond ((primitive? fun)
           (apply-primitive (second fun) vals))
          ((non-primitive? fun)
           (apply-closure (second fun) vals)))))

(define primitive?
  (lambda (l)
    (eq? (car l) (quote primitive))))

(define non-primitive?
  (lambda (l)
    (eq? (car l) (quote non-primitive))))

(define apply-primitive
  (lambda (name vals)
    (cond
     ((eq? name 'cons)
      (cons (first vals) (second vals)))
     ((eq? name 'car)
      (car (first vals)))
     ((eq? name 'cdr)
      (cdr (first vals)))
     ((eq? name 'null?)
      (null? (first vals)))
     ((eq? name 'eq?)
      (eq? (first vals) (second vals)))
     ((eq? name 'atom?)
      (:atom? (first vals)))
     ((eq? name 'zero?)
      (zero? (first vals)))
     ((eq? name 'add1)
      (add1 (first vals)))
     ((eq? name 'sub1)
      (sub1 (first vals)))
     ((eq? name 'number?)
      (number? (first vals))))))

(define :atom?
  (lambda (x)
    (cond ((atom? x)
           #t)
          ((null? x)
           #f)
          ((eq? (car x) 'primitive)
           #t)
          ((eq? (car x) 'non-primitive)
           #t)
          (else
           #f))))

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table (new-entry (formals-of closure) vals)
                           (table-of closure)))))


;;;; test

;; 對*identifer的測試可以看作對報錯情況的測試
(value 'not-const)

;; *const
;; 數字與邏輯值
(value 1)
;; 下面兩個用到*application
(value '(add1 1))
(value '(sub1 1))
(value #t)
(value #f)

;; 基本函數primitive
(value 'add1)
(value 'car)
(value 'atom?)

;; *null
(value '())

;; *quote
(value '(quote a-quoted-string))
(value '(quote (a-quoted-string)))
(value '(quote (lambda (x) (add1 (add1 x)))))

;; *lambda 非基本函數non-primitive
(value '(lambda (x) (add1 (add1 x))))

;; *cond
;; 用到*application
(value '(cond ((eq? 1 2) 123) (else 321)))
(value '(cond ((eq? 1 'kkk) 123) (else 321)))
(value '(cond ((eq? cons car) 123) (else 321)))

;; *application
(value '(cons 1 '()))
(value '((lambda (x) (cons 'drink (cons x '())))
         'milk))
(value '((lambda (y) (cond
                      ((eq? y 'thirst)
                       ((lambda (x) (cons 'drink (cons x '())))
                        'water))
                      ((eq? y 'not-thirst)
                       ((lambda (x) (cons 'do (cons 'not (cons 'drink (cons x '())))))
                        'water))
                      (else
                       'what-ever)))
         'thirst))
(value '((lambda (y) (cond
                      ((eq? y 'thirst)
                       ((lambda (x) (cons 'drink (cons x '())))
                        'water))
                      ((eq? y 'not-thirst)
                       ((lambda (x) (cons 'do (cons 'not (cons 'drink (cons x '())))))
                        'water))
                      (else
                       'what-ever)))
         'not-thirst))
;; 這裏用到的是有else保護的cond
(value '((lambda (y) (cond
                      ((eq? y 'thirst)
                       ((lambda (x) (cons 'drink (cons x '())))
                        'water))
                      ((eq? y 'not-thirst)
                       ((lambda (x) (cons 'do (cons 'not (cons 'drink (cons x '())))))
                        'water))
                      (else
                       'what-ever)))
         'do-not-tell-you))

;; 而如果不用else就會報錯
;; 報出的是元解釋器中的(car '())錯誤
;; (value '((lambda (y) (cond
;;                     ((eq? y 'thirst)
;;                      ((lambda (x) (cons 'drink (cons x '())))
;;                       'water))
;;                     ((eq? y 'not-thirst)
;;                      ((lambda (x) (cons 'do (cons 'not (cons 'drink (cons x '())))))
;;                       'water))))
;;        'do-not-tell-you))

;; 測試結果
;; "error: at least one name is unbound"
;; > 1
;; > 2
;; > 0
;; > #t
;; > #f
;; > (primitive add1)
;; > (primitive car)
;; > (primitive atom?)
;; > ()
;; > a-quoted-string
;; > (a-quoted-string)
;; > (lambda (x) (add1 (add1 x)))
;; > (non-primitive (() (x) (add1 (add1 x))))
;; > 321
;; > 321
;; > 321
;; > (1)
;; > (drink milk)
;; > (drink water)
;; > (do not drink water)
;; > what-ever

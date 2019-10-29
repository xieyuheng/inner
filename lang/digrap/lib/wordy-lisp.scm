(define string<-symbol symbol->string)
(define list<-string string->list)

(define not-proper-list?
  (lambda (x)
    (and (pair? x)
         (not (list? x)))))
(define key?
  (lambda (x)
    (and (symbol? x)
         (eq? #\: (car (list<-string
                        (string<-symbol x)))))))
;; test:
;; (key? 1)
;; (key? ':kkk)
;; (key? '::kkk)
;; (key? '卡夫卡)


(define wordy-list?
  (lambda (x)
    (and (list? x)
         (or (null? x);; can be '() 因为要作为递归函数的基本步骤
             (key? (car x))))))
;; (wordy-list? '())
;; (wordy-list? '(:k1 1 :k2 2 2 2 :k3 3 (3) () :k4 . 1))



(define field?
  (lambda (x)
    (and (list x)
         (not (null? x))
         (key? (car x))
         (not (wordy-list? (cdr x))))))
;; 就接口而言 curry总是可以增加使用上的灵活性
;; 并且简化对使用方式的理解: 因为所有的作用都是一元的了

;; (take n) => <taker>
;; (<taker> <field>) => <val>
;;   例如 (take n) will take the key of <field>
(define take
  (lambda (n)
    (lambda (field)
      (letrec ([R (lambda (n lis)
                    (cond [(= n 0)
                           (car lis)]
                          [else
                           (R (sub1 n) (cdr lis))]))])
        (if (>= n (length field))
          (error 'take
            "taker too far!" n (sub1 (length field)) field)
          (R n field))))))
;; ((take 1) '(:kkk))
;; ((take 3) '(:kkk 1 2 3))
;; ((take 4) '(:kkk 1 2 3))


;; (find <key>) => <finder>
;; (<finder> <wordy-list>) => <field> | #f
;;    上面当失败的时候是否不应该简单地返回#f
;;    而去返回更多的信息 比如找什么key的时候失败了
;;    其实用#f也行 因为返回值正常的时候返回的都是列表
;;    但是其他的函数的错误处理可能就不能这么草率了
;; 注意:
;;   所返回的是一个新构建的列表
;;   但是列表里的元素还是老元素
;;   因此对于finder所返回的值要小心地使用副作用
;; 又注意:
;;   所返回的新列表会丧失``在列表内引用列表头''的性质
;;   所返回的:
;; (:0-dimension-geometry-object-list
;;      #2=[:v vertex-2 :address #2#
;;          :can (:value 222)
;;          :abut-edge-list
;;             (:e edge:2-->3 :address #4#)
;;             (:e edge:3-->2 :address #6#)])
;; 对其中#2#的引用不会丧失``在列表内引用列表头''的性质
;; 但是如果是(:kkk 1 2 . #0=(:tree 7 8 9 #0#) 4 5 6)
;; 那么
;; ((find ':tree)
;;  '(:kkk 1 2 . #0=(:tree 7 8 9 #0#)))
;; =>
;; (:tree 7 8 9 #0=(:tree 7 8 9 #0#))
;; 而不是#0=(:tree 7 8 9 #0#)
;; 也就是说返回值丧失了``在列表内引用列表头''的性质
;; 但是从某种语义上来说
;; (:kkk 1 2 . #0=(:tree 7 8 9 #0#) 4 5 6)中的#0#并不是对列表头的引用不是吗?
;; 看你从什么角度去理解了
;; 但是一定要小心而仔细地分析find这类函数的性质
(define find
  (lambda (key)
    (lambda (wordy-list)
      (letrec ([find-the-key
                (lambda (wordy-list)
                  (cond [(null? wordy-list)
                         #f]
                        [(eq? key (car wordy-list))
                         (cons (car wordy-list)
                               (find-2nd-key (cdr wordy-list)))]
                        [else
                         (find-the-key (cdr wordy-list))]))]
               [find-2nd-key
                (lambda (wordy-list)
                  (cond [(null? wordy-list)
                         wordy-list]
                        [(atom? wordy-list)
                         wordy-list]
                        [(not (key? (car wordy-list)))
                         (cons (car wordy-list)
                               (find-2nd-key (cdr wordy-list)))]
                        [(key? (car wordy-list))
                         '()]
                        ))])
        (cond [(not (key? key))
               (error 'find "inupt must be a key" key)]
              [(null? wordy-list)
               #f]
              [else
               (find-the-key wordy-list)])
        ))))
;; test:

;; ((find ':k0) '(:k1 1 :k2 2 2 2 :k3 3 (3) () :k4))
;; ((find ':k1) '(:k1 1 :k2 2 2 2 :k3 3 (3) () :k4))
;; ((find ':k2) '(:k1 1 :k2 2 2 2 :k3 3 (3) () :k4))
;; ((find ':k4) '(:k1 1 :k2 2 2 2 :k3 3 (3) () :k4))

;; 尽管下面的测试看似正确
;; 但是非proper-list不被看作是wordy-list
;; wordy-list?这个谓词会帮助判断
;; 所以需要一个例外处理来处理下面的东西
;; 否则这种形式被(有意或无意地)滥用后 将会带来麻烦
;; ((find ':k4) '(:k1 1 :k2 2 2 2 :k3 3 (3) () :k4 . 1))

;; ((find ':k3) '(:k1 1
;;              :k2 2 2 2
;;              :k3 3 (3) ()
;;              :k4))
;; ((find ':can) '[:v :can <-- <-- λ])
;; ((find ':can) '[:v :can (:v-type black :kkk kkk)])



;; ((find ':type)
;; '(:type 

;;   :0-dimension-geometry-object-list
;;      #2=[:v vertex-2 :address #2#
;;          :can (:value 222)
;;          :abut-edge-list
;;             (:e edge:2-->3 :address #4#)
;;             (:e edge:3-->2 :address #6#)]

;;      #3=[:v vertex-3 :address #3#
;;          :can (:value 333)
;;          :abut-edge-list
;;             (:e edge:2-->3 :address #4#)
;;             (:e edge:3-->2 :address #6#)]

;;   :1-dimension-geometry-object-list
;;      #4=[:e edge:2-->3 :address #4#
;;          :can (:edge-type black-arrow)
;;          :abut-vertex-list
;;             (:v vertex-2 :address #2#)
;;             (:v vertex-3 :address #3#)]

;;      #6=[:e edge:3-->2 :address #6#
;;          :can (:edge-type black-arrow)
;;          :abut-vertex-list
;;             (:v vertex-3 :address #3#)
;;             (:v vertex-2 :address #2#)]
;;      ))




;; (let ([first-edge-fo-first-vertex-finder
;;        (lambda (a-graph)
;;          ((find ':e)
;;           ((take 1)
;;            ((find ':abut-edge-list)
;;             ((take 1)
;;              ((find ':0-dimension-geometry-object-list) a-graph))))))])
;;   (first-edge-fo-first-vertex-finder
;;    '(:type 

;;      :0-dimension-geometry-object-list
;;         #2=[:v vertex-2 :address #2#
;;             :can (:value 222)
;;             :abut-edge-list
;;                (:e edge:2-->3 :address #4#)
;;                (:e edge:3-->2 :address #6#)]

;;         #3=[:v vertex-3 :address #3#
;;             :can (:value 333)
;;             :abut-edge-list
;;                (:e edge:2-->3 :address #4#)
;;                (:e edge:3-->2 :address #6#)]

;;      :1-dimension-geometry-object-list
;;         #4=[:e edge:2-->3 :address #4#
;;             :can (:edge-type black-arrow)
;;             :abut-vertex-list
;;                (:v vertex-2 :address #2#)
;;                (:v vertex-3 :address #3#)]

;;         #6=[:e edge:3-->2 :address #6#
;;             :can (:edge-type black-arrow)
;;             :abut-vertex-list
;;                (:v vertex-3 :address #3#)
;;                (:v vertex-2 :address #2#)]
;;         )
;;    ))



;; (let ([can-of-first-edge-fo-first-vertex-finder
;;        (lambda (a-graph)
;;          ((find ':can)
;;           ((take 1)
;;            ((find ':address)
;;             ((take 1)
;;              ((find ':abut-edge-list)
;;               ((take 1)
;;                ((find ':0-dimension-geometry-object-list) a-graph))))))))])
;;   (can-of-first-edge-fo-first-vertex-finder
;;    '(:type 

;;      :0-dimension-geometry-object-list
;;         #2=[:v vertex-2 :address #2#
;;             :can (:value 222)
;;             :abut-edge-list
;;                (:e edge:2-->3 :address #4#)
;;                (:e edge:3-->2 :address #6#)]

;;         #3=[:v vertex-3 :address #3#
;;             :can (:value 333)
;;             :abut-edge-list
;;                (:e edge:2-->3 :address #4#)
;;                (:e edge:3-->2 :address #6#)]

;;      :1-dimension-geometry-object-list
;;         #4=[:e edge:2-->3 :address #4#
;;             :can (:edge-type black-arrow)
;;             :abut-vertex-list
;;                (:v vertex-2 :address #2#)
;;                (:v vertex-3 :address #3#)]

;;         #6=[:e edge:3-->2 :address #6#
;;             :can (:edge-type black-arrow)
;;             :abut-vertex-list
;;                (:v vertex-3 :address #3#)
;;                (:v vertex-2 :address #2#)]
;;         )

;;    ))


;; (address-find <key>) => <address-finder>
;; (<address-finder> <wordy-list>) => <wordy-list>
;; 不构建新列表而直接返回找到的列表的地址
;; 有点像又两个参数的cdr
;; 第一个参数是<field-name> (即<key>)
;; 第二个参数是<wordy-list>
(define address-find
  (lambda (key)
    (lambda (wordy-list)
      (letrec ([find-the-key
                (lambda (wordy-list)
                  (cond [(null? wordy-list)
                         #f]
                        [(eq? key (car wordy-list))
                         wordy-list]
                        [else
                         (find-the-key (cdr wordy-list))]))])
        (cond [(not (key? key))
               (error 'find "inupt must be a key" key)]
              [(null? wordy-list)
               #f]
              [(eq? key (car wordy-list))
               wordy-list]
              [else
               (find-the-key wordy-list)])
        ))))
;; test:
;; ((address-find ':k0) '(:k1 1 :k2 2 2 2 :k3 3 (3) () :k4))
;; ((address-find ':k1) '(:k1 1 :k2 2 2 2 :k3 3 (3) () :k4))
;; ((address-find ':k2) '(:k1 1 :k2 2 2 2 :k3 3 (3) () :k4))
;; ((address-find ':k3) '(:k1 1 :k2 2 2 2 :k3 3 (3) () :k4))
;; ((address-find ':k4) '(:k1 1 :k2 2 2 2 :k3 3 (3) () :k4))




(define append-two!
  ;; SIDE-EFFECT on wlist-1, 所以append-two!的第一个参数不能是'()
  ;; RETURN-VAL wlist-1
  (lambda (wlist-1 wlist-2)
    (letrec ([R! (lambda (wlist-1)
                   (cond [(null? (cdr wlist-1)) ;; (= 1 (length wlist-1))
                          (set-cdr! wlist-1 wlist-2)]
                         [else
                          (R! (cdr wlist-1))]))])
      (cond [(null? wlist-1)
             (error 'append-two!
               "append-two!'s 1th-arg can not be '()" wlist-1 wlist-2)]
            [(not-proper-list? wlist-1)
             (error 'append-two!
               "append-two!'s 1th-arg can not be a not proper-list"
               wlist-1 wlist-2)]
            ;; [(not (list? wlist-2))
            ;;  (error 'append-two!
            ;;    "append-two!'s 2th-arg must be a list" wlist-1 wlist-2)]
            ;; 上面的一句对类型的控制比较严格一点
            [(and (not (pair? wlist-2))
                  (not (null? wlist-2)))
             (error 'append-two!
               "append-two!'s 2th-arg must be a pair or '()" wlist-1 wlist-2)]
            [else
             ;; SIDE-EFFECT
             (R! wlist-1)
             ;; RETURN-VAL
             wlist-1]))))
;; test:
;; (append-two! '(1 2 3) '(kkk))
;; (append-two! '() '(kkk))
;; (append-two! '(1 2 . 3) '(kkk))
;; (let ([kkk '(1 2 3)])
;;   (append-two! kkk '(kkk))
;;   (append-two! kkk '(kkk))
;;   (append-two! kkk '(kkk))
;;   kkk)
;; (let ([kkk '(1 2 3)])
;;   (append kkk '(kkk))
;;   (append kkk '(kkk))
;;   (append kkk '(kkk))
;;   kkk)

(define-syntax append!
  (syntax-rules ()
    [(_)
     (error 'append!
       "append! is a syntax with at least 2 args, not 0 arg !")]
    [(_ anthing)
     (error 'append!
       "append! is a syntax with at least 2 args, not 1 arg !" anthing)]
    [(_ wlist-1 wlist-2)
     (append-two! wlist-1 wlist-2)]
    [(_ wlist-1 wlist-2 wlist-3 ...)
     (append!
      (append-two! wlist-1 wlist-2) wlist-3 ...)]
    ))
;; test:
;; (append! '(1 2 3) '(1 2 3) '(kkk))
;; (append! '(kkk) '() '(kkk))
;; (append! '(0 0 0) '(kkk) '(1 2 . 3))
;; (append! '(0 0 0) '(1 2 . 3) '(kkk))
;; (let ([kkk '(1 2 3)])
;;   (append! kkk '(kkk))
;;   (append! kkk '(kkk))
;;   (append! kkk '(kkk))
;;   kkk)
;; (let ([kkk '(1 2 3)])
;;   (append kkk '(kkk))
;;   kkk)


;; 一个同构变换:
;; 主要的区别是
;; alist中递增一下就能找到下一个field
;;   而wlist中可能需要很多对key?判断才能找到下一个field
;; wlist的样子看起来很简单括号很少并且更容易理解
;;   而alist看起来很复杂
;; alist在视觉上的的劣势很容易用一个语法解析器来弥补
;; wlist理解起来还是更简单

;; 为了获得alist的优势 并不必作同构变换
;; 只要跑一遍wlist 然后增加一个key.address-alist就行了
;; 真正的同构变换会破坏原来的结构化数据的结构
;; 即 第一个field的尾部被一个'()截断了
;; 但是更重要的是注意这种截断并不影响wlist中的loop
;; 也就是说不会影响wlist中的某些位置对其他位置的地址的记录

;; 注意只有当使用同构所节省的时间超过
;; 同构变换所浪费的时间
;; (define alist<-wlist
;;   (lambda (wlist)
;;     ()))

;; (define wlist<-alist
;;   (lambda (alist)
;;     ()))


;; 用副作用删除和增加field
;; (delete! <key>) => <deleter!>
;; (<deleter!> <wordy-list>) => {WITH-SIDE-EFFECT} <wordy-list> | #f
;; (define delete!
;;   (lambda (key)
;;     (lambda (wordy-list)
;;       需要定义find-next-field
;;       )))

;; (substitute! <field>) => <substituent!>
;; ((<substituent!> <key>) )
;; (define substitute!
;;   (lambda (key)
;;     (lambda (wordy-list)
;;  
;;       )))


;; 用副作用删除和增加field中的值
;; 用副作用更改field的名字




(define insert-val-to-the-2nd-position-of-a-list!
  (lambda (val lis)
    (if (null? lis)
      (error 'insert-val-to-the-2nd-position-of-a-list!
        "input list can not be '() !")
      (set-cdr! lis (append!
                      (list val)
                      (cdr lis))))))


(define insert-a-val-to-a-field-of-a-wlist!
  (lambda (val field-name wlist)
    (insert-val-to-the-2nd-position-of-a-list!
     val ((address-find field-name) wlist))))
;; test:
;; (define kkk '(:kkk))
;; (insert-a-val-to-a-field-of-a-wlist! 1 ':kkk kkk)

;; 发现如果可以自由的改变参数的顺序 那将很有利于可读性
;; 在wordy-lisp中就可以做到这一点
;; 我可以直接用macro把wordy-lisp嵌入到scheme中吗???
;; 如果能自动实现动态的curry 就更好了 !!!
;; 甚至可以自动变换求值顺序形成不同的curry
(define to-a-field-of-a-wlist--let-us-insert-a-val!
  (lambda (field-name wlist val)
    (insert-val-to-the-2nd-position-of-a-list!
     val ((address-find field-name) wlist))))

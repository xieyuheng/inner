;; (define syntax-example-of-add-vertex
;;   '(:type vertex-representation-for-add-vertex

;;     :v vertex-1
;;     :can (:value 111)

;;     :abut-edge-list-representation-for-add-vertex
;;        (:e edge:1-->2
;;         :can (:edge-type black-arrow)
;;         :abut-vertex-of-this-edge vertex-2)

;;        (:e edge:1-->3
;;         :can (:edge-type black-arrow)
;;         :abut-vertex-of-this-edge vertex-3)

;;        (:e edge:1-->2
;;         :can (:edge-type red-arrow)
;;         :abut-vertex-of-this-edge vertex-2)

;;        (:e edge:1-->3
;;         :can (:edge-type red-arrow)
;;         :abut-vertex-of-this-edge vertex-3)

;;        ))

;; (define example-graph
;;   '(:type 


;;     :0-dimension-geometry-object-list

;;     #2=[:v vertex-2 :address #2#
;;         :can (:value 222)
;;         :abut-edge-list
;;            (:e edge:2-->3 :address #4#)
;;            (:e edge:3-->2 :address #6#)]

;;     #3=[:v vertex-3 :address #3#
;;         :can (:value 333)
;;         :abut-edge-list
;;            (:e edge:2-->3 :address #4#)
;;            (:e edge:3-->2 :address #6#)]


;;     :1-dimension-geometry-object-list

;;     #4=[:e edge:2-->3 :address #4#
;;         :can (:edge-type black-arrow)
;;         :abut-vertex-list
;;            (:v vertex-2 :address #2#)
;;            (:v vertex-3 :address #3#)]

;;     #6=[:e edge:3-->2 :address #6#
;;         :can (:edge-type black-arrow)
;;         :abut-vertex-list
;;            (:v vertex-3 :address #3#)
;;            (:v vertex-2 :address #2#)]


;;     ))


(define find-vertex-address-in-vertex-list
  (lambda (name-of-vertex vertex-list-of-the-graph)
    (cond [(null? vertex-list-of-the-graph)
           (error 'find-vertex-address
             "can not find a vertex in graph" name-of-vertex)]
          [(eq? ((take 1) ((find ':v) (car vertex-list-of-the-graph)))
                name-of-vertex)
           ((take 1) ((find ':address) (car vertex-list-of-the-graph)))]
          [else
           (find-vertex-address-in-vertex-list
            name-of-vertex
            (cdr vertex-list-of-the-graph))])))

(define add-vertex
  (lambda (vertex-to-be-processed the-graph)
    (add-vertex! vertex-to-be-processed (list-copy the-graph))))


;; add-vertex!它不是一个递归函数
;; 不是递归函数就意味着我可以最明显的平铺直叙的方式把程序先写出来
;; 也许正是在这种平铺直叙中最容易把程序先写对
;; 也许正是在这种平铺直叙中最容易找到程序中重复的模式
;;     把这些重复的模式抽象出来就简化了代码

;; add-vertex!完全是一个副作用
;; 返回的是副作用之后的输入的图的地址

;; 需要控制求值顺序
;; 作出这个新的点
;; [1]把新的点加进0-dimension-geometry-object-list里
;; 在制作这个新的点的同时需要:
;; [2]用递归的副作用扩充1-dimension-geometry-object-list
;; [3]          同时调整0-dimension-geometry-object-list中的其他点

(define add-vertex!
  (lambda (vertex-to-be-processed the-graph)
    (let ([list-of-abut-edge-to-be-processed
           (cdr ((find ':abut-edge-list-representation-for-add-vertex)
                 vertex-to-be-processed))])

      ;; SIDE-EFFECT:[1]============================================
      (let* ([head-of-processed-vertex ((find ':v) vertex-to-be-processed)]
             [address-of-processed-vertex  head-of-processed-vertex])
        (to-a-field-of-a-wlist--let-us-insert-a-val!
          ':0-dimension-geometry-object-list the-graph
          (append!
            head-of-processed-vertex
            `(:address ,address-of-processed-vertex)
            ((find ':can) vertex-to-be-processed)
            `(:abut-edge-list
              ,(map
                ;; SIDE-EFFECT-by-map:[2]==========================================
                (lambda (abut-edge-to-be-processed)
                  (let* ([head-of-new-maked-edge ((find ':e) abut-edge-to-be-processed)]
                         [address-of-new-maked-edge head-of-new-maked-edge])
                    ;; let*在上面的好处是 这样就又更大的scope 返回值的之后也可能引用到这些绑定
                    (to-a-field-of-a-wlist--let-us-insert-a-val!
                      ':1-dimension-geometry-object-list the-graph
                      (append!
                        head-of-new-maked-edge
                        `(:address ,address-of-new-maked-edge)
                        ((find ':can) abut-edge-to-be-processed)
                        `(:abut-vertex-list
                          (:v ,((take 1) ((find ':v) address-of-processed-vertex))
                              :address ,address-of-processed-vertex)
                          (:v ,((take 1) ((find ':abut-vertex-of-this-edge) abut-edge-to-be-processed))
                              :address
                              ;; SIDE-EFFECT:[3]==========================================
                              ,(let* ([name-of-this-vertex
                                       ((take 1)
                                        ((find ':abut-vertex-of-this-edge)
                                         abut-edge-to-be-processed))]
                                      [address-of-this-vertex
                                       (find-vertex-address-in-vertex-list
                                        name-of-this-vertex
                                        (cdr ((find ':0-dimension-geometry-object-list)
                                              the-graph)))])
                                 (to-a-field-of-a-wlist--let-us-insert-a-val!
                                   ':abut-edge-list address-of-this-vertex
                                   (append
                                    ((find ':e) abut-edge-to-be-processed)
                                    `(:address ,address-of-new-maked-edge)))
                                 ;; RETURN-VAL:[3]--------------------------------
                                 address-of-this-vertex)))))
                    ;; RETURN-VAL-by-map:[2]--------------------------------
                    (append
                     ((find ':e) abut-edge-to-be-processed)
                     `(:address ,address-of-new-maked-edge))))
                    list-of-abut-edge-to-be-processed)))))
      
      ;; RETURN-VAL:[1]--------------------------------
      the-graph)))


;; test:
;; (add-vertex syntax-example-of-add-vertex
;;             example-graph)


;; (:type  :0-dimension-geometry-object-list
;;        #0=(:v vertex-1 :address #0# :can (:value 111)
;;               :abut-edge-list
;;               ((:e edge:1-->2 :address
;;                    #1=(:e edge:1-->2 :address #1# :can
;;                           (:edge-type black-arrow) :abut-vertex-list
;;                           (:v vertex-1 :address #0#)
;;                           (:v vertex-2 :address
;;                               #2=(:v vertex-2 :address #2# :can
;;                                      (:value 222) :abut-edge-list
;;                                      (:e edge:1-->2 :address
;;                                          #3=(:e edge:1-->2 :address #3# :can
;;                                                 (:edge-type red-arrow)
;;                                                 :abut-vertex-list
;;                                                 (:v vertex-1 :address #0#)
;;                                                 (:v vertex-2 :address #2#)))
;;                                      (:e edge:1-->2 :address #1#)
;;                                      (:e edge:2-->3 :address
;;                                          #4=(:e edge:2-->3 :address #4# :can
;;                                                 (:edge-type black-arrow)
;;                                                 :abut-vertex-list
;;                                                 (:v vertex-2 :address #2#)
;;                                                 (:v vertex-3 :address
;;                                                     #5=(:v vertex-3 :address #5#
;;                                                            :can (:value 333)
;;                                                            :abut-edge-list
;;                                                            (:e edge:1-->3 :address
;;                                                                #6=(:e edge:1-->3
;;                                                                       :address #6# :can
;;                                                                       (:edge-type
;;                                                                        red-arrow)
;;                                                                       :abut-vertex-list
;;                                                                       (:v vertex-1
;;                                                                           :address #0#)
;;                                                                       (:v vertex-3
;;                                                                           :address #5#)))
;;                                                            (:e edge:1-->3 :address
;;                                                                #7=(:e edge:1-->3
;;                                                                       :address #7# :can
;;                                                                       (:edge-type
;;                                                                        black-arrow)
;;                                                                       :abut-vertex-list
;;                                                                       (:v vertex-1
;;                                                                           :address #0#)
;;                                                                       (:v vertex-3
;;                                                                           :address #5#)))
;;                                                            (:e edge:2-->3 :address
;;                                                                #4#)
;;                                                            (:e edge:3-->2 :address
;;                                                                #8=(:e edge:3-->2
;;                                                                       :address #8# :can
;;                                                                       (:edge-type
;;                                                                        black-arrow)
;;                                                                       :abut-vertex-list
;;                                                                       (:v vertex-3
;;                                                                           :address #5#)
;;                                                                       (:v vertex-2
;;                                                                           :address #2#)))))))
;;                                      (:e edge:3-->2 :address #8#)))))
;;                (:e edge:1-->3 :address #7#)
;;                (:e edge:1-->2 :address #3#)
;;                (:e edge:1-->3 :address #6#)))
;;        #2# #5# :1-dimension-geometry-object-list #6# #3# #7# #1#
;;        #4# #8#)
;; (define syntax-example-of-add-edge
;;   '(:type edge-representation-for-add-edge

;;     :e kkk-edge
;;     :can (:value kkk)

;;     :abut-vertex-list-representation-for-add-edge
;;        (:v vertex-2)
;;        (:v vertex-3)

;;        ))

;; (define example-graph
;;   '(:type 

;;     :0-dimension-geometry-object-list

;;     #2=[:v vertex-2 :address #2#
;;         :can (:value 222)
;;         :abut-edge-list
;;            (:e edge:2-->3 :address #4#)
;;            (:e edge:3-->2 :address #6#)]

;;     #3=[:v vertex-3 :address #3#
;;         :can (:value 333)
;;         :abut-edge-list
;;            (:e edge:2-->3 :address #4#)
;;            (:e edge:3-->2 :address #6#)]


;;     :1-dimension-geometry-object-list

;;     #4=[:e edge:2-->3 :address #4#
;;         :can (:edge-type black-arrow)
;;         :abut-vertex-list
;;            (:v vertex-2 :address #2#)
;;            (:v vertex-3 :address #3#)]

;;     #6=[:e edge:3-->2 :address #6#
;;         :can (:edge-type black-arrow)
;;         :abut-vertex-list
;;            (:v vertex-3 :address #3#)
;;            (:v vertex-2 :address #2#)]


;;     ))

(define add-edge
  (lambda (edge-to-be-processed the-graph)
    (add-edge! edge-to-be-processed (list-copy the-graph))))

;; 比add-vertex简单多了
;; 构造新边
;; [1]加到:1-dimension-geometry-object-list中
;; 构造新边的过程中需要
;; [2]调整:0-dimension-geometry-object-list中的两个点
(define add-edge!
  (lambda (edge-to-be-processed the-graph)
    (let ([list-of-abut-vertex-to-be-processed
           (cdr ((find ':abut-vertex-list-representation-for-add-edge)
                 edge-to-be-processed))]
          [vertex-list-of-the-graph
           (cdr
            ((find ':0-dimension-geometry-object-list)
             the-graph))])

      ;; SIDE-EFFECT:[1]==========================================
      (let* ([head-of-processed-edge ((find ':e) edge-to-be-processed)]
             [address-of-processed-edge head-of-processed-edge]
             [name-of-vertex-1
              ((take 1)
               (car list-of-abut-vertex-to-be-processed))]
             [address-of-vertex-1
              (find-vertex-address-in-vertex-list
               name-of-vertex-1 
               vertex-list-of-the-graph)]
             [name-of-vertex-2
              ((take 1)
               (cadr list-of-abut-vertex-to-be-processed))]
             [address-of-vertex-2
              (find-vertex-address-in-vertex-list
               name-of-vertex-2
               vertex-list-of-the-graph)])
        (to-a-field-of-a-wlist--let-us-insert-a-val!
          ':1-dimension-geometry-object-list the-graph
          (append!
            head-of-processed-edge
            `(:address ,address-of-processed-edge)
            ((find ':can) edge-to-be-processed)
            `(:abut-vertex-list
              (:v ,name-of-vertex-1
                  :address
                  ,(let ([address-of-this-vertex address-of-vertex-1])
                     ;; SIDE-EFFECT:[2](1)==============================
                     (to-a-field-of-a-wlist--let-us-insert-a-val!
                       ':abut-edge-list address-of-this-vertex
                       (append
                        ((find ':e) edge-to-be-processed)
                        `(:address ,address-of-processed-edge)))
                     ;; RETURN-VAL:[2](1)--------------------------------
                     address-of-this-vertex))
              (:v ,name-of-vertex-2
                  :address
                  ,(let ([address-of-this-vertex address-of-vertex-2])
                     ;; SIDE-EFFECT:[2](2)==============================
                     (to-a-field-of-a-wlist--let-us-insert-a-val!
                       ':abut-edge-list address-of-this-vertex
                       (append
                        ((find ':e) edge-to-be-processed)
                        `(:address ,address-of-processed-edge)))
                     ;; RETURN-VAL:[2](2)--------------------------------
                     address-of-this-vertex)))))))
    
    ;; RETURN-VAL:[1]--------------------------------
    the-graph))

;; test:
;; (add-edge syntax-example-of-add-edge
;;           example-graph)

;; (:type
;;  
;;  :0-dimension-geometry-object-list
;;  #0=(:v vertex-2 :address #0# :can (:value 222)
;;         :abut-edge-list
;;         (:e kkk-edge :address
;;             #1=(:e kkk-edge :address #1# :can (:value kkk)
;;                    :abut-vertex-list (:v vertex-2 :address #0#)
;;                    (:v vertex-3 :address
;;                        #2=(:v vertex-3 :address #2# :can
;;                               (:value 333) :abut-edge-list
;;                               (:e kkk-edge :address #1#)
;;                               (:e edge:2-->3 :address
;;                                   #3=(:e edge:2-->3 :address #3# :can
;;                                          (:edge-type black-arrow)
;;                                          :abut-vertex-list
;;                                          (:v vertex-2 :address #0#)
;;                                          (:v vertex-3 :address #2#)))
;;                               (:e edge:3-->2 :address
;;                                   #4=(:e edge:3-->2 :address #4# :can
;;                                          (:edge-type black-arrow)
;;                                          :abut-vertex-list
;;                                          (:v vertex-3 :address #2#)
;;                                          (:v vertex-2 :address #0#)))))))
;;         (:e edge:2-->3 :address #3#)
;;         (:e edge:3-->2 :address #4#))
;;  #2# :1-dimension-geometry-object-list #1# #3# #4#)

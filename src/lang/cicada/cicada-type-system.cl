#+TITLE:  cicada type system
#+AUTHOR: 謝宇恆 / XIE Yuheng

* ===================================

* program as proof

** note type as proposition

   * a function returns a type
     can be viewed as [a family of] proposition

** pattern match

   * maybe (type -> type)
     nothing ({type #type} -> :type maybe)
     just ({type #type} :type -> :type maybe)

   * maybe-apply
     ({type #type #type'}
      :type' (:type -> :type') :type maybe -> :type')
     * :default :function nothing
     | :default
     * :default :function :value just
     | :value :function apply

** map

   * 在則式中
     當出現
     (...
      (:type -> :type') #function
      ...) 時
     :function (-> (:type -> :type'))

   * map
     ({type #type #type'}
      (:type -> :type') #function
       :type list -> :type' list)
     * :function null
     | null
     * :function :car :cdr cons
     | :car :function apply
       :function :cdr map cons

   * list-length? ({type #type} :type list natural -> type)
     list-length?null
     (---------->
      null 0 list-length?)
     list-length?cons
     ({natural #length
       :type #car
       :type list #cdr}
      :cdr :length list-length?
      ------------------------->
      :car :cdr cons :length add1 list-length?)

   * list-length?map
     ({type #type #type'
       (:type -> :type') #function
       :type list #list natural #length}
      :list :length list-length?
      -------------------------->
      :list :function map :length list-length?)
     * list-length?null
     | list-length?null
     * :inductive-hypothesis list-length?cons
     | :inductive-hypothesis list-length?map list-length?cons

   << explicit argument >>

   * list-length? ({type #type} :type list natural -> type)
     list-length?null
     (------------->
      null 0 list-length?)
     list-length?cons
     (natural #length
      :type #car
      :type list #cdr
      :cdr :length list-length?
      ------------------------->
      :car :cdr cons :length add1 list-length?)

   * list-length?map
     ({type #type #type'}
      (:type -> :type') #function
      :type list #list natural #length
      :list :length list-length?
      -------------------------->
      :list map :function apply :length list-length?)
     * :function null 0
       list-length?null
     | list-length?null
     * :function :car :cdr cons :length add1
       :length
       :car
       :cdr
       :inductive-hypothesis
       list-length?cons
     | :length
       :car :function apply
       :cdr :function map
       :function :cdr :length :inductive-hypothesis list-length?map
       list-length?cons

** red-black-tree

   * red-black-tree
     import
       preliminaries
     assume
       key (-> type)
       compare (key key -> order)
       value (-> type)

   * color (-> type)
     red (-> color)
     black (-> color)

   * tree (-> type)
     empty (-> tree)
     node (tree tree color key value -> tree)

   * balance
     (tree -> tree)
     * :a
       :b red ::x node
                    :c red ::y node
                                 :d black ::z node
     | :a
       :b black ::x node
       :c
       :d black ::z node red ::y node
     *              :a
       :b
       :c red ::y node red ::x node
                                 :d black ::z node
     | :a
       :b black ::x node
       :c
       :d black ::z node red ::y node
     *                           :a
       :b
       :c red ::y node
                    :d red ::z node black ::x node
     | :a
       :b black ::x node
       :c
       :d black ::z node red ::y node
     *                           :a
                    :b
       :c
       :d red ::z node red ::y node black ::x node
     | :a
       :b black ::x node
       :c
       :d black ::z node red ::y node
     * :already-balanced-tree
     | :already-balanced-tree

   * insert,help
     (tree key value -> tree)
     * empty :key :value
     | empty empty
       red :key :value node
     * :left :right
       :color :key :value node
       :key' :value'
     | :key :key' compare
       * equal
       | :left
         :right
         :color :key' :value' node
       * less
       | :left :key' :value' insert,help
         :right
         :color :key :value node balance
       * greater
       | :left
         :right :key' :value' insert,help
         :color :key :value node balance

   * blacken-root
     (tree -> tree)
     * empty
     | empty
     * :left :right :color ::key-value node
     | :left :right black ::key-value node

   * insert
     (tree key value -> tree)
     insert,help
     blacken-root

   * black-high? (tree natural -> type)
     black-high?empty
     (--------------->
      empty 1 black-high?)
     black-high?red
     ({natural #high tree #left #right key value ##key-value}
      :left :high black-high?
      :right :high black-high?
      ----------------------->
      :right :high red ::key-value node :high black-high?)
     black-high?black
     ({natural #high tree #left #right key value ##key-value}
      :left :high black-high?
      :right :high black-high?
      ----------------------->
      :right :high black ::key-value node :high add1 black-high?)

   * black-high?blacken-root
     ({tree #tree natural #high}
      :tree :high black-high?
      ---------------------->
      natural #high'
      :tree blacken-root :high' black-high?)
     * black-high?empty
     | 1 black-high?empty
     * :black-high?left :black-high?right black-high?red
     | _ :black-high?left :black-high?right black-high?black
     * :black-high?left :black-high?right black-high?black
     | _ :black-high?left :black-high?right black-high?black

   * black-high?example
     assume
       k1 (-> key)
       k2 (-> key)
       v1 (-> value)
       v2 (-> value)
     * t (-> tree)
       empty empty red k1 v1 node
                            empty black k2 v2 node

     * black-high?t
       (-------------->
        t 2 black-high?)
       black-high?empty
       black-high?empty
       black-high?red
       black-high?empty
       black-high?black

   * black-high?balance,red
     ({tree #left #right key value ##key-value natural #high}
      :left :high black-high?
      :right :high black-high?
      ----------------------->
      :left :right red ##key-value balance :high add1 black-high?)
     *?
     |?

   * black-high?balance,black
     ({tree #left #right key value ##key-value natural #high}
      :left :high black-high?
      :right :high black-high?
      ----------------------->
      :left :right black ##key-value balance :high add1 black-high?)
     *?
     |?

   * black-high?insert,help
     ({tree #tree key value ##key-value natural #high}
      :tree :high black-high?
      ---------------------->
      :tree ::key-value insert,help :high black-high?)
     * black-high?empty
     |
     *
     |

   * black-high?insert
     ({tree #tree key value ##key-value natural #high}
      :tree :high black-high?
      ---------------------->
      natural #high'
      :tree ::key-value insert :high' black-high?)
     black-high?insert,help
     black-high?blacken-root

** note in & exi

* generic function

** note typeclass & functor

   * 類似 typeclass
     但是相反 描述某些抽象的特性 然後找滿足這些抽象的特性的例子
     此時我們 觀察已有的類型的共同點 然後把這些共同點提取出來

   * 一旦把相似性提取出來之後
     我們就能利用 用以編碼 相似性 的接口函數
     來寫適用於多個類型的函數了 [所謂 generic function]

   * 技巧是 把原本用 子集 描述的東西
     用 映射的值域 來描述
     而 映射的義域 用以勾勒出 值域 做爲子集的範圍
     定義類型 的 構造子 就是用以 用以勾勒出 值域 範圍 的 子集
     同時還要有一個配合這個 所定義的類型的 解釋函數
     * 就像是 指稱語義 一樣
       所以 有一種 DSL[domain specific language] 的感覺
     比如 對於 functor 解釋函數是 functor:apply
     比如 對於 format 解釋函數是 data

** format

** functor

   * functor (-> type1)
     functor:list (functor -> functor)
     functor:identity (-> functor)
     functor:product (functor functor -> functor)
     functor:constant (type -> functor)

   * functor:apply (type functor -> type)
     * :type :functor functor:list
     | :type :functor functor:apply list
     * :type functor:identity
     | :type
     * :type :functor1 :functor2 functor:product
     | :type :functor1 functor:apply
       :type :functor2 functor:apply product
     * :type :constant-type functor:constant
     | :constant-type

   * functor:map
     (functor #functor {type #type1 #type2}
      (:type1 -> :type2) ->
      (:type1 :functor functor:apply -> :type2 :functor functor:apply))
     *
     |
     *
     |
     *
     |
     *
     |

* >< homotopy type theory

* ><>< type class

** equal & order

   * equal-able < type
     equal??
     ({equal-able #value} :value :value -> (true | false))

   * not-equal??
     ({equal-able #value} :value :value -> (true | false))
     equal?? not

   * natural (-> equal-able)
     ><><><

   * order-able < equal-able
     ({order-able #value} :value :value -> (less-than | equal | greater-than))

   * sort
     ({order-able #value} :value list -> :value list)
     ><><><

** functor

   * map-able < (type -> type) << have-functor >>

   * function-map
     ({type #type #type'} {map-able #functor}
      (:type -> :type')
      :type :functor apply -> :type' :functor apply)

   * functor-map
     ({type #type #type'} {map-able #functor}
      (:type -> :type') ->
      (:type :functor apply -> :type' :functor apply))

   * list (-> map-able)

   * map
     ({(list -> list) #functor}
       (list -> list) list :functor apply -> list :functor apply)
     * :function null
     | null
     * :function :car :cdr cons
     | :car :function apply
       :function :cdr map cons

** ><

   * map-able
     (type -> type) << have-functor >>

   * function-map
     ({type #type #type'} {map-able #functor}
      (:type -> :type')
      :type :functor apply -> :type' :functor apply)

   * functor-map
     ({type #type #type'} {map-able #functor}
      (:type -> :type') ->
      (:type :functor apply -> :type' :functor apply))

   * list (-> map-able)

   * map
     ({(list -> list) #functor}
       (list -> list) list :functor apply -> list :functor apply)
     * :function null
     | null
     * :function :car :cdr cons
     | :car :function apply
       :function :cdr map cons

* ===================================

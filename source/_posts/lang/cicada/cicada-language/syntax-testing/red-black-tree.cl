+ author
  謝宇恆 / XIE Yuheng

* order
  (-> type)

  * less
    (-> order)

  * equal
    (-> order)

  * greater
    (-> order)

* red-black-tree

    + assume

      * key
        (-> type)

      * compare
        (key key -> order)

      * value
        (-> type)

    * color
      (-> type)

      * red
        (-> color)

      * black
        (-> color)

    * tree
      (-> type)

      * empty
        (-> tree)

      * node
        (tree tree color key value -> tree)

    * balance
      (tree -> tree)
      + :a
        :b red ::x node
                     :c red ::y node
                                  :d black ::z node
      | :a
        :b black ::x node
        :c
        :d black ::z node red ::y node
      +              :a
        :b
        :c red ::y node red ::x node
                                  :d black ::z node
      | :a
        :b black ::x node
        :c
        :d black ::z node red ::y node
      +                           :a
        :b
        :c red ::y node
                     :d red ::z node black ::x node
      | :a
        :b black ::x node
        :c
        :d black ::z node red ::y node
      +                           :a
                     :b
        :c
        :d red ::z node red ::y node black ::x node
        | :a
          :b black ::x node
          :c
          :d black ::z node red ::y node
      + :already-balanced-tree
      | :already-balanced-tree


    * insert,help
      (tree key value -> tree)
      + empty :key :value
      | empty empty
        red :key :value node
      + :left :right
        :color :key :value node
        :key' :value'
      | :key :key' compare
        + equal
        | :left
          :right
          :color :key' :value' node
        + less
        | :left :key' :value' insert,help
          :right
          :color :key :value node balance
        + greater
        | :left
          :right :key' :value' insert,help
          :color :key :value node balance

    * blacken-root
      (tree -> tree)
      + empty
      | empty
      + :left :right :color ::key-value node
      | :left :right black ::key-value node

    * insert
      (tree key value -> tree)
      insert,help
      blacken-root

    * black-high?
      (tree natural -> type)

      * black-high?empty
        (--------------->
         empty 1 black-high?)

      * black-high?red
        ({natural #high tree #left #right key value ##key-value}
         :left :high black-high?
         :right :high black-high?
         ----------------------->
         :right :high red ::key-value node :high black-high?)

      * black-high?black
        ({natural #high tree #left #right key value ##key-value}
         :left :high black-high?
         :right :high black-high?
         ----------------------->
         :right :high black ::key-value node :high succ black-high?)

    * black-high?blacken-root
      ({tree #tree natural #high}
       :tree :high black-high?
       ---------------------->
       natural #high'
       :tree blacken-root :high' black-high?)
      + black-high?empty
      | 1 black-high?empty
      + :black-high?left :black-high?right black-high?red
      | _ :black-high?left :black-high?right black-high?black
      + :black-high?left :black-high?right black-high?black
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
         :left :right red ##key-value balance :high succ black-high?)
        +
        |

      * black-high?balance,black
        ({tree #left #right key value ##key-value natural #high}
         :left :high black-high?
         :right :high black-high?
         ----------------------->
         :left :right black ##key-value balance :high succ black-high?)
        +
        |

      * black-high?insert,help
        ({tree #tree key value ##key-value natural #high}
         :tree :high black-high?
         ---------------------->
         :tree ::key-value insert,help :high black-high?)
        + black-high?empty
        |
        +
        |

      * black-high?insert
        ({tree #tree key value ##key-value natural #high}
         :tree :high black-high?
         ---------------------->
         natural #high'
         :tree ::key-value insert :high' black-high?)
        black-high?insert,help
        black-high?blacken-root

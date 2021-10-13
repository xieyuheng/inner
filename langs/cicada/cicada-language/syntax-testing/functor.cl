+ author
  謝宇恆 / XIE Yuheng

* functor

  * functor
    (-> type1)

    * functor:list
      (functor -> functor)

    * functor:identity
      (-> functor)

    * functor:product
      (functor functor -> functor)

    * functor:constant
      (type -> functor)

  * functor:apply
    ( type functor -> type )
    + :type :functor functor:list
    | :type :functor functor:apply list
    + :type functor:identity
    | :type
    + :type :functor1 :functor2 functor:product
    | :type :functor1 functor:apply
      :type :functor2 functor:apply product
    + :type :constant-type functor:constant
    | :constant-type

  * functor:map
    (functor #functor {type #type1 #type2}
     (:type1 -> :type2) ->
     (:type1 :functor functor:apply -> :type2 :functor functor:apply))
    +
    |
    +
    |
    +
    |
    +
    |

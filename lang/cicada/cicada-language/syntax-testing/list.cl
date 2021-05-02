author: 謝宇恆 / XIE Yuheng

* natural

  type-constructor: natural
    type:
      +
      | type

    element-constructor: zero
      type:
        +
        | natural

    element-constructor: succ
      type:
        + natural
        | natural

  function: add
    type:
      + natural natural
      | natural
    match:
      + :m zero
      | :m
      + :m :n succ
      | :m :n recur succ

  function: mul
    type:
      + natural natural
      | natural
    match:
      + :m zero
      | zero
      + :m :n succ
      | :m :n recur :m add

* list

  type-constructor: list
    type:
      + { type #type }
        :type
      | type

    element-constructor: null
      type:
        + { type #type }
        | :type list

    element-constructor: cons
      type:
        + { type #type }
          :type :type list
        | :type list

  function: map
    type:
      + { type #type #type' }
        :type list
        ( :type -- :type' )
      | :type' list
    match:
      + null :function
      | null
      + :car :cdr cons :function
      | :car :function apply
        :cdr :function recur cons

  * has-length

    type-constructor: has-length
      type:
        + { type #type }
          :type list natural
        | type

      element-constructor: null
        type:
          +
          | null
            zero
            has-length

      element-constructor: cons
        type:
          + { type #type
              natural #length
              :type #car
              :type list #cdr }
            :cdr
            :length
            has-length
          | :car :cdr cons
            :length succ
            has-length

    function: map
      type:
        + { type #type #type'
            ( :type -- :type' ) #function
            :type list #list
            natural #length }
          :list
          :length
          has-length
        | :list :function map
          :length
          has-length
      match:
        + has-length:null
        | has-length:null
        + :inductive-hypothesis
          has-length:cons
        | :inductive-hypothesis recur
          has-length:cons

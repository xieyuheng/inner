* natural

  :
  | type

  * zero

    :
    | natural

  * succ

    : natural
    | natural

  * add

    : natural natural
    | natural

    + :m zero
    | :m

    + :m :n succ
    | :m :n recur succ

  * mul

    : natural natural
    | natural

    + :m zero
    | zero

    + :m :n succ
    | :m :n recur :m add

  * natural-recursion

    : {type #type}
      :type (:type natural -> :type) natural
    | :type

    + :base :step zero
    | :base

    + :base :step :n succ
    | :base :step :n recur
      :n :step apply

    * add

      : natural natural
      | natural

      + :m :n
      | :m [drop succ] :n natural-recursion

    * mul

      : natural natural
      | natural

      + :m :n
      | zero [drop :m add] :n natural-recursion

  * simpler-natural-recursion

    : {type #type}
      :type (:type -> :type) natural
    | :type

    + :base :step zero
    | :base

    + :base :step :n succ
    | :base :step :n recur
      :step apply

    * add

      : natural natural
      | natural

      + :m :n
      | :m [succ] :n simpler-natural-recursion

    * mul

      : natural natural
      | natural

      + :m :n
      | zero [:m add] :n simpler-natural-recursion

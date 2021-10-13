+ author
  謝宇恆 / XIE Yuheng

* vector

  - {type #type}
    natural :type
  | :type

  * null

    - {type :type}
    | zero :type vector

  * cons

    - {natural #length}
      :type
      :length :type vector
    | :length succ :type vector

* append

  - {type #type natural #n #m}
    :n :type vector :m :type vector
  | :n :m add :type vector

  + null :vector
  | :vector

  + :head :tail cons :vector
  | :head :tail :vector recur cons
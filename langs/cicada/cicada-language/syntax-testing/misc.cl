+ author
  謝宇恆 / XIE Yuheng

* id

  - {set #set} :set
  | :set

  + :n
  | :n

* maybe

  - type
  | type

  * nothing

    - {type #type}
    | :type maybe)

  * just

    - {type #type} :type
    | :type maybe

  * maybe-apply

    - {type #type #type'}
      :type' (:type -> :type') :type maybe
    | :type'

    + :default :function nothing
    | :default

    + :default :function :value just
    | :value :function apply

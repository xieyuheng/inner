;; plays.json...

{:hamlet {:name "Hamlet":type "tragedy"}
 :as-like {:name "As You Like It":type "comedy"}
 :othello {:name "Othello":type "tragedy"}}

;; invoices.json...

[{:customer "BigCo"
  :performances
  [{:play-id :hamlet :audience 55}
   {:play-id :as-like :audience 35}
   {:play-id :othello :audience 40}]}]

;; Running
;;     (statement (inv invoices) (plays (list play)))
;; on the test data data above,
;; should result in the following output:

;; Statement for BigCo
;;   Hamlet: $650.00 (55 seats)
;;   As You Like It: $580.00 (35 seats)
;;   Othello: $500.00 (40 seats)
;; Amount owed is $1,730.00
;; You earned 47 credits


(map cons
     (split (lc-dialog-get "challenges" "ch-names") ",")
     '("challenges/goliath.lisp"
       "challenges/goblin_raid.lisp"
       "challenges/backdoor.lisp"
       "challenges/masonry.lisp"
       "challenges/mycelium.lisp"
       "challenges/porcupine1.lisp"
       "challenges/porcupine2.lisp"
       "challenges/decimator.lisp"
       "challenges/arcgun_defense.lisp"
       "challenges/beacon.lisp"
       "challenges/exchange.lisp"
       "challenges/demolition.lisp"
       "challenges/fire_brigade.lisp"))

;; note: the result format is '((challenge-name . path) ...)
;; During localization, I had to move the names to a separate file.

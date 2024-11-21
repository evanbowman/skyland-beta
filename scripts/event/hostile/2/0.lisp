;;;
;;; hostile/2/0.lisp
;;;


(opponent-init 7 'hostile)


(island-configure
 (opponent)
 '((power-core 1 13)
   (power-core 3 13)
   (hull 2 11)
   (hull 2 12)
   (infirmary 3 11)
   (stairwell 5 11)
   (cannon 0 13)
   (cannon 0 12)
   (cannon 0 11)
   (hull 2 10)
   (hull 3 10)
   (hull 4 10)
   (cannon 0 14)
   (missile-silo 1 11)
   (transporter 6 13)
   (missile-silo 6 11)
   (hull 5 10)
   (forcefield 6 10)))


(chr-new (opponent) 3 14 'hostile 0)
(chr-new (opponent) 2 14 'hostile 0)

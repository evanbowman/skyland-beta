;;;
;;; hostile/0/6.lisp
;;;


(opponent-init 3 'hostile)


(flag-show (opponent) 0)


(island-configure
 (opponent)
 '((missile-silo 0 13)
   (power-core 1 13)))

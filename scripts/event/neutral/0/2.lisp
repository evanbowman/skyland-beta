;;;
;;; neutral/0/2.lisp
;;;


(load-dialog "zone1" "ab_intro")


(let ((opts
       '((7 . ((plundered-room 0 13)
               (power-core 1 13)
               (hull 1 12)
               (hull 2 12 6)
               (stairwell 3 11)
               (workshop 4 13)
               (workshop 4 11)
               (hull 4 10)
               (hull 5 10 12)
               (masonry 6 14 3)
               (masonry 6 13 3)
               (windmill 6 12)
               (hull 6 11)))
         (5 . ((hull 0 14)
               (power-core 1 13)
               (hull 1 12)
               (workshop 3 13))))))
  (let ((opt (sample opts)))
    (opponent-init (car opt) 'neutral)
    (island-configure (opponent) (cdr opt))))


(if (choice 2)
    (secret
     1 12
     (get-dialog "zone1" "ab_secret")))



(defn on-converge [0]
  (setq on-converge nil)

  (let ((amt (+ 400 (choice 900))))

    (when (equal (zone) 3)
      (setq amt (+ 800 (choice 900))))

    (load-dialog "zone1" "ab_award" amt)

    (adventure-log-add 11 (list amt))

    (coins-add amt)

    ((eval-file "/scripts/util/pickup_cart.lisp") 1
     (get-dialog "zone1" "ab_cart"))))

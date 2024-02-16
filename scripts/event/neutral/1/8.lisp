;;;
;;; neutral/1/8.lisp
;;;


(lc-dialog-load "forsaken-town" "intro")



(opponent-init 9 'neutral)


(island-configure
 (opponent)
 '((power-core 0 13)
   (manufactory 0 11)
   (masonry 2 14 0)
   (masonry 2 13 0)
   (water-source 3 14)
   (bridge 3 12)
   (masonry 4 14 0)
   (masonry 4 13 0)
   (bridge 5 12)
   (water-source 5 14)
   (power-core 6 9)
   (bronze-hull 6 11)
   (masonry 6 13 0)
   (masonry 6 14 0)
   (workshop 7 11)
   (masonry 7 13 0)
   (water-source 7 14)
   (masonry 8 13 0)
   (masonry 8 14 0)))

(flag-show (opponent) 7)


(defn on-converge [0]
  (let ((c (choice 5))
        (end (lambda
               ((eval-file "/scripts/util/pickup_cart.lisp") 4
         (lc-dialog-get "forsaken-town" "cart")))))
    (cond
     ((equal c 0)
      (let ((amt (+ 200 (choice 400))))
        (coins-add amt)
        (lc-dialog-load-fmt "forsaken-town" "nothing" amt)
        (adventure-log-add 38 (list amt))
        (end)))
     (true
      (let ((opts '(power-core
                    infirmary
                    manufactory
                    incinerator
                    warhead
                    beam-gun
                    splitter)))
        (let ((pick (sample opts)))
          (dialog
           (lc-dialog-get "forsaken-town" "found1")
           (rinfo 'name pick)
           (lc-dialog-get "forsaken-town" "found2"))
          (adventure-log-add 38 (rinfo 'name pick))
          (defn on-dialog-closed [0]
            (setq on-dialog-closed nil)
            (alloc-space pick)
            (sel-input
             pick
             (format (lc-dialog-get "forsaken-town" "pick")
                     (car (rinfo 'size pick))
                     (cdr (rinfo 'size pick)))
             (lambda
               (room-new (player) `(,pick ,$1 ,$2))
               (sound "build0")
               (lc-dialog-load "forsaken-town" "done")
               (end))))))))))

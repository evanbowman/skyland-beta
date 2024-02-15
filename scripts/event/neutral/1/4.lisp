;;;
;;; neutral/1/4.lisp
;;;


(load-dialog "overgrown-isle" "intro")


(opponent-init 7 'neutral)

(island-configure
 (opponent)
 '((masonry 0 14)
   (masonry 0 13)
   (mycelium 0 12)
   (mycelium 0 11)
   (power-core 1 13)
   (masonry 1 12)
   (mycelium 1 11)
   (masonry 2 12)
   (mycelium 2 11)
   (mycelium 3 12)
   (mycelium 3 7)
   (mycelium 3 11)
   (mycelium 3 10)
   (mycelium 3 9)
   (manufactory 3 13)
   (mycelium 3 8)
   (mycelium 4 7)
   (radar 4 8)
   (power-core 4 10)
   (mycelium 4 12)
   (mycelium 5 7)
   (mycelium 5 8)
   (mycelium 5 9)
   (mycelium 5 12)
   (masonry 6 14)
   (mycelium 6 12)
   (mycelium 6 13)
   (mycelium 6 11)
   (mycelium 6 10)
   (mycelium 6 9)))

(flag-show (opponent) 4)


(defn on-converge [0]
  (load-dialog "overgrown-isle" "offer")
  (dialog-await-y/n))


(defn on-dialog-accepted [0]
  (let ((end (lambda
               ((eval-file "/scripts/util/pickup_cart.lisp") 6
                (get-dialog "overgrown-isle" "cart")))))
    (if (choice 3)
        (progn
          (let ((locs (construction-sites (player) '(1 . 1))))
            (when locs
              (let ((c (get locs (choice (length locs)))))
                (room-new (player) (list 'mycelium (car c) (cdr c))))
              (adventure-log-add 34 '())
              (load-dialog "overgrown-isle" "infected")))
          (end))
      (let ((temp (+ 1000 (choice 1000))))
        (load-dialog "overgrown-isle" "reward" temp)
        (coins-add temp)
        (adventure-log-add 35 (list temp))
        (end)))))



(setq on-dialog-declined exit)

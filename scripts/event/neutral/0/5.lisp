
(load-dialog "orphan" "intro")

(opponent-init 8 'neutral)

(island-configure
 (opponent)
 '((hull 0 14)
   (hull 0 13)
   (masonry 0 12)
   (torch 0 10)
   (masonry 1 14)
   (masonry 1 13)
   (masonry 2 14)
   (plundered-room 2 12)
   (torch 3 9)
   (masonry 3 14)
   (plundered-room 3 12)
   (masonry 4 14)
   (power-core 5 13)))

(defn on-fadein [0]
  (fire-new (opponent) 3 9)
  (fire-new (opponent) 0 10)
  (setq on-fadein nil))

(flag-show (opponent) 7)


(chr-new (opponent) 1 12 'neutral 0)

(setq on-converge
      (lambda
        (load-dialog "orphan" "greeting")

        (setq on-dialog-closed
              (lambda
                (load-dialog "orphan" "offer")
                (dialog-await-y/n)
                (setq on-dialog-closed '())))

        (setq on-converge nil)))


(setq on-dialog-accepted
      (lambda
        (let ((temp (chr-slots (player)))
              (end (lambda
                     ((eval-file "/scripts/util/pickup_cart.lisp") 2
                      (get-dialog "orphan" "cart")))))
          (if temp
              (progn
                (setq temp (get temp (choice (length temp))))
                (chr-new (player) (car temp) (cdr temp) 'neutral nil)
                (chr-del (opponent) 1 12)
                (adventure-log-add 15 '())
                (load-dialog "orphan" "join")
                (end))
            (progn
              (load-dialog "orphan" "no-room")
              (defn on-dialog-closed [0]
                (dialog "orphan" "beg")
                (defn on-dialog-closed [0]
                  (alloc-space 'cargo-bay)
                  (sel-input 'cargo-bay
                             (get-dialog "orphan" "build")
                             (lambda
                               (sound "build0")
                               (room-new (player) `(cargo-bay ,$1 ,$2))
                               (chr-del (opponent) 1 12)
                               (chr-new (player) $1 (+ 1 $2) 'neutral nil)
                               (load-dialog "orphan" "ungrateful")
                               (defn on-dialog-closed [0]
                                 (adventure-log-add 15 '())
                                 (load-dialog "orphan" "result")
                                 (end)))))))))

        (exit)))


(setq on-dialog-declined exit)

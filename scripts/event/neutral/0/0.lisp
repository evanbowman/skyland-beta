;;;
;;; neutral/0/0.lisp
;;;


(load-dialog "castaway" "intro")


(opponent-init 6 'neutral)


(island-configure
 (opponent)
 '((power-core 3 13)
   (coconut-palm 5 12)
   (hull 5 14)))


(secret
 5 14
 (get-dialog "castaway" "secret"))


(chr-new (opponent) 1 14 'neutral 0)


(setq on-converge
      (lambda
        (load-dialog "castaway" "greet")

        (setq on-dialog-closed
              (lambda
                (load-dialog "castaway" "offer")
                (dialog-await-y/n)
                (setq on-dialog-closed '())))

        (setq on-converge nil)))


(setq on-dialog-accepted
      (lambda
        (let ((temp (chr-slots (player)))
              (join (lambda
                      (adventure-log-add 7 '())
                      (dialog $0))))
          (if temp
              (progn
                (setq temp (get temp (choice (length temp))))
                (chr-new (player) (car temp) (cdr temp) 'neutral nil)
                (chr-del (opponent) 1 14)
                (if (or (equal (choice 2) 1) (< (coins) 300))
                    (join (get-dialog "castaway" "join1"))
                  (progn
                    (coins-set (- (coins) 300))
                    (join (get-dialog "castaway" "join2")))))
            (progn
              (load-dialog "castaway" "full")
              (defn on-dialog-closed [0]
                (load-dialog "castaway" "plead")
                (defn on-dialog-closed [0]
                  (alloc-space 'ladder)
                  (sel-input 'ladder
                             (get-dialog "castaway" "place_ladder")
                             (lambda
                               (sound "build0")
                               (room-new (player) `(ladder ,$1 ,$2))
                               (chr-del (opponent) 1 14)
                               (chr-new (player) $1 (+ 1 $2) 'neutral nil)
                               (load-dialog "castaway" "thanks")
                               (defn on-dialog-closed [0]
                                 (join (get-dialog "castaway" "join1"))
                                 (setq on-dialog-closed nil)
                                 (exit)))))))))
        (exit)))


(setq on-dialog-declined exit)

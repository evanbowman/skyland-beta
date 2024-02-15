;;;
;;; neutral/2/3.lisp
;;;


(load-dialog "decimator" "intro")


(opponent-init 9 'neutral)


(island-configure
 (opponent)
 '((power-core 3 13)
   (power-core 3 11)
   (decimator 0 13)
   (plundered-room 5 13)
   (plundered-room 5 11)
   (plundered-room 6 13)
   (hull 8 14)
   (hull 8 13)
   (hull 8 12)))

(chr-new (opponent) 2 14 'neutral 0)


(defn on-converge [0]
  (load-dialog "decimator" "offer")
  (setq on-converge nil)
  (dialog-await-binary-q "Here's the money…" "no thanks"))


(setq on-dialog-declined exit)


(defn on-dialog-accepted [0]
  (if (bound? 'fut) (unbind 'fut))

  (if (< (coins) 1500)
      (progn
        (load-dialog "decimator" "low-funds")
        (dialog-await-y/n)
        (let ((f (this)))
          (defn fut [0]
            (if (> (coins) 1499)
                (progn
                  (load-dialog "decimator" "funds-good")
                  (setq on-dialog-closed f))
              (f))))
        (setq on-dialog-accepted (lambda (on-timeout 15000 'fut)))
        (setq on-dialog-declined (lambda (unbind 'fut) (exit))))
    (progn
      (coins-add -1500)

      ;; We wouldn't want the player to get into a position where there isn't
      ;; enough terrain to place the weapon! The game would get locked up. Just
      ;; give the player some terrain for free.
      (dotimes 2
        (if (not (construction-sites (player) '(2 . 2)))
            (terrain-set (player) (+ (terrain (player)) 1))))

      (sel-input
       'decimator
       (get-dialog "decimator" "place")
       (lambda
         (room-new (player) (list 'decimator $1 $2))
         (room-del (opponent) 0 13)
         (load-dialog "decimator" "installed")
         (adventure-log-add 44 '())

         (setq on-dialog-closed '())
         (exit))))))

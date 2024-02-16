;;;
;;; neutral/1/1.lisp
;;;


(lc-dialog-load "risky-castle" "intro")



(opponent-init 5 'neutral)


(island-configure
 (opponent)
 '((masonry 0 14 3)
   (hull 0 13)
   (masonry 2 12 3)
   (power-core 1 13)
   (hull 1 12)
   (workshop 3 13)))

(secret
 0 13
 (lc-dialog-get "risky-castle" "secret"))



(let ((trap (choice 2)))

  (setq on-dialog-accepted
      (lambda
        (if (not trap)
            (let ((val (+ 600 (choice 300))))
              (lc-dialog-load-fmt "risky-castle" "award" val)
              (coins-add val)
              (adventure-log-add 29 (list val))
              (exit))
          (progn
            (island-configure
             (opponent)
             '((hull 2 12)
               (cannon 0 13)
               (arc-gun 0 14)
               (power-core 1 13)
               (hull 1 12)
               (missile-silo 3 13)
               (missile-silo 4 13)))
            (opponent-mode 'hostile)
            (flag-show (opponent) 0)
            (adventure-log-add 30 '())
            (lc-dialog-load "risky-castle" "trap"))))))


(setq on-converge
      (lambda
        (lc-dialog-load "risky-castle" "offer")
        (setq on-converge '())
        (dialog-await-y/n)))


(setq on-dialog-declined
      (lambda
        (lc-dialog-load "risky-castle" "decline")
        (exit)))

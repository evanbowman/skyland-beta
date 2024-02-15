;;;
;;; neutral/2/2.lisp
;;;



(load-dialog "warship" "intro")



(eval-file "/scripts/event/hostile/3/0.lisp")


(opponent-mode 'neutral)

(flag-show (opponent) 2)


(let ((val (+ 1000 (choice 800))))
  (setq on-converge
        (lambda
          (load-dialog "warship" "demand" val)
          (dialog-await-y/n)
          (setq on-converge nil)))


  (setq on-dialog-accepted
        (lambda
          (if (> 500 (coins))
              (progn
                (opponent-mode 'hostile)
                (adventure-log-add 42 '())
                (load-dialog "warship" "low-funds"))
            (progn
              (coins-add (- val))
              (load-dialog "warship" "bribe-accepted")
              (adventure-log-add 43 '())
              (exit))))))


(setq on-dialog-declined
      (lambda
        (opponent-mode 'hostile)
        (adventure-log-add 42 '())
        (load-dialog "warship" "decline")))

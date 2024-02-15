;;;
;;; neutral/0/1.lisp
;;;


(load-dialog "goblin-stronghold" "intro")



(eval-file "/scripts/event/hostile/2/2.lisp")


(opponent-mode 'neutral)



(let ((val (if (equal (difficulty) 0)
               (+ 900 (choice 500))
             (max (list (+ 500 (choice 500))
                        (/ (coins) 3))))))
  (setq on-converge
        (lambda
          (load-dialog "goblin-stronghold" "demand" val)

          (dialog-await-binary-q (get-dialog "goblin-stronghold" "opt1")
                                 (get-dialog "goblin-stronghold" "opt2"))
          (setq on-converge nil)))


  (setq on-dialog-accepted
        (lambda
          (if (> val (coins))
              (progn
                (opponent-mode 'hostile)
                (adventure-log-add 32 '())
                (load-dialog "goblin-stronghold" "low-funds"))
            (progn
              (coins-add (- val))
              (load-dialog "goblin-stronghold" "extortion" val)
              (adventure-log-add 31 (list val))
              (exit))))))




(setq on-dialog-declined
      (lambda
        (opponent-mode 'hostile)
        (adventure-log-add 33 '())
        (load-dialog "goblin-stronghold" "decline")))

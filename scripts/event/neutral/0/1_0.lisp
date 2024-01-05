;;;
;;; neutral/0/1.lisp
;;;


(load-dialog "mr-intro")


(opponent-init 5 'neutral)

(island-configure
 (opponent)
 '((power-core 3 13)
   (cannon 0 14)
   (cannon 0 13)
   (cannon 0 12)
   (missile-silo 3 11)
   (hull 4 12)
   (hull 1 14)
   (hull 2 14)
   (hull 2 13)
   (hull 1 12)
   (hull 2 12)
   (hull 1 13)))

(flag-show (opponent) 1)


(defn on-converge [0]
  (load-dialog "mr-greeting")
  (dialog-opts-reset)
  (dialog-opts-push (get-dialog "mr-pay") on-dialog-accepted)

  (dialog-opts-push (get-dialog "mr-bluf")
                    (lambda

                      (defn cb0 [0]
                        (emit (opponent) 0 12 (terrain (player)) 0))

                      (defn cb1 [0]
                        (emit (opponent) 0 13 (terrain (player)) 0))

                      (defn cb2 [0]
                        (emit (opponent) 0 14 (terrain (player)) 0))

                      (defn cb3 [0]
                        (load-dialog "mr-last-chance")
                        (dialog-await-binary-q (get-dialog "mr-pay-2")
                                               (get-dialog "mr-decline-2"))
                        (unbind 'cb0 'cb1 'cb2 'cb3))

                      (on-timeout 400 'cb0)
                      (on-timeout 600 'cb1)
                      (on-timeout 800 'cb2)
                      (on-timeout 2000 'cb3)))

  (dialog-opts-push (get-dialog "mr-decline") on-dialog-declined)
  (setq on-converge nil))


(let ((scr
       (lambda
         (dialog $0)
         (defn on-dialog-closed [0]
           (load-dialog "mr-goblin-agree")
           (defn on-dialog-closed [0]
             (load-dialog "mr-trans-cut")
             (setq on-dialog-closed nil)))
         (opponent-mode 'hostile))))
  (setq on-dialog-accepted
        (lambda
          (if (< (coins) 600)
              (progn
                (adventure-log-add 12 '())
                (scr (get-dialog "mr-not-enough")))
            (progn
              (adventure-log-add 13 (list 600))
              (coins-add -600)
              (load-dialog "mr-smug")
              (exit)))))


  (setq on-dialog-declined
        (lambda
          (adventure-log-add 14 '())
          (scr (get-dialog "mr-attack")))))



(load-dialog "crew-gamble" "intro")


(opponent-init 5 'neutral)


(island-configure
 (opponent)
 '((power-core 3 13)
   (hull 2 13)
   (hull 2 14)
   (hull 3 12)
   (hull 4 12)))


(chr-new (opponent) 1 14 'neutral 0)


(setq on-converge
      (lambda
        (load-dialog "crew-gamble" "offer")

        (dialog-await-y/n)
        (setq on-converge nil)))


(let ((bad (choice 2)))

  (if (< (zone) 2)
      (secret
       4 12
       (if bad
           (get-dialog "crew-gamble" "hint1")
         (get-dialog "crew-gamble" "hint2"))))


  (setq on-dialog-accepted
        (lambda

          (let ((temp (chr-slots (player))))
            (setq temp (get temp (choice (length temp))))

            (if temp
                (progn
                  (chr-del (opponent) 1 14)
                  (if (not bad)
                      (progn
                        (chr-new (player) (car temp) (cdr temp) 'neutral nil)
                        (load-dialog "crew-gamble" "success")
                        (adventure-log-add 40 '())
                        (exit))
                    (progn
                      (chr-new (player) (car temp) (cdr temp) 'hostile nil)
                      (load-dialog "crew-gamble" "raid")
                      (adventure-log-add 41 '())
                      (setq on-dialog-closed
                            (lambda
                              (load-dialog "crew-gamble" "goblin-taunt")
                              (setq on-dialog-closed '()))))))
              (progn
                (load-dialog "crew-gamble" "no-room")
                (exit)))))))



(setq on-dialog-declined
      (lambda
        ;; TODO...
        (exit)))

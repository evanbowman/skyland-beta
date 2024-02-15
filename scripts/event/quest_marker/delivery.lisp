
(load-dialog "market-quest" "dest-intro")



(opponent-init 6 'neutral)

(island-configure
 (opponent)
 '((power-core 3 13)
   (coconut-palm 5 13)))


(setq on-converge
      (lambda
        (let ((c (cargo-bays (player))))
          (let ((p (filter
                    (lambda
                      (equal
                       "parcel"
                       (cargo (player) (car $0) (cdr $0))))
                    c)))
            (if p
                (let ((temp (+ 2500 (choice 2000))))
                  ;; Clear out cargo
                  (cargo-set
                   (player)
                   (caar p)
                   (cdr (car p))
                   "")

                  (load-dialog "market-quest" "thanks" temp)

                  (coins-add temp)
                  (adventure-log-add 22 (list temp))

                  (setq on-dialog-closed exit))
              (progn
                (setq on-dialog-closed exit)
                (load-dialog "market-quest" "failed")))))))

;;;
;;; neutral/0/3.lisp
;;;


(load-dialog "zone1" "mc_intro")


(opponent-init 5 'neutral)


(island-configure
 (opponent)
 '((power-core 3 13)
   (hull 0 14)))

(chr-new (opponent) 1 14 'neutral 0)
(chr-new (opponent) 2 14 'neutral 0)


(flag-show (opponent) 6)


(let ((item (sample '(arc-gun flak-gun fire-charge)))
      (skip 1))

  (terrain-set (opponent) (+ (terrain (opponent)) (* 2 (car (rinfo 'size item)))))
  (room-new (opponent) (list item 5 14))
  (room-new (opponent) (list item 5 13))
  (room-new (opponent) (list item 5 12))
  (room-new (opponent) (list item 5 11))

  (room-new (opponent) (list item (+ 5 (car (rinfo 'size item))) 14))
  (room-new (opponent) (list item (+ 5 (car (rinfo 'size item))) 13))
  (room-new (opponent) (list item (+ 5 (car (rinfo 'size item))) 12))
  (room-new (opponent) (list item (+ 5 (car (rinfo 'size item))) 11))

  (setq on-converge
        (lambda
          (dialog (format (get-dialog "zone1" "mc_offer1") (rinfo 'name item))
                  (if (< (coins) 1300)
                      (get-dialog "zone1" "mc_retry1")
                    (get-dialog "zone1" "mc_offer_end")))
          (dialog-await-y/n)
          (setq on-converge nil)))



  (setq on-dialog-accepted
        (lambda
          (if (bound? 'fut) (unbind 'fut))

          (if (< (coins) 1300)
              (progn
                ;; Capture the current executing function, reinvoke after n seconds...
                (let ((f (this)))
                  (defn fut
                    (if (> (coins) 1299)
                        (progn
                          (load-dialog "zone1" "mc_retry_done")
                          (setq on-dialog-closed f))
                      (f))))

                (if skip
                    (progn
                      (setq skip 0)
                      (on-timeout 15000 'fut))
                  (progn
                    (load-dialog "zone1" "mc_retry2")
                    (dialog-await-y/n)
                    (setq on-dialog-accepted (lambda (on-timeout 15000 'fut)))
                    (setq on-dialog-declined (lambda (unbind 'fut) (exit))))))
            (progn
              (adventure-log-add 10 (list (rinfo 'name item) 1300))
              (coins-add -1300)
              (alloc-space item)
              (sel-input
               item
               (format (get-dialog "zone1" "mc_place1")
                       (symbol (rinfo 'name item))
                       (car (rinfo 'size item)) (cdr (rinfo 'size item)))
               (lambda
                 (room-new (player) (list item $1 $2))
                 (sound "build0")
                 (alloc-space item)
                 (sel-input
                  item
                  (format (get-dialog "zone1" "mc_place2")
                          (symbol (rinfo 'name item)))
                  (lambda
                    (room-new (player) (list item $1 $2))
                    (sound "build0")
                    (load-dialog "zone1" "mc_done")
                    (setq on-dialog-closed exit))))))))))

(gc) ;; just in case, no harm in running it.



(setq on-dialog-declined exit)

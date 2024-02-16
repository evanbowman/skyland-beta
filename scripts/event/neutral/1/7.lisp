;;;
;;; neutral/1/7.lisp
;;;


(lc-dialog-load "engineer" "intro")


(opponent-init 7 'neutral)

(island-configure
 (opponent)
 '((stacked-hull 0 14)
   (manufactory 1 10)
   (stacked-hull 1 9)
   (stacked-hull 2 9)
   (stacked-hull 2 14)
   (stacked-hull 3 14)
   (stacked-hull 3 12)
   (stacked-hull 3 13)
   (stacked-hull 3 9)
   (stacked-hull 4 11)
   (stacked-hull 4 12)
   (stacked-hull 4 9)
   (stacked-hull 4 10)
   (power-core 4 13)
   (stacked-hull 5 12)
   (stacked-hull 6 14)
   (stacked-hull 6 12)
   (stacked-hull 6 13)
   (stacked-hull 6 10)
   (stacked-hull 6 11)))


(defn on-converge [0]
  (let ((r (filter (lambda
                     (or (equal (get $0 0) 'hull)
                         (equal (get $0 0) 'bronze-hull)))
                   (rooms (player)))))
    (let ((cost (* (length r) 160)))
      (if (length r)
          (progn
            (lc-dialog-load-fmt "engineer" "offer" cost)
            (dialog-await-y/n)
            (defn on-dialog-accepted [0]
              (if (< (coins) cost)
                  (progn
                    (dialog (lc-dialog-get "engineer" "low-funds") (string (coins) " " cost))
                    (exit))
                (progn
                  (coins-add (* -1 cost))
                  (sound "build0")
                  (map
                   (lambda
                     (room-mut (player) (get $0 1) (get $0 2) 'stacked-hull))
                   r)
                  (adventure-log-add 37 '())
                  (lc-dialog-load "engineer" "success")
                  (exit)))))
        (progn
          (lc-dialog-load "engineer" "fail")
          (exit))
        (setq on-converge nil)))))


(defn on-dialog-declined [0]
  (lc-dialog-load "engineer" "decline")
  (exit))

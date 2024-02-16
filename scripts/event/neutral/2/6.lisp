;;;
;;; neutral/2/6.lisp
;;;


(lc-dialog-load "burning-isle" "intro")


(opponent-init 7 'neutral)

(island-configure
 (opponent)
 '((incinerator 0 13)
   (hull 2 14)
   (power-core 3 13)
   (hull 3 12)
   (hull 5 14)
   (torch 2 10)
   (torch 3 9)
   (torch 5 11)))

(defn on-fadein [0]
  (map (lambda
         (if (equal (get $0 0) 'torch)
             (fire-new (opponent) (get $0 1) (get $0 2))))
       (rooms (opponent)))
  (setq on-fadein nil))

;; (flag-show (opponent) 1)


(defn on-converge [0]
  (lc-dialog-load "burning-isle" "award")
  (setq on-converge nil)
  (alloc-space 'incinerator)
  (adventure-log-add 46 '())
  (sel-input 'incinerator
             (lc-dialog-get "burning-isle" "place")
             (lambda
               (room-new (player) (list 'incinerator $1 $2))
                 (room-del (opponent) 0 13)
               (sound "build0")
               (lc-dialog-load "burning-isle" "done")
               (run-util-script "pickup_cart.lisp"
                                7
                                (lc-dialog-get "burning-isle" "cart")))))

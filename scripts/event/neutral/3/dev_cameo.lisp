;;;
;;; neutral/3/dev_cameo.lisp
;;;


(load-dialog "dev-cameo" "intro")


(opponent-init 7 'neutral)


(island-configure
 (opponent)
 '((bronze-hull 0 14)
   (sunflower 0 13)
   (ice 1 14)
   (windmill 2 13)
   (masonry 2 10 3)
   (masonry 2 11 3)
   (masonry 2 12 3)
   (masonry 2 14 3)
   (masonry 2 9 3)
   (masonry 3 12 3)
   (masonry 3 11 3)
   (masonry 3 14 3)
   (masonry 3 13 3)
   (masonry 3 10 3)
   (power-core 3 8)
   (power-core 4 11)
   (masonry 4 13 3)
   (masonry 4 14 3)
   (bronze-hull 4 10)
   (ice 5 14)
   (bronze-hull 6 14)))

(flag-show (opponent) 4)


(setq on-converge
      (lambda
        (let ((info (cart-info 0)))
          (load-dialog "dev-cameo" "meet")
          (setq on-dialog-closed
                (lambda
                  (sound "click_digital_1")
                  (cart-add 0)
                  (dialog (get-dialog "dev-cameo" "pickup1")
                          (get info 0)
                          (get-dialog "dev-cameo" "pickup2"))
                  (setq on-dialog-closed nil)
                  (exit))))))

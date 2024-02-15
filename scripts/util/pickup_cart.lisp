
;; args: (cart-id dialog-string)
;; sets on-dialog-closed, beginning a dialog chain
(lambda
  (let ((n $0)
        (str $1))
    (setq on-dialog-closed
          (if (cart-found? n)
              exit
            (lambda
              (dialog str)
              (setq on-dialog-closed
                    (lambda
                      (sound "click_digital_1")
                      (cart-add n)
                      (dialog (get-dialog "cart" "pickup1")
                              (car (cart-info n))
                              (format (get-dialog "cart" "pickup2") (+ n 1)))
                      (setq on-dialog-closed
                            (if (save-bit-load 8)
                                exit
                              (lambda
                                (load-dialog "cart" "help")
                                (save-bit-store 8 1)
                                (setq on-dialog-closed exit)))))))))))


(lc-dialog-load "surrender" "plead")

(defn on-dialog-closed [0]
    (setq on-dialog-closed '())
  (let ((c (+ (/ (coins-victory) 2) (/ (coins-victory) 6))))
    (lc-dialog-load "surrender" "offer")

    (dialog-opts-reset)
    (dialog-opts-push (format (lc-dialog-get "surrender" "opt-crew") c)
                      (lambda
                          (coins-add c)
                        (let ((g (chrs (opponent)))
                              (ss (chr-slots (player))))

                          (unless ss
                            (alloc-space 'ladder)

                            (let ((s (construction-sites (player) '(1 . 2))))
                              (room-new (player) (list 'ladder (caar s) (cdr (car s))))
                              (setq ss (chr-slots (player)))))

                          (if g
                              (let ((s (get ss (choice (length ss)))))
                                (chr-del (opponent)
                                         (caar g)
                                         (cdr (car g)))
                                (chr-new (player)
                                         (car s)
                                         (cdr s)
                                         'neutral
                                         '((race . 1)))
                                (adventure-log-add 51 '())
                                (lc-dialog-load "surrender" "add-crew")
                                (run-util-script "pickup_cart.lisp"
                                                 8
                                                 (lc-dialog-get "surrender" "cart")))))
                        (exit 2)))

    (let ((cnt 0)
          (tot (/ (length (rooms (opponent))) 5)))
      (setq cnt tot)
      (when cnt
        (dialog-opts-push
         (format (lc-dialog-get "surrender" "opt-salvage") cnt)
         (lambda
           (let ((rtry (this)))
             (sel-input-opponent
              nil
              (format (lc-dialog-get "surrender" "take") (- tot cnt) tot)
              (lambda
                (let ((took (car (room-load (opponent) $1 $2))))
                  (if (room-is-critical (opponent) $1 $2)
                      (progn
                        (lc-dialog-load "surrender" "salvage-failed")
                        (if (equal 1 (length (rooms (opponent))))
                            (exit 2)
                            (setq on-dialog-closed rtry)))
                      (progn
                        (room-del (opponent) $1 $2)
                        (sound "gravel")
                        (alloc-space took)
                        (sel-input
                         took
                         (format (lc-dialog-get "surrender" "place") (- tot cnt) tot)
                         (lambda
                           (room-new (player) (list took $1 $2))
                           (sound "build0")
                           (setq cnt (- cnt 1))
                           (if (equal cnt 0)
                               (progn
                                 (dialog (format (lc-dialog-get "surrender" "done") tot))
                                 (adventure-log-add 62 '())
                                 (setq on-dialog-closed (curry exit 2)))
                               (rtry))))))))))))))


    (let ((rtry (this)))
      (dialog-opts-push (lc-dialog-get "surrender" "opt-help")
                        (lambda
                          (lc-dialog-load-fmt "surrender" "help" (coins-victory))
                          (setq on-dialog-closed rtry))))


    (dialog-opts-push (lc-dialog-get "surrender" "opt-refuse") (lambda nil))))

(gc)

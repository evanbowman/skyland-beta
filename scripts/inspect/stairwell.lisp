;;;
;;; scripts/inspect/stairwell.lisp
;;;

(tr-bind-current)

(lambda (isle x y)
  (let ((info (room-load (player) x y)))
    (when info
      (let ((sz (rinfo 'size (car info)))
            (y-start (get info 2)))
        (if (remove (map (lambda (y2)
                           (chr-find x y2))
                         (range y-start (+ y-start (cdr sz))))
                    nil)
            (dialog (tr "The stairwell bustles with activity..."))
            (dialog (tr "The stairwell is quiet...")))))))

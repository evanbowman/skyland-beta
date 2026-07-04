;;;
;;; scripts/inspect/power-core.lisp
;;;

(tr-bind-current)

(lambda (isle x y)
  (dialog (tr "The core hums steadily: <a:WAVE>mmmmmmmmmmmmmmmmmm <B:0> Warmth radiates evenly from the housing...")
          (let ((hp (room-hp isle x y)))
            (cond
              ((> hp 30)
               (tr " "))
              (true
               (tr "<B:0> This power supply could use some maintenance."))))))

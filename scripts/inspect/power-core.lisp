;;;
;;; scripts/inspect/power-core.lisp
;;;

(tr-bind-current)

(lambda (isle x y)
  (dialog (tr "The core hums steadily: <a:WAVE>mmmmmmmmmmmmmmmmmm <B:0> Warmth radiates evenly from the housing, the vibration steady... <B:0>")
          (let ((hp (room-hp isle x y)))
            (cond
              ((> hp 30)
               (tr "This power supply is in good working order."))
              (true
               (tr "This power supply could use some maintenance."))))))

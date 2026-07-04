;;;
;;; /scripts/inspect/forcefield.lisp
;;;

(tr-bind-current)

(lambda (isle x y)
  (case (weather)
    (weather-id-rain
     (dialog (tr "Tendrils of hissing steam rise above the forcefield as rain falls on its surface...")))
    (else
     (dialog (tr "The forcefield glows with electricity...")))))

;;;
;;; /scripts/inspect/cargo-bay.lisp
;;;

(tr-bind-current)

(lambda (isle x y)
  (if-let ((item (cargo isle x y)))
      (dialog (tr (format "A cargo bay! It contains: '%'" item)))
    (dialog (tr "A cargo bay! It's empty..."))))

;;;
;;; wildcard.lisp
;;;
;;; Certain block names are not possible to represent as files in various
;;; operating systems.
;;;

(tr-bind-current)

(lambda (isle x y room-sym)
  (let ((sub '((forcefield* . forcefield)
               (stairwell++ . stairwell)
               (stairwell+ . stairwell)
               (ladder+ . stairwell))))
    (let ((match (lookup room-sym sub)))
      (if match
          ((eval-file (format "scripts/inspect/%.lisp" match) isle x y))
          (case (rinfo 'category room-sym)
            ('weapon
             (if (room-target-get isle x y)
                 (dialog (tr "The weapon is charging..."))
                 (dialog (tr "The weapon sits idle..."))))
            (else
             (dialog (tr "There's nothing remarkable here..."))))))))

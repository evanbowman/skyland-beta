;;;
;;; inspect.lisp
;;;
;;; The game invokes the result of this script when the player picks the inspect
;;; option from the select menu.
;;;

(tr-bind-current)

(lambda (isle x y)
  (if-let ((info (room-load isle x y)))
      (let ((path (format "/scripts/inspect/%.lisp" (car info))))
        (if (file-exists? path)
            ((eval-file path) isle x y)
            ((eval-file "/scripts/inspect/wildcard.lisp") isle x y (car info))))
    (dialog (tr "There's nothing remarkable here..."))))

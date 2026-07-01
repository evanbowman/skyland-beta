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
        (dialog-opts-reset)
        (dialog-opts-push (rinfo 'name (car info))
                          (lambda ()
                            (if (file-exists? path)
                                ((eval-file path) isle x y)
                                ((eval-file "/scripts/inspect/wildcard.lisp") isle x y (car info)))))
        (if-let ((chr (chr-find x y)))
            (dialog-opts-push (tr "crewmember")
                              (lambda ()
                                ((eval-file "/scripts/inspect/crew.lisp") chr))))
        (dialog (tr "Inspect what?")))
    (dialog (tr "There's nothing remarkable here..."))))

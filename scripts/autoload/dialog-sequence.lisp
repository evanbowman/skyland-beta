;;;
;;; autoload/dialog-sequence.lisp
;;;

(lambda steps
  (foreach (lambda (step)
             (cond ((lambda? step) (step))
                   ((string? step) (await (dialog* step)))))
           steps))

;;;
;;; uncharted.lisp
;;;
;;; Some unexpected, randomly occurring, events.
;;;


(breakpoint)

(case (wg-current-type)
  (wg-id-neutral/uncharted
   (eval-file "scripts/event/neutral.lisp"))

  (wg-id-hostile/uncharted
   (eval-file "scripts/event/hostile.lisp")
   (let ((prob (get '(26 10 8) (difficulty)))          ; Chance of surprise event occurring.
         (lvs (difference surprises-seen (range surprise-count))))

     (if (and lvs
              (not (has-dialog?))
              (equal (choice prob) 0))
         (let ((lv (sample lvs)))
           (setq surprises-seen (cons lv surprises-seen))
           (procgen)
           (eval-file (format "scripts/event/surprise/%.lisp" lv))))))

  (else
   (fatal (format "invalid uncharted node %"
                  (wg-current-type)))))

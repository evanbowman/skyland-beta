;;;
;;; neutral/1/8.lisp
;;;


(dialog "You discover the ruins of a forsaken town. I guess no one would mind if you scavenged some resources...")



(opponent-init 9 'neutral)


(island-configure
 (opponent)
 '((power-core 0 13)
   (manufactory 0 11)
   (masonry 2 14 0)
   (masonry 2 13 0)
   (water-source 3 14)
   (bridge 3 12)
   (masonry 4 14 0)
   (masonry 4 13 0)
   (bridge 5 12)
   (water-source 5 14)
   (power-core 6 9)
   (bronze-hull 6 11)
   (masonry 6 13 0)
   (masonry 6 14 0)
   (workshop 7 11)
   (masonry 7 13 0)
   (water-source 7 14)
   (masonry 8 13 0)
   (masonry 8 14 0)))


(defn on-converge
  (let ((c (choice 2))
        (end (lambda
               ((eval-file "/scripts/util/pickup_cart.lisp") 4
         "Something else catches your attention.<d:500>.<d:500>.<d:500> a data cartridge!"))))
    (cond
     ((equal c 0)
      (let ((amt (+ 200 (choice 400))))
        (coins-add amt)
        (dialog
         (format
          "Looks like someone already got here first. You collect %@."
          amt))
        (end)))
     ((equal c 1)
      (let ((opts '(manufactory
                    backup-core
                    incinerator
                    beam-gun)))
        (let ((pick (sample opts)))
          (dialog
           "After boarding, you find a completely intact "
           (rname pick)
           ". Your crew asks you where to install it...")
          (defn on-dialog-closed
            (setq on-dialog-closed nil)
            (while (not (construction-sites (player) (rsz pick)))
              (terrain (player) (+ (terrain (player)) 1)))
            (sel-input
             (rsz pick)
             (format "Pick a slot (%x%)"
                     (car (rsz pick))
                     (cdr (rsz pick)))
             (lambda
               (room-new (player) `(,pick ,$1 ,$2))
               (syscall "sound" "build0")
               (dialog "All done!")
               (end))))))))))
;;;
;;; neutral/1/6.lisp
;;;


(opponent-init 11 'neutral)

(island-configure
 (opponent)
 '((windmill 0 9)
   (bronze-hull 0 13)
   (bronze-hull 0 12)
   (bronze-hull 0 14)
   (shrubbery 0 11)
   (hull 1 14)
   (workshop 1 7)
   (stairwell 1 9)
   (masonry 1 13 0)
   (bronze-hull 2 10)
   (masonry 2 14 0)
   (masonry 2 13 0)
   (bronze-hull 2 9)
   (bulkhead-door 2 11)
   (water-source 3 14)
   (bridge 3 12)
   (workshop 3 7)
   (water-source 4 14)
   (backup-core 5 11)
   (water-source 5 14)
   (sunflower 6 8)
   (masonry 6 9 3)
   (water-source 6 14)
   (masonry 6 10 3)
   (masonry 7 10 3)
   (lemon-tree 7 7)
   (masonry 7 12 3)
   (masonry 7 11 3)
   (masonry 7 9 3)
   (water-source 7 14)
   (water-source 8 14)
   (workshop 8 11)
   (masonry 8 10 3)
   (masonry 8 9 3)
   (workshop 8 7)
   (water-source 9 14)
   (bronze-hull 10 14)
   (banana-plant 10 13)))


(flag-show (opponent) 7)


(let ((pc (filter (lambda (equal (car $0) 'power-core)) (rooms (player))))
      (rc (filter (lambda (equal (car $0) 'reactor)) (rooms (player))))
      (sc (filter (lambda (equal (car $0) 'backup-core)) (rooms (player))))
      (wpn (sample '(flak-gun fire-charge))))

  (when pc
    (secret 1 14 (string "Notice: surplus " wpn " in stock!")))

  (if (or sc (and (not rc) (not pc))) ;; player must have a core and not already have a backup
      (progn
        (defn on-converge [0]
          (load-dialog "village" "intro2")
          (exit)))
    (progn
      (load-dialog "village" "intro1")
      (defn on-converge [0]

        (setq on-converge nil)

        ;; In case anything changed...
        (setq pc (filter (lambda (equal (car $0) 'power-core)) (rooms (player))))
        (setq rc (filter (lambda (equal (car $0) 'reactor)) (rooms (player))))

        (load-dialog "village" "offer")
        (dialog-await-binary-q (get-dialog "village" "opt1") (get-dialog "village" "opt2"))

        (setq on-dialog-declined exit)

        (defn on-dialog-accepted [0]
          (let ((c nil))
            (if pc
                (setq c (car pc))
              (progn
                ;; The player has no powercore, but is instead donating a
                ;; reactor. Give a potentially rare weapon!
                (setq c (car rc))
                (setq wpn (sample '(flak-gun
                                    annihilator
                                    decimator
                                    rocket-bomb
                                    warhead
                                    incinerator)))))

            (room-mut (player) (get c 1) (get c 2) 'backup-core)
            (room-mut (opponent) 5 11 'power-core)

            (let ((mkch
                   (lambda
                     (if (not (chr-slots (player)))
                         (let ((c (construction-sites (player) '(1 . 2))))
                           (if c
                               (room-new (player) `(ladder ,(caar c) ,(cdr (car c)))))))

                     (let ((c (chr-slots (player))))
                       (chr-new (player)
                                (caar c)
                                (cdr (car c))
                                'neutral
                                nil)))))
              (mkch)
              (mkch)
              (mkch))

            (alloc-space wpn)

            (adventure-log-add 36 (list (rinfo 'name wpn) 3))

            (let ((impl
                   (lambda
                     (let ((cb $0))

                       (sel-input wpn
                                  (string (get-dialog "village" "place") (rinfo 'name wpn)
                                          (format " (%x%):"
                                                  (car (rinfo 'size wpn))
                                                  (cdr (rinfo 'size wpn))))
                                  (lambda
                                    (room-new (player) `(,wpn ,$1 ,$2))
                                    (sound "build0")
                                    (cb)))))))
              (impl
               (lambda
                 (impl
                  (lambda
                    (load-dialog "village" "thanks")
                    ((eval-file "/scripts/util/pickup_cart.lisp") 3
                     (get-dialog "village" "cart")))))))))))))


(dialog "A distress call sounds over your radio! The remnants of a town appear, wrecked by war...")

(opponent-init 8 'neutral)

(island-configure
 (opponent)
 '((hull 0 14)
   (hull 0 13)
   (torch 0 10)
   (masonry 1 14)
   (masonry 1 13)
   (masonry 2 14)
   (plundered-room 2 12)
   (torch 3 9)
   (masonry 3 14)
   (plundered-room 3 12)
   (masonry 4 14)
   (power-core 5 13)))

(defn on-fadein
  (fire-new (opponent) 3 9)
  (fire-new (opponent) 0 10)
  (setq on-fadein nil))


(chr-new (opponent) 1 12 'neutral 0)

(setq on-converge
      (lambda
        (dialog
         "<c:girl:14>Heya! I'm so lucky someone showed up! Damned goblins took my whole village as hostages. Somehow I slept through the whole thing... Anyway, please take me with you! I promise not to get in the way!")

        (setq on-dialog-closed
              (lambda
                (dialog "She seems harmless, invite her aboard?")
                (dialog-await-y/n)
                (setq on-dialog-closed '())))

        (setq on-converge nil)))


(setq on-dialog-accepted
      (lambda
        (let ((temp (chr-slots (player)))
              (end (lambda
                     ((eval-file "/scripts/util/pickup_cart.lisp") 2
                      "<c:girl:14>.<d:500>.<d:500>.<d:500> Actually, I was wondering if you can do me one more small favor? I brought this data cartridge with an old photo of my village, can you hold onto it for me?"))))
          (if temp
              (progn
                (setq temp (get temp (choice (length temp))))
                (chr-new (player) (car temp) (cdr temp) 'neutral 0)
                (chr-del (opponent) 1 12)
                (dialog "The villager girl joined your crew!")
                (end))
            (progn
              (dialog "Sadly, there's no room...")
              (defn on-dialog-closed
                (dialog "<c:girl:14>Wait up a second, I know your castle's pretty full, but don't leave me here! This island is literally burning! I'll even sleep in a cargo bay...")
                (defn on-dialog-closed
                  (while (< (length (construction-sites (player) '(1 . 2))) 1)
                    (terrain (player) (+ (terrain (player)) 1)))
                  (sel-input '(1 . 2)
                             "Place cargo bay (1x2):"
                             (lambda
                               (syscall "sound" "build0")
                               (room-new (player) `(cargo-bay ,$1 ,$2))
                               (chr-del (opponent) 1 12)
                               (chr-new (player) $1 (+ 1 $2) 'neutral 0)
                               (dialog "<c:girl:14>Wait, you're serious! I guess I asked for it haha...")
                               (defn on-dialog-closed
                                 (dialog "The villager girl joined your crew!")
                                 (end)))))))))

        (exit)))


(setq on-dialog-declined
      (lambda
        (exit)))
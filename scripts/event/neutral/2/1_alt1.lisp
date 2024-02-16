
(lc-dialog-load "crew-gamble2" "intro")


(opponent-init 8 'neutral)


(island-configure
 (opponent)
 '((hull 0 12 56)
   (hull 0 11)
   (hull 0 13)
   (hull 0 14 16)
   (hull 1 11)
   (hull 1 12)
   (hull 1 14 72)
   (hull 1 13)
   (hull 2 14 200)
   (hull 3 10)
   (hull 3 14)
   (transporter 3 12)
   (hull 4 9)
   (reactor 4 11)
   (hull 4 10)
   (masonry 4 14 3)
   (hull 5 10)
   (masonry 5 14 3)
   (hull 6 14)
   (ladder 6 12)
   (infirmary 6 10)
   (hull 6 8)
   (hull 7 14)
   (hull 7 12)
   (windmill 7 13)
   (hull 7 9)
   (hull 7 8)))


(map (lambda (chr-new (opponent) (car $0) (cdr $0) 'neutral 0))
     '((0 . 10) (1 . 10) (6 . 7)))


(defn on-converge [0]
  (lc-dialog-load "crew-gamble2" "offer")
  (dialog-await-y/n)
  (setq on-converge nil))


(let ((bad (choice 2))
      (move (lambda
              (let ((ch '((0 . 10) (1 . 10) (6 . 7))))
                (while (and ch (chr-slots (player)))
                  (let ((sl (chr-slots (player))))
                    (chr-del (opponent) (caar ch) (cdar ch))
                    (setq ch (cdr ch))
                    (chr-new (player) (caar sl) (cdar sl) $0 0)))
                (while (and ch (chr-slots (opponent)))
                  (let ((sl (chr-slots (opponent))))
                    (chr-del (opponent) (caar ch) (cdar ch))
                    (setq ch (cdr ch))
                    (chr-new (opponent) (caar sl) (cdar sl) $0 0)))))))

  (defn on-dialog-accepted [0]
    (cond
     (bad
      (move 'hostile)
      (chr-new (opponent) 4 13 'hostile 0)
      (chr-new (opponent) 5 13 'hostile 0)
      (room-new (opponent) '(mycelium 3 11))
      (room-new (opponent) '(mycelium 6 9))
      (room-mut (opponent) 0 12 'arc-gun)
      (room-mut (opponent) 0 14 'arc-gun)
      (map (lambda
             (if (equal (car $0) 'hull)
                 (room-mut (opponent) (get $0 1) (get $0 2) 'mirror-hull)))
           (rooms (opponent)))
      (lc-dialog-load "crew-gamble2" "trap")
      (defn on-dialog-closed [0]
        (lc-dialog-load "crew-gamble2" "goblin-taunt")
        (setq on-dialog-closed nil))
      (opponent-mode 'hostile))
     (true
      (move 'neutral)
      (if (chrs (opponent))
          (lc-dialog-load "crew-gamble2" "join")
        (lc-dialog-load "crew-gamble2" "join2"))
      (defn on-dialog-closed [0]
        (dialog "...")
        (setq on-dialog-closed exit))))))


(setq on-dialog-declined exit)

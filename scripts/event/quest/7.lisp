
(lc-dialog-load "mining-quest" "intro")


(opponent-init 12 'neutral)

(island-configure
 (opponent)
 '((hull 0 14)
   (hull 0 13)
   (hull 1 14)
   (power-core 1 10)
   (ladder 1 12)
   (dynamite-ii 2 13)
   (dynamite-ii 2 12)
   (dynamite-ii 2 14)
   (dynamite-ii 3 14)
   (masonry 3 11 0)
   (dynamite-ii 3 12)
   (bridge 3 10)
   (dynamite-ii 3 13)
   (hull 4 9)
   (dynamite-ii 4 12)
   (masonry 4 11 0)
   (dynamite-ii 4 14)
   (dynamite-ii 4 13)
   (masonry 5 11 0)
   (dynamite-ii 5 14)
   (bridge 5 10)
   (dynamite-ii 5 12)
   (hull 5 9)
   (dynamite-ii 5 13)
   (dynamite-ii 6 12)
   (masonry 6 11 0)
   (dynamite-ii 6 13)
   (dynamite-ii 6 14)
   (bridge 7 10)
   (dynamite-ii 7 13)
   (dynamite-ii 7 14)
   (masonry 7 11 0)
   (dynamite-ii 7 12)
   (hull 8 9)
   (dynamite-ii 8 12)
   (dynamite-ii 8 14)
   (dynamite-ii 8 13)
   (masonry 8 11 0)
   (crane 9 8)
   (dynamite-ii 9 12)
   (dynamite-ii 9 14)
   (masonry 9 11 0)
   (bridge 9 10)
   (dynamite-ii 9 13)
   (hull 10 14)
   (hull 10 7)
   (masonry 10 13 0)
   (masonry 10 11 0)
   (masonry 10 12 0)
   (hull 11 14)
   (hull 11 13)))


(defn on-converge [0]
  (setq on-converge nil)
  (lc-dialog-load "mining-quest" "greet")
  (defn on-dialog-closed [0]
    (lc-dialog-load "mining-quest" "offer")
    (dialog-await-y/n))

  (defn on-dialog-accepted [0]
    (let ((m (eval-file "/scripts/event/quest/make_quest_marker.lisp"))
          (bloc (if (equal (difficulty) 0)
                    'dynamite
                  'dynamite-ii)))
      (if m
          (progn
            ;; push adventure log
            (push 'qids 7)
            (push 'quests (cons "/scripts/event/quest_marker/dynamite-ii.lisp" m))
            (lc-dialog-load "mining-quest" "cargo")
            (adventure-log-add 57 nil)

            (defn on-dialog-closed [0]
              (let ((cnt 0)) ; closure
                ((lambda
                   (let ((t (this))
                         (del 0))
                     (alloc-space bloc)
                     (sel-input
                      bloc
                      (string (lc-dialog-get "mining-quest" "place") (+ cnt 1) "/7:")
                      (lambda
                        (room-new (player) (list bloc $1 $2))
                        (sound "build0")

                        (room-del (opponent) (+ cnt 2) 12)

                        (+= cnt 1)

                        (if (equal cnt 7)
                            (progn
                              (lc-dialog-load "mining-quest" "instructions")
                              (setq on-dialog-closed exit))
                          (t))))))))))
        (progn
          (lc-dialog-load "mining-quest" "skip")
          (exit)))))

  (setq on-dialog-declined exit))

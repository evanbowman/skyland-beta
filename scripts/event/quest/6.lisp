
(lc-dialog-load "sylph-quest" "intro")


(opponent-init 9 'neutral)

(island-configure
 (opponent)
 '((masonry 0 13 0) (masonry 0 12 0) (masonry 0 14 2) (masonry 1 14 0) (masonry 1 13 2) (sunflower 3 13) (masonry 3 14 0) (windmill 4 10) (masonry 4 12 3) (masonry 4 14 3) (masonry 4 13 3) (masonry 4 11 3) (masonry 5 14 0) (masonry 5 13 3) (masonry 5 12 3) (masonry 5 11 3) (masonry 5 10 3) (workshop 6 13) (masonry 6 12 2) (bronze-hull 8 14)))



(defn on-converge [0]
  (lc-dialog-load "sylph-quest" "greet")

  (defn on-dialog-closed [0]
    (lc-dialog-load "sylph-quest" "boy-speek")

    (defn on-dialog-closed [0]
      (lc-dialog-load "sylph-quest" "offer")

      (setq on-dialog-closed nil)

      (dialog-await-y/n)

      (defn on-dialog-accepted [0]

        (let ((sl (chr-slots (player))))
          (when (not sl)
            (alloc-space 'ladder)
            (let ((site (construction-sites (player) '(1 . 2))))
              (sound "build0")
              (room-new (player) `(ladder ,(caar site) ,(cdar site)))))

          (setq sl (chr-slots (player)))
          (let ((id (chr-new (player)
                             (caar sl)
                             (cdar sl)
                             'neutral
                             '((icon . 26)))))
            (chr-hp id 128)
            (let ((m (eval-file "/scripts/event/quest/make_quest_marker.lisp")))
              (if m
                  (progn
                    (adventure-log-add 54 nil)
                    (push 'qids 6)
                    (push 'quests (cons "/scripts/event/quest_marker/civ.lisp" m))
                    (push 'qvar (cons 6 id))
                    (lc-dialog-load "sylph-quest" "join1")
                    (exit))
                (progn
                  (lc-dialog-load "sylph-quest" "join2")
                  (exit)))))))

      (setq on-dialog-declined exit))))

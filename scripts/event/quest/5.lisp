
(lc-dialog-load "wanderer-quest" "intro")


(opponent-init 14 'neutral)

(island-configure
 (opponent)
 '((power-core 0 12)
   (masonry 0 14 3)
   (workshop 1 6)
   (masonry 1 14 3)
   (masonry 2 13 0)
   (masonry 2 8 3)
   (masonry 2 14 3)
   (workshop 3 5)
   (masonry 3 8 3)
   (masonry 3 13 2)
   (masonry 3 10 3)
   (masonry 3 14 3)
   (windmill 3 9)
   (masonry 3 7 3)
   (workshop 3 11)
   (masonry 4 14 3)
   (masonry 4 13 3)
   (masonry 5 14 0)
   (bridge 5 12)
   (shrubbery 5 10)
   (hull 5 11)
   (masonry 5 13 0)
   (bridge 5 6)
   (masonry 6 14 0)
   (lemon-tree 7 9)
   (bridge 7 12)
   (bridge 7 6)
   (masonry 7 14 3)
   (hull 7 11)
   (masonry 7 13 3)
   (bridge 9 6)
   (hull 9 11)
   (masonry 9 14 0)
   (statue 9 9 3)
   (bridge 9 12)
   (hull 10 11)
   (shrubbery 10 10)
   (masonry 10 14 0)
   (masonry 10 13 0)
   (workshop 11 5)
   (transporter 11 11)
   (masonry 11 14 3)
   (masonry 11 13 0)
   (masonry 12 10 3)
   (masonry 12 9 3)
   (masonry 12 8 3)
   (masonry 12 7 3)
   (transporter 12 11)
   (masonry 12 14 3)
   (masonry 12 13 0)
   (masonry 13 14 3)
   (masonry 13 13 0)
   (stairwell 13 9)
   (stairwell 13 5)))


(defn on-converge [0]
  (setq on-converge nil)

  (lc-dialog-load "wanderer-quest" "offer")

  (dialog-await-binary-q (lc-dialog-get "wanderer-quest" "opt1")
                         (lc-dialog-get "wanderer-quest" "opt2"))

  (defn on-dialog-accepted [0]
    (let ((sl (chr-slots (player))))
      (when (not sl)
        (alloc-space 'ladder)

        (let ((site (construction-sites (player) '(1 . 2))))
          (sound "build0")
          (room-new (player) (list 'ladder (caar site) (cdr (car site))))))

      (setq sl (chr-slots (player)))
      (let ((id (chr-new (player)
                         (caar sl)
                         (cdr (car sl))
                         'neutral
                         '((icon . 23)))))

        (let ((m (eval-file "/scripts/event/quest/make_quest_marker.lisp")))
          (if m
              (progn
                (push 'qids 5)
                (adventure-log-add 52 '())
                (push 'quests (cons "/scripts/event/quest_marker/traveller.lisp" m))
                (lc-dialog-load "wanderer-quest" "accepted")
                (defn on-dialog-closed [0]
                  (lc-dialog-load "wanderer-quest" "join")
                  (setq on-dialog-closed exit)))
            (progn
              (lc-dialog-load "wanderer-quest" "skip")
              (defn on-dialog-closed [0]
                (lc-dialog-load "wanderer-quest" "join")
                (setq on-dialog-closed exit))))))))

  (defn on-dialog-declined [0]
    (exit)))

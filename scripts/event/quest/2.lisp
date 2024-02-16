
(lc-dialog-load "lemon-quest" "intro")


(opponent-init 8 'neutral)

(island-configure
 (opponent)
 '((bronze-hull 0 13)
   (sunflower 0 11)
   (bronze-hull 0 12)
   (bronze-hull 0 14)
   (power-core 1 13)
   (bronze-hull 1 12)
   (bronze-hull 2 12)
   (bronze-hull 2 11)
   (lemon-tree 2 9)
   (fountain 3 10)
   (lemon-tree 3 13)
   (bronze-hull 3 11)
   (fountain 4 14)
   (lemon-tree 4 9)
   (bronze-hull 4 11)
   (lemon-tree 5 9)
   (lemon-tree 5 13)
   (bronze-hull 5 11)
   (lemon-tree 6 13)
   (bronze-hull 6 11)
   (lemon-tree 6 9)
   (masonry 7 14)
   (masonry 7 13)
   (bronze-hull 7 12)
   (workshop 7 10)
   (masonry 8 14)
   (masonry 8 13)
   (bronze-hull 8 12)))

(secret
 4 11
 (lc-dialog-get "lemon-quest" "secret"))

(defn on-converge [0]
  (let ((m (eval-file "/scripts/event/quest/make_quest_marker.lisp")))
    (if m
        (progn
          (lc-dialog-load "lemon-quest" "offer")

          (defn on-dialog-closed [0]
            (map
             (lambda
               (if (equal (car $0) 'lemon-tree)
                   (room-del (opponent) (get $0 1) (get $0 2))))
             (rooms (opponent)))

            (push 'qids 2)
            (push 'quests (cons "/scripts/event/quest_marker/lemons.lisp" m))

            (let ((reward 0))
              (map
               (lambda
                 ((room-new
                   (player)
                   (list 'lemon-tree (car $0) (cdr $0)))
                  (+= reward 1400)))
               (construction-sites (player) '(1 . 2)))

              (push 'qvar (cons 2 reward)))

            (adventure-log-add 18 (list (rcnt (player) 'lemon-tree)))

            (lc-dialog-load "lemon-quest" "instructions")
            (setq on-dialog-closed exit)))
      (progn
        (lc-dialog-load "lemon-quest" "skip")
        (setq on-dialog-closed exit)))))

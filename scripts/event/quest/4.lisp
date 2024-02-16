
(lc-dialog-load "ceramics-quest" "intro")


(opponent-init 13 'neutral)

(island-configure
 (opponent)
 '((bronze-hull 0 12)
   (bronze-hull 0 11)
   (bronze-hull 0 14)
   (bronze-hull 0 13)
   (bridge 1 12)
   (ladder 1 13)
   (masonry 2 9 0)
   (bronze-hull 2 11)
   (masonry 2 10 1)
   (power-core 2 13)
   (bridge 3 12)
   (masonry 3 11 1)
   (masonry 3 9 0)
   (masonry 3 10 1)
   (workshop 3 7)
   (water-source 4 14)
   (masonry 4 11 1)
   (masonry 4 9 0)
   (masonry 4 10 1)
   (coconut-palm 5 7)
   (bronze-hull 5 11)
   (masonry 5 14 1)
   (bridge 5 12)
   (masonry 5 9 0)
   (masonry 5 13 0)
   (masonry 5 10 1)
   (water-source 6 14)
   (windmill 6 13)
   (workshop 6 9)
   (stairwell 7 11)
   (shrubbery 8 13)
   (masonry 8 14 0)
   (masonry 9 14 0)
   (masonry 10 14 1)
   (hull 10 13)
   (statue 10 11)
   (masonry 11 14 1)
   (coconut-palm 11 12)
   (bronze-hull 12 14)
   (bronze-hull 12 13)
   (bronze-hull 12 12)
   (bronze-hull 12 11)
   (windmill 12 10)))


(flag-show (opponent) 6)


(let ((fee (cond
            ((< (coins) 1000) (coins))
            ((< (coins) 8000) (/ (coins) 2))
            (true 8000)))
      (qid 4))
  (defn on-converge [0]
    (setq on-converge nil)
    (dialog
     (format
      (lc-dialog-get "ceramics-quest" "offer")
      fee
      (* fee 2)))

    (dialog-await-binary-q (lc-dialog-get "ceramics-quest" "opt1")
                           (lc-dialog-get "ceramics-quest" "opt2"))

    (defn on-dialog-accepted [0]
      (let ((m (eval-file "/scripts/event/quest/make_quest_marker.lisp"))
            (c (eval-file "/scripts/util/find_create_cargo_bay.lisp")))
        (if (and m c)
            (progn
              (adventure-log-add 20 (list fee))
              (push 'qids qid)
              (push 'quests (cons "/scripts/event/quest_marker/ceramics.lisp" m))
              (push 'qvar (cons qid fee))
              (coins-set (- (coins) fee))
              (cargo-set (player) (car c) (cdr c) "ceramic tiles")
              (lc-dialog-load "ceramics-quest" "cargo")
              (defn on-dialog-closed [0]
                (lc-dialog-load "ceramics-quest" "exit")
                (exit)
                (setq on-dialog-closed exit)))
          (progn
            (lc-dialog-load "ceramics-quest" "skip")
            (setq on-dialog-closed exit)))))


    (setq on-dialog-declined
          (lambda
            (lc-dialog-load "ceramics-quest" "decline")
            (setq on-dialog-closed exit)))))

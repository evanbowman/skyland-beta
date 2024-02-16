
(lc-dialog-load "banana-quest" "intro")


(opponent-init 8 'neutral)

(island-configure
 (opponent)
 '((bronze-hull 0 13)
   (bronze-hull 0 14)
   (bronze-hull 0 12)
   (banana-plant 1 13)
   (masonry 1 14)
   (banana-plant 2 12)
   (masonry 2 13)
   (masonry 2 14)
   (masonry 3 14)
   (masonry 3 11)
   (power-core 3 9)
   (masonry 3 13)
   (masonry 3 12)
   (masonry 4 14)
   (masonry 4 13)
   (masonry 4 12)
   (masonry 4 11)
   (masonry 5 14)
   (masonry 5 13)
   (masonry 5 12)
   (banana-plant 5 11)
   (masonry 6 14)
   (masonry 7 14)))


(flag-show (opponent) 5)


(secret
 4 13
 (lc-dialog-get "banana-quest" "secret1"))

(secret
 5 14
 (lc-dialog-get "banana-quest" "secret2"))


(setq on-converge
      (lambda
        (lc-dialog-load "banana-quest" "offer")
        (dialog-await-binary-q (lc-dialog-get "banana-quest" "opt1")
                               (lc-dialog-get "banana-quest" "opt2"))

        (setq on-dialog-accepted
              (lambda
                (let ((m (eval-file "/scripts/event/quest/make_quest_marker.lisp")))
                  (setq on-dialog-closed exit)
                  (if m
                      (progn
                        (adventure-log-add 17 '())
                        (push 'qids 1)
                        (push 'quests (cons "/scripts/event/quest_marker/nanas.lisp"
                                            m))
                        (lc-dialog-load "banana-quest" "accepted"))
                    (progn
                      (lc-dialog-load "banana-quest" "impossible"))))))

        (setq on-dialog-declined
              (lambda
                (lc-dialog-load "banana-quest" "declined")
                (setq on-dialog-closed exit)))))

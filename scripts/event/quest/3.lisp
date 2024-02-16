
(lc-dialog-load "taxi-quest" "intro")



(opponent-init 3 'neutral)

(island-configure
 (opponent)
 '((power-core 0 13)))


(defn on-converge [0]
  (lc-dialog-load "taxi-quest" "offer")

  (dialog-await-binary-q (lc-dialog-get "taxi-quest" "opt1")
                         (lc-dialog-get "taxi-quest" "opt2")))


(defn on-dialog-accepted [0]
  (let ((m (eval-file "/scripts/event/quest/make_quest_marker.lisp")))
    (setq on-dialog-closed exit)
    (if m
        (progn
          (push 'qids 3)
          (push 'quests (cons "/scripts/event/quest_marker/pickup.lisp" m))
          (adventure-log-add 19 '())
          (lc-dialog-load "taxi-quest" "accepted"))
      (progn
        (lc-dialog-load "taxi-quest" "skip")))))


(defn on-dialog-declined [0]
  (lc-dialog-load "taxi-quest" "declined")
  (setq on-dialog-closed exit))

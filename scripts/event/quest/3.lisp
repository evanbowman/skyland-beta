
(load-dialog "taxi-quest" "intro")



(opponent-init 3 'neutral)

(island-configure
 (opponent)
 '((power-core 0 13)))


(defn on-converge [0]
  (load-dialog "taxi-quest" "offer")

  (dialog-await-binary-q (get-dialog "taxi-quest" "opt1")
                         (get-dialog "taxi-quest" "opt2")))


(defn on-dialog-accepted [0]
  (let ((m (eval-file "/scripts/event/quest/make_quest_marker.lisp")))
    (setq on-dialog-closed exit)
    (if m
        (progn
          (push 'qids 3)
          (push 'quests (cons "/scripts/event/quest_marker/pickup.lisp" m))
          (adventure-log-add 19 '())
          (load-dialog "taxi-quest" "accepted"))
      (progn
        (load-dialog "taxi-quest" "skip")))))


(defn on-dialog-declined [0]
  (load-dialog "taxi-quest" "declined")
  (setq on-dialog-closed exit))

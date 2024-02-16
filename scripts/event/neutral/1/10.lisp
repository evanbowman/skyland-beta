
(lc-dialog-load "explorer" "intro")

(opponent-init -3 'neutral)

(island-configure
 (opponent)
 '((balloon 0 10)))

(terrain-set (opponent) -3)

(chr-new (opponent) 1 14 'neutral 0)


(defn on-converge [0]
  (lc-dialog-load "explorer" "greeting")

  (setq on-dialog-closed
        (lambda
          (lc-dialog-load "explorer" "offer")
          (dialog-await-y/n)
          (setq on-dialog-closed '())))
  (setq on-converge nil))


(defn on-dialog-accepted [0]

  (let ((temp (chr-slots (player)))
        (join (lambda
                (adventure-log-add 53 '())
                (dialog $0))))
    (if temp
        (progn
          (setq temp (get temp (choice (length temp))))
          (chr-new (player) (car temp) (cdr temp) 'neutral nil)
          (chr-del (opponent) 1 14)
          (if (or (equal (choice 2) 1) (< (coins) 600))
              (join (lc-dialog-get "explorer" "join1"))
            (progn
              (coins-set (- (coins) 600))
              (join (lc-dialog-get "explorer" "join2")))))
      (progn
        (lc-dialog-load "explorer" "no-room")
        (defn on-dialog-closed [0]
          (lc-dialog-load "explorer" "offer2")
          (defn on-dialog-closed [0]
            (alloc-space 'ladder)
            (sel-input 'ladder
                       (lc-dialog-get "explorer" "place")
                       (lambda
                         (sound "build0")
                         (room-new (player) `(ladder ,$1 ,$2))
                         (chr-del (opponent) 1 14)
                         (chr-new (player) $1 (+ 1 $2) 'neutral nil)
                         (lc-dialog-load "explorer" "thanks")
                         (defn on-dialog-closed [0]
                           (join (lc-dialog-get "explorer" "done"))
                           (setq on-dialog-closed nil)
                           (exit)))))))))
  (exit))


(defn on-dialog-declined [0]
  (exit))

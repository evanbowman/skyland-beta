
(load-dialog "drone-carrier" "intro")


(opponent-init 7 'neutral)

(island-configure
 (opponent)
 '((hull 0 12)
   (hull 0 13)
   (hull 0 14)
   (hull 0 11)
   (hull 1 14)
   (power-core 1 12)
   (hull 1 11)
   (hull 2 14)
   (hull 2 11)
   (masonry 3 12 0)
   (hull 3 14)
   (drone-bay 3 13)
   (masonry 3 10 0)
   (drone-bay 3 11)
   (masonry 4 12 0)
   (hull 4 14)
   (masonry 4 10 0)
   (masonry 5 12 0)
   (drone-bay 5 13)
   (hull 5 14)
   (drone-bay 5 11)
   (masonry 5 10 0)
   (masonry 6 12 0)
   (hull 6 14)
   (masonry 6 10 0)))


(secret 6 14 "yes no")

(flag-show (opponent) 4)


(defn on-converge [0]
  ;; want drones?
  (load-dialog "drone-carrier" "question1")
  (dialog-await-y/n)

  (defn on-dialog-accepted [0]
    ;; less than 2?
    (load-dialog "drone-carrier" "question2")
    (dialog-await-y/n)

    (adventure-log-add 39 '())

    (defn on-dialog-accepted [0]
      ;; place one drone bay
      (alloc-space 'drone-bay)

      (sel-input 'drone-bay
                 (get-dialog "drone-carrier" "pick")
                 (lambda
                   (sound "build0")
                   (room-new (player) `(drone-bay ,$1 ,$2))
                   (load-dialog "drone-carrier" "success")
                   (exit))))

    (defn on-dialog-declined [0]
      ;; place two drone bays

      (alloc-space 'drone-bay)

      (sel-input 'drone-bay
                 (get-dialog "drone-carrier" "pick2")
                 (lambda
                   (sound "build0")
                   (room-new (player) `(drone-bay ,$1 ,$2))

                   (sel-input 'drone-bay
                              (get-dialog "drone-carrier" "pick3")
                              (lambda
                                (sound "build0")
                                (room-new (player) `(drone-bay ,$1 ,$2))
                                (load-dialog "drone-carrier" "success")
                                (exit)))))))

  (setq on-dialog-declined exit))

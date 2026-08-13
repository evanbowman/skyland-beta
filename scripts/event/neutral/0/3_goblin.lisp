;;;
;;; neutral/0/3_goblin.lisp
;;;

(tr-bind-current)

(dialog (tr "Your sensors detect some goblin scavengers returning from a raid. <B:0> Their leader excitedly waves you down, eager to trade their mysterious finds..."))

(opponent-init 5 'neutral)

(island-configure
 (opponent)
 '((power-core 3 13)
   (hull 0 14)))


(chr-new (opponent) 1 14 'neutral '((race . 1)))
(chr-new (opponent) 2 14 'neutral '((race . 1)))


(flag-show (opponent) flag-id-marauder)


(let ((item (sample '(arc-gun flak-gun fire-charge))))

  (terrain-set (opponent) (+ (terrain (opponent)) (* 2 (car (rinfo 'size item)))))

  (map (lambda (y)
         (room-new (opponent) (list item 5 y))
         (room-new (opponent) (list item (+ 5 (car (rinfo 'size item))) y)))
       '(11 12 13 14))


  (defn/temp place-items (reply)
    (let ((msgs (list (string (tr "Place first ")
                              (rinfo 'name item)
                              (format " (%x%):" (car (rinfo 'size item)) (cdr (rinfo 'size item))))
                      (string (tr "Place second ") (rinfo 'name item) ":"))))
      (while msgs
        (alloc-space item)
        (let ((xy (await (sel-input* item (car msgs)))))
          (room-new (player) (list item (car xy) (cdr xy)))
          (sound "build0"))
        (setq msgs (cdr msgs))))
    (await (dialog* reply))
    (exit))


  (defn/temp buy-items ()
    (adventure-log-add 67 (list (rinfo 'name item) 1300))
    (coins-add -1300)
    (place-items (tr "<c:Scavenger:35>Yesss! Sssmart choice! Don't mind the burn marksss, they add character!")))


  (defn/temp take-by-force ()
    (adventure-log-add 71 (list (rinfo 'name item)))
    (await (dialog* (tr "<c:Scavenger:35>Gah! Fine, take them! <B:0> Ssstolen from some fat merchantsss anyway... <B:0> But we won't forget thisss. We know where to find more friendsss...")))
    (push-pending-event (+ 2 (choice 4)) "/scripts/event/hostile/scavenger-vengeance.lisp")
    (place-items (tr "The goblins storm off, swearing vengeance...")))


  (defn/temp remove-shop ()
    (let ((xy (cdr (wg-pos))))
      ;; Switch the current map node back to visited, so that it doesn't appear
      ;; as a shop on the world map.
      (wg-node-set (first xy) (second xy) wg-id-visited)))


  (defn/temp setup-shop ()
    (let ((xy (cdr (wg-pos))))
      ;; Swap the current level type, converting it temporarily into a shop, so
      ;; the player can leave to salvage and return.
      (wg-node-set (first xy) (second xy) wg-id-shop)
      (await (dialog* (tr "<c:Scavenger:35>Heh, we'll ssstick around. Come find usss when you've ssscraped up the resources! <B:0> (or use the START menu to return to the world map)")))))


  (defn on-converge ()
    (setq on-converge nil)
    (let ((msg (string
                (format (tr "<c:Scavenger:35> Found thessse %s") (rinfo 'name item))
                (if (equal (faction) 'goblin)
                    (tr " on a human isssle!")
                    (tr "... err ... Well, don't worry where I got them! <B:0>"))
                (tr " Still working, barely sssinged! 1300@ for two, yesss? Better price than waiting for your workshop to build them!"))))
      (case (await (dialog-choice* msg
                                   (list (tr "Purchase for 1300@.")
                                         (tr "Take by force.")
                                         (tr "Decline offer."))))
        (0 (on-dialog-accepted))
        (1 (take-by-force))
        (2 (on-dialog-declined)))))


  (setq on-dialog-declined exit)


  (defn on-dialog-accepted ()
    (if (> (coins) 1299)
        (buy-items)
        (if (dialog-await-y/n (tr "<c:Scavenger:35>Sorry, that's not enough! Do you want to sssalvage some ssstuff to come up with the ressourcesss for payment?"))
            (setup-shop)
            (on-dialog-declined))))


  (defn on-shop-enter ()
    (if (> (coins) 1299)
        (if (dialog-await-y/n (format (tr "<c:Scavenger:35>Ssseems like you have enough now! <B:0> Buy two %s for 1300@?")
                                      (rinfo 'name item)))
            (progn
              ;; In shop levels, sel-input allows you to cancel selecting
              ;; coordinates. take down the shop now that we no longer need it.
              (remove-shop)
              (buy-items)))
        (if (not (dialog-await-binary-q (format (tr "<c:Scavenger:35>Sorry, the price wasss 1300@, you're ssstill %@ short…")
                                                (- 1300 (coins)))
                                        (tr "Salvage more stuff…")
                                        (tr "Exit.")))
            (exit))))


  (defn on-level-exit ()
    (remove-shop)))

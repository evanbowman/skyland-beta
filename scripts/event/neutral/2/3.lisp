;;;
;;; neutral/2/3.lisp
;;;

(tr-bind-current)


(dialog (tr "A small fortress hurtles through the air, with goblins in pursuit. The captain calls for help..."))


(opponent-init 9 'neutral)

(island-configure
 (opponent)
 '((power-core 3 13)
   (power-core 3 11)
   (decimator 0 13)
   (plundered-room 5 13)
   (plundered-room 5 11)
   (plundered-room 6 13)
   (hull 8 14)
   (hull 8 13)
   (hull 8 12)))

(chr-new (opponent) 2 14 'neutral 0)


(set-temp 'dec-cost 1500)
(set-temp 'dec-discount (floor (/ dec-cost 2)))


(defn/temp place-decimator ()
  (coins-add (- dec-cost))
  (alloc-space 'decimator)
  (let ((xy (await (sel-input* 'decimator (tr "Place weapon where? (2x2)")))))
    (sound "build0")
    (room-new (player) (list 'decimator (car xy) (cdr xy)))
    (room-del (opponent) 0 13))
  (await (dialog* (tr "<c:Captain:7> OK, all finished! The weapon recharges quite slowly, but nothing's more destructive! You need to move one of your crew into the weapon, though, or it won't recharge.")))
  (adventure-log-add 44 '())
  (exit))


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
    (await (dialog* (tr "<c:Captain:7>Alright, I'll wait here. Come find me when you've scraped together the resources! <B:0> (or use the START menu to return to the world map)")))))


(defn/temp hide-evidence? ()
  (case (await (dialog-choice*
                (format (tr "<c:Captain:7>Look, those goblins are getting closer... <B:0> Tell you what - pay me %@ instead of %@, and help me destroy the evidence that I was ever here. <B:0> They'll assume YOU stole it from them directly. Deal?")
                        dec-discount
                        dec-cost)
                (list (format (tr "Hide evidence (%@).") dec-discount)
                      (format (tr "Pay %@…") dec-cost)
                      (tr "No thanks."))))
    (0 (setq dec-cost dec-discount)
       (push-pending-event (+ 1 (choice 2))
                           "/scripts/event/hostile/dec-revenge.lisp")
       (on-dialog-accepted))
    (1 (on-dialog-accepted))
    (2 (on-dialog-declined))))


(defn on-converge ()
  (setq on-converge nil)
  (case (await (dialog-choice*
                (format (tr "<c:Captain:7>I managed to steal this decimator from some goblins, but they're catching up to me! I know... I could sell you the weapon! I'll install it on your island for %@...")
                        dec-cost)
                (list (format (tr "Here's %@…") dec-cost)
                      (tr "Can I have a discount?")
                      (tr "No thanks."))))
    (0 (on-dialog-accepted))
    (1 (hide-evidence?))
    (2 (on-dialog-declined))))


(setq on-dialog-declined exit)


(defn on-dialog-accepted ()
  (if (> (coins) (decr dec-cost))
      (place-decimator)
      (if (dialog-await-y/n (format (tr "<c:Captain:7>Sorry, I went to all this trouble, I really can't sell you this tech for less than %@. Do you want to salvage some stuff to come up with the funds?")
                                    dec-cost))
          (setup-shop)
          (on-dialog-declined))))


(defn on-shop-enter ()
  (if (> (coins) (decr dec-cost))
      (if (dialog-await-y/n (format (tr "<c:Captain:7>Looks like you've got enough now. <B:0> Install the decimator for %@?")
                                    dec-cost))
          (progn
            ;; In shop levels, sel-input allows you to cancel selecting
            ;; coordinates. take down the shop now that we no longer need it.
            (remove-shop)
            (place-decimator)))
      (if (not (dialog-await-binary-q (format (tr "<c:Captain:7>Sorry, the price was %@, you're still %@ short…")
                                              dec-cost
                                              (- dec-cost (coins)))
                                      (tr "Salvage more stuff…")
                                      (tr "Exit.")))
          (exit))))


(defn on-level-exit ()
  (remove-shop))

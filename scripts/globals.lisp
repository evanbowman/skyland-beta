;;;
;;; globals.lisp
;;;
;;; The interpreter does not allow you to set a variable that isn't either a let
;;; binding or define explicitly as global.
;;;


(global
 'on-fadein
 'on-converge
 'on-dialog-closed
 'on-victory
 'on-room-destroyed
 'on-crew-died
 'on-shop-item-sel
 'on-shop-enter
 'on-dialog-accepted
 'on-dialog-declined
 'on-level-exit
 'on-menu-resp
 'last-zone
 'enemies-seen
 'friendlies-seen
 'surprises-seen
 'qids
 'quests
 'adventure-log
 'shop-items
 'zone-shop-items
 'qvar
 'pending-events
 'debrief-strs
 'adv-var-set
 'adv-var-list
 'tr-bindings
 'tr-files)


(defconstant gamespeed-paused 0)
(defconstant gamespeed-slow 1)
(defconstant gamespeed-normal 2)
(defconstant gamespeed-fast 3)
(defconstant gamespeed-rewind 4)


(defconstant difficulty-beginner 0)
(defconstant difficulty-normal 1)
(defconstant difficulty-hard 2)
(defconstant surprise-count 3)

(defconstant weather-id-clear 1)
(defconstant weather-id-sunshower 2)
(defconstant weather-id-rain 3)
(defconstant weather-id-snow 4)
(defconstant weather-id-storm 5)
(defconstant weather-id-ash 6)
(defconstant weather-id-night 7)
(defconstant weather-id-solar-storm 8)

(defconstant flag-id-pirate 0)
(defconstant flag-id-marauder 1)
(defconstant flag-id-old-empire 4)
(defconstant flag-id-banana 5)
(defconstant flag-id-merchant 6)
(defconstant flag-id-colonist 7)
(defconstant flag-id-sylph 36)

(defconstant faction-enable-human-mask (bit-shift-left 1 0))
(defconstant faction-enable-goblin-mask (bit-shift-left 1 1))
(defconstant faction-enable-sylph-mask (bit-shift-left 1 2))

(defconstant wg-id-visited 2)
(defconstant wg-id-neutral 2)
(defconstant wg-id-shop 7)

(defconstant default-early-gc-thresh 1000)

(defconstant prop-workshop-required    0x1)
(defconstant prop-not-constructible    0x2)
(defconstant prop-plugin               0x4)
(defconstant prop-manufactory-required 0x8)
(defconstant prop-tutorial-disabled    0x10)
(defconstant prop-locked-by-default    0x20)
(defconstant prop-roof-hidden          0x40)
(defconstant prop-has-chimney          0x80)
(defconstant prop-chimney-hidden       0x100)
(defconstant prop-takes-ion-damage     0x200)
(defconstant prop-cancels-ion-damage   0x400)
(defconstant prop-flag-mount           0x800)
(defconstant prop-fragile              0x1000)
(defconstant prop-adventure-only       0x2000)
(defconstant prop-sandbox-only         0x4000)
(defconstant prop-fluid                0x8000)
(defconstant prop-destroy-special      0x10000)
(defconstant prop-salvage-disabled     0x20000)
(defconstant prop-multiplayer-disabled 0x40000)
(defconstant prop-sk-forever-disabled  0x80000)
(defconstant prop-fireproof            0x100000)
(defconstant prop-highly-flammable     0x200000)
(defconstant prop-habitable            0x400000)
(defconstant prop-generates-heat       0x800000)
(defconstant prop-big-explosion        0x1000000)
(defconstant prop-multiboot-allowed    0x2000000)
(defconstant prop-singleton            0x4000000)
(defconstant prop-human-only           0x8000000)
(defconstant prop-sylph-only           0x10000000)
(defconstant prop-goblin-only          0x20000000)


(global '--autoload-symbols)

(setq --autoload-symbols
      '(clone-isle
        core-service
        find-or-create-cargo-bay
        find-crew-slot
        find-crew-slot-cb
        hash
        dialog-sequence
        cargo-bays
        hostile-pickup-cart
        pickup-cart
        pickup-cart-cb
        place-new-block
        repairman
        on-cargo-plundered))

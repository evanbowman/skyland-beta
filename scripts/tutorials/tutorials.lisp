;;;
;;; tutorials.lisp
;;;


(eval-file "/scripts/reset_hooks.lisp")


(gc)

(map cons
     (split (lc-dialog-get "tutorials" "names") ",")
     '("tutorials/overview.lisp"
       "tutorials/weapon_groups.lisp"
       "tutorials/salvage.lisp"
       "tutorials/characters.lisp"
       "tutorials/terrain.lisp"
       "tutorials/power_balance.lisp"
       "tutorials/construction_tricks.lisp"
       "tutorials/advanced_structures.lisp"
       "tutorials/game_speed.lisp"
       "tutorials/transporter.lisp"
       "tutorials/moving_blocks.lisp"
       "tutorials/replicator.lisp"
       "tutorials/firing_patterns.lisp"))

;;;
;;; sandbox.lisp
;;;


(eval-file "/scripts/reset_hooks.lisp")

;; NOTE: engine binds config fields from the sandbox settings menu to a list
;; variable called conf.


(coins-set (get conf 0))
(terrain (player) (get conf 1))



(defn sb-help
  (dialog "Sandbox mode gives you nearly unlimited coins, and allows you to build on your opponent's island in addition to your own! You may also reposition your opponent's characters! Try out strategies, or just play around! You can even build a couple of big fortresses, select spectate on the start menu, and let the AI control both castles!"))


(if (not (syscall "save-bit-load" 3))
    (setq on-fadein
          (lambda
            (setq on-fadein nil)
            (syscall "save-bit-store" 3 1)
            (dialog "Welcome to the Battle Sandbox! Want any help?")
            (dialog-await-y/n)
            (setq on-dialog-accepted sb-help)
            (setq on-dialog-declined nil))))





;; NOTE: in case I haven't explained elsewhere, the interpreter does a small
;; symbol optimization to save space in the string intern table, hence all of
;; the four-character variable names.
(defn mkch
  ;; Arg 0: island
  ;; Arg 1: 'hostile or 'neutral symbol
  (let ((isle $0)
        (mode $1))

    ;; NOTE: conf[5] holds the character count config
    (repeat (get conf 5)

      (let ((slot (chr-slots isle)))
        (if (not slot)
            (let ((s (construction-sites isle '(2 . 2))))
              (if (not s)
                  (syscall "fatal" "not enough room to place chrs!"))
              (room-new isle (list 'workshop (car (car s)) (cdr (car s))))
              (setq slot (chr-slots isle))))
        (if slot
            (chr-new isle (car (car slot)) (cdr (car slot)) mode 0))))))


(island-configure
 (player)
 '((power-core 1 13)))


(mkch (player) 'neutral)
(flag-show (player))



(opponent-init (get conf 1) 'hostile)


(flag-show (opponent))


(island-configure
 (opponent)
 `((power-core ,(- (get conf 1) 3) 13)))

(mkch (opponent) 'hostile)


(unbind 'conf 'mkch)
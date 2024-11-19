
(global '*test-data*)

(defn create-test ()
  ;; This is a bit of a hack. The tutorial mode is already set up to record
  ;; keystrokes to the game's logfile. Tutorial mode is also well-structured
  ;; for creating reproducible level outcomes. Maybe someday I'll create a
  ;; game mode specific for creating test cases, but for now, let's just abuse
  ;; tutorial mode.
  (game-mode-set 2)
  (sel-move (player) 0 14)
  (island-set-pos (opponent) (+ 250 (* 16 (- 10 (terrain (opponent))))) 374)
  (eval-file "/scripts/sandbox/create_test.lisp")

(defn on-victory ()
  (when *test-data*
    (eval-file "/scripts/sandbox/finalize_test.lisp")
    (file-store *test-data*)
    (fatal "test environment saved to /test-setup.lisp! See log.txt for keystrokes!"))))

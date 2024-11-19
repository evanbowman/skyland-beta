
(global '*test-data*) ; for the linter

(defn newline ()
  (file-write! *test-data* -1 '(10)))

(defn append-str (str)
  (file-write! *test-data* -1 (string-to-bytes str))
  (newline))

(file-unlink "/test-setup.lisp")
(setq *test-data* (file-load "/test-setup.lisp"))


(append-str (string "(weather-set " (weather) ")"))


(append-str (string "(terrain-set (player) " (terrain (player)) ")"))
(append-str (string "(island-configure (player) "
                    (rooms (player))
                    ")"))


(append-str (string "(opponent-init " (terrain (opponent)) " 'hostile)"))
(append-str (string "(island-configure (opponent) "
                    (rooms (opponent))
                    ")"))

(map (lambda (info)
       (append-str (format "(chr-new (player) % % 'neutral %)" (get info 0) (get info 1) (cddr info))))
     (chrs (player)))

(map (lambda (info)
       (append-str (format "(chr-new (opponent) % % 'hostile %)" (get info 0) (get info 1) (cddr info))))
     (chrs (opponent)))

(append-str (string "(coins-set " (coins) ")"))
(append-str "(defn on-fadein () (sel-move (player) 0 14))")
(append-str "(autopilot '(<see log.txt for keyloger results...>))")

(unbind 'newline 'append-str)

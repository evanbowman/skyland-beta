
(global '*test-data*) ; for the linter

(defn newline ()
  (file-write! *test-data* -1 '(10)))

(defn append-str (str)
  (file-write! *test-data* -1 (string-to-bytes str))
  (newline))


(append-str "(defn on-level-exit ()")

(append-str (string
             "(assert-eq (rooms (player)) "
             (rooms (player))
             ")"))

(append-str (string
             "(assert-eq (rooms (opponent)) "
             (rooms (opponent))
             ")"))

(append-str (string
             "(assert-eq (chrs (player)) "
             (chrs (player))
             ")"))

(append-str (string
             "(assert-eq (chrs (opponent)) "
             (chrs (opponent))
             ")"))

(append-str ")")

(unbind 'newline 'append-str)

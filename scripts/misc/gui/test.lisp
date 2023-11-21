

(defn on-A
  nil)


(defn on-B
  (push-menu "ready" '()))


(defn on-L
  (gui-set-content "t1" (rinfo 'name 'splitter))
  (gui-delete-node "t2")
  (gui-add-node "t3" "text")
  (gui-set-attr "t3" "x" "5")
  (gui-set-attr "t3" "y" "6")
  (gui-set-content "t3" "hey, it works!")
  (gui-set-attr "ic3" "icon"
                (string (cdr (assoc 'ico1 (room-meta 'splitter))))))


(defn on-R
  nil)


(defn on-U
  nil)


(defn on-D
  nil)


(defn on-menu-exit
  (unbind 'on-A 'on-B 'on-L 'on-R 'on-U 'on-D))

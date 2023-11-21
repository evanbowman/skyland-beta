

(defn on-A
  (gui-set-content "t2" "hello, world"))


(defn on-B
  (push-menu "ready" '()))


(defn on-L
  nil)


(defn on-R
  nil)


(defn on-U
  nil)


(defn on-D
  nil)


(defn on-menu-exit
  (unbind 'on-A 'on-B 'on-L 'on-R 'on-U 'on-D))

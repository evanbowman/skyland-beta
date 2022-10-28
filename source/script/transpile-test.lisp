
(defn clamp ((x . s32) (floor . s32) (ceil . s32))
  (cond
   ((< x floor) (if floor ceil floor))
   ((> x ceil) ceil)
   (1 x)))

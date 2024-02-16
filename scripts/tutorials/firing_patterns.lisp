;;;
;;; firing_patterns.lisp
;;;


(terrain-set (player) 4)
(island-configure
 (player)
 '((power-core 1 13)
   (cannon 3 14)
   (cannon 3 13)
   (cannon 3 12)
   (cannon 3 11)))


(chr-new (player) 1 14 'neutral 0)



(coins-add 1000)


(opponent-init 7 'hostile)

(island-configure
 (opponent)
 '((power-core 5 13)))

(map
 (lambda
   (let ((x $0))
     (map
      (lambda
        (room-new (opponent) (list 'hull x $0)))
      (range 8 15))))
 (range 0 4))


(autopilot
 '((500 nil)
   (100 (lc-dialog-get "tutorials" "fp1"))
   (884 Right)
   (167 Right)
   (133 Right)
   (183 Start-p)
   (116 Down)
   (116 Start-np)
   (334 A)
   (183 Up)
   (183 A)
   (183 Up)
   (166 A)
   (183 Up)
   (133 A)
   (317 B)
   (233 Down)
   (217 Down)
   (1669 A)
   (768 Down)
   (600 Down)
   (1319 Right)
   (317 A)
   (200 nil)
   (100 (lc-dialog-get "tutorials" "fp2"))
   (2000 A)
   (567 R)
   (2933 Right)
   (383 A)
   (634 Left)
   (350 Left)
   (67 Up)
   (834 Right)
   (4000 nil)
   (100 (lc-dialog-get "tutorials" "fp3"))
   (1000 Right)
   (934 A)
   (500 Down)
   (199 Left)
   (167 Down)
   (249 Down)
   (350 A)
   (451 Up)
   (366 Left)
   (750 Down)
   (634 Down)
   (50 Right)
   (1301 A)
   (283 Right)
   (150 Up)
   (150 Up)
   (166 Up)
   (167 Up)
   (200 Up)
   (300 Right)
   (400 A)
   (451 Left)
   (216 Left)
   (166 Up)
   (217 Up)
   (667 Up)
   (7855 Right)
   (66 Down)
   (183 Right)
   (530 Down)
   (1500 nil)
   (100 (lc-dialog-get "tutorials" "fp4"))
   (500 A)
   (383 R)
   (612 R)
   (350 Down)
   (200 Down)
   (334 A)
   (1351 Left)
   (166 Left)
   (5000 nil)
   (100 Up)))

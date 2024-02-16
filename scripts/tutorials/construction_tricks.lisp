;;;
;;; construction_tricks.lisp
;;;


(coins-add 4000)

(terrain-set (player) 4)
(island-configure
 (player)
 '((power-core 1 13)
   (manufactory 1 11)
   (hull 3 13)
   (hull 3 14)))


(opponent-init 4 'hostile)

(island-configure
 (opponent)
 '((power-core 1 13)))


(autopilot
 '((818 Right)
   (1000 nil)
   (100 (lc-dialog-get "tutorials" "cons1"))
   (100 R)
   (434 A)
   (2354 Down)
   (567 Down)
   (1185 Down)
   (701 Down)
   (1101 Down)
   (1535 Up)
   (634 Up)
   (384 Up)
   (350 Up)
   (316 Up)
   (350 Up)
   (1703 Down)
   (1318 Right)
   (300 B)
   (200 B)
   (100 nil)
   (100 (lc-dialog-get "tutorials" "cons2"))
   (1000 R)
   (300 A)
   (400 Right)
   (400 Right)
   (2253 Start-p)
   (33 Start-np)
   (1768 Right)
   (667 Right)
   (517 Right)
   (867 Left)
   (383 Left)
   (383 Left)
   (3071 B)
   (1286 B)
   (100 (lc-dialog-get "tutorials" "cons3"))))

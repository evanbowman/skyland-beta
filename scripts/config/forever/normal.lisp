;;
;; Coin yeild percentage settings for SKYLAND Forever
;;

(coins-add 3000)

(map (lambda (syscall "setvar" (cdr $0) (car $0)))
 '((25 . "sf_p1_coin_yield")
   (18 . "sf_p2_coin_yield")
   (10 . "sf_p3_coin_yield")
   (7  . "sf_p4_coin_yield")))

(syscall "setvar" "score_multiplier" 2)
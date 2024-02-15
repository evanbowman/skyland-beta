
(load-dialog "sylph-quest" "dest-intro")


(opponent-init 9 'neutral)

(island-configure
 (opponent)
 '((forcefield* 0 9)
   (forcefield 0 11)
   (bronze-hull 0 14)
   (forcefield 0 12)
   (forcefield 0 10)
   (forcefield* 0 13)
   (forcefield 1 7)
   (water-source 1 14)
   (forcefield* 1 8)
   (water-source 1 13)
   (bronze-hull 2 11)
   (masonry 2 14 0)
   (masonry 2 12 0)
   (shrubbery 2 10)
   (forcefield 2 7)
   (masonry 2 13 0)
   (war-engine 3 9)
   (forcefield* 3 7)
   (masonry 3 13 3)
   (water-source 3 14)
   (masonry 4 13 3)
   (masonry 4 14 0)
   (forcefield* 4 7)
   (forcefield* 5 7)
   (water-source 5 14)
   (masonry 5 13 3)
   (masonry 6 14 0)
   (forcefield 6 7)
   (bronze-hull 6 11)
   (masonry 6 12 0)
   (masonry 6 13 0)
   (lemon-tree 6 9)
   (forcefield 7 7)
   (water-source 7 13)
   (forcefield* 7 8)
   (water-source 7 14)
   (forcefield 8 10)
   (forcefield 8 11)
   (forcefield 8 12)
   (forcefield* 8 13)
   (bronze-hull 8 14)
   (forcefield* 8 9)))

(secret 4 14 (get-dialog "sylph-quest" "secret"))


(let ((id (lookup 6 qvar))
      (boy nil))

  (map (lambda
         (if (equal id (lookup 'id (cddr $0)))
             (setq boy true)))
       (chrs (player)))

  (if boy
      (defn on-converge [0]
        (load-dialog "sylph-quest" "sylph-greet")

        (defn on-dialog-closed [0]
          (load-dialog "sylph-quest" "boy-reply")

          (defn on-dialog-closed [0]
            (load-dialog "sylph-quest" "sylph-greet2")

            (defn on-dialog-closed [0]
              (map (lambda
                     (if (equal id (lookup 'id (cddr $0)))
                         (chr-del (player) (car $0) (cadr $0))))
                   (chrs (player)))
              (coins-add 3000)
              (adventure-log-add 55 nil)
              (load-dialog "sylph-quest" "boy-depart")
              (defn on-dialog-closed [0]
                (load-dialog "sylph-quest" "thanks")
                (setq on-dialog-closed (lambda
                                         (on-timeout 500 'fut)
                                         (setq on-dialog-closed nil)))
                (defn fut [0]
                  (sound "bell")
                  (sound "thunder_close_1")
                  (effect "lightning" 0 0)
                  (opponent-reset)
                  (wg-storm-frontier-set (max (list (- (wg-storm-frontier) 3) 1)))

                  (on-timeout 1000 'fut)

                  (defn fut[0]
                    (load-dialog "sylph-quest" "award")
                    (unbind 'fut)
                    (setq on-dialog-closed exit))))))))

    (defn on-converge [0]
      (load-dialog "sylph-quest" "failed")
      (exit))))

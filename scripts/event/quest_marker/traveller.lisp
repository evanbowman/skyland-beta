

(load-dialog "wanderer-quest" "dest-intro")


(defn on-fadein [0]
  (map (lambda
         (if (equal (get $0 0) 'torch)
             (fire-new (opponent) (get $0 1) (get $0 2))))
       (rooms (opponent)))
  (setq on-fadein nil))


(let ((id nil))
  (map (lambda
         (let ((icon (lookup 'icon (cddr $0))))
           (if (equal icon 23)
               (setq id (lookup 'id (cddr $0))))))
       (chrs (player)))

  (defn on-converge [0]
    (setq on-converge nil)
    (if tr
        (progn

          (let ((tab (eval-file "/scripts/config/room_tab.lisp"))
                (sel '())
                (eq (lambda
                      (let ((sym $0)
                            (ret 0))
                        (map (lambda
                               (if (equal $0 sym)
                                   (setq ret 1)))
                             $1)
                        ret))))

            (while (< (length sel) 1)
              (let ((pick (sample tab)))
                (let ((cost (get pick 2)))
                  (when (> cost 1000)
                    (setq sel (cons (car pick) sel))))))

            (load-dialog "wanderer-quest" "dest-success")
            (defn on-dialog-closed [0]
              (coins-add 2000)
              (let ((sym0 (get sel 0)))
                (alloc-space sym0)
                (sel-input sym0
                           (string (get-dialog "wanderer-quest" "dest-place") (rinfo 'name sym0))
                           (lambda
                             (sound "build0")
                             (room-new (player) (list sym0 $1 $2))
                             (load-dialog "wanderer-quest" "goodbye")
                             (map (lambda
                                    (if (equal id (lookup 'id (cddr $0)))
                                        (chr-del (player) (car $0) (cadr $0))))
                                  (chrs (player)))
                             (defn on-dialog-closed [0]
                               (load-dialog "wanderer-quest" "depart")
                               (setq on-dialog-closed exit))))))))
      (progn
        (load-dialog "wanderer-quest" "failed")
        (exit)))))


(opponent-init 7 'neutral)

(island-configure
 (opponent)
 '((lemon-tree 0 13)
   (workshop 1 11)
   (masonry 1 14 0)
   (masonry 2 13 3)
   (masonry 2 14 3)
   (power-core 3 11)
   (masonry 3 14 3)
   (masonry 3 13 3)
   (torch 4 9)
   (masonry 4 13 3)
   (masonry 4 14 3)
   (masonry 5 14 0)
   (masonry 5 12 0)
   (masonry 5 11 0)
   (windmill 5 13)
   (masonry 6 14 2)
   (masonry 6 13 0)
   (masonry 6 12 0)
   (torch 6 11)
   (plundered-room 6 8)))

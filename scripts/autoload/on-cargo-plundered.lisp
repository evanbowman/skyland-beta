;;;
;;; autoload/on-cargo-plundered.lisp
;;;

(tr-bind-current)

(lambda (cargo-list)
  (if-let ((sel (dialog-await-y/n (format (tr (s+ "You've plundered a cargo bay containing: %! "
                                                  "<B:0> Accept the cargo?"))
                                          (string-join (map (lambda (r)
                                                              (rinfo 'name r))
                                                            cargo-list)
                                                       ", ")))))
      (foreach (lambda (item)
                 (alloc-space item)
                 (let ((msg (format (tr "Place % where?") (rinfo 'name item))))
                   (let (([x . y] (await (sel-input* item msg))))
                     (ts-record-enable true)
                     (room-new (player) (list item x y))
                     (ts-record-enable false)
                     (sound "build0"))))
               cargo-list)
    (coins-add (engine-get "enemy_cargo_min_value"))))

;;;
;;; scripts/inspect/crew.lisp
;;;

(tr-bind-current)

(let ((busy (lambda (icon name)
              (dialog-await (format (tr "<c:%:%> ...") name icon))
              (dialog-await (tr "This crewmember seems busy...")))))
  (lambda (chr room)
    (if-let ((icon (lookup 'icon (cddr chr))))
        (let* ((profile (format "/strings/%/character_inter.ini" (lang)))
               (tag (format "character_%" icon))
               (personality (read-ini profile tag "personality"))
               (name (read-ini profile tag "name"))
               (fpath (format "/strings/%/character_%.ini" (lang) personality)))
          (if (file-exists? fpath)
              (let ((opts nil))
                (push opts (read-ini fpath "rooms" (string room)))
                (when adventure-log
                  (push opts (read-ini fpath "adv-log" (string (caar adventure-log))))
                  (when (cdr adventure-log)
                    (push opts (read-ini fpath "adv-log" (string (caar (cdr adventure-log)))))))
                (if-let ((txt (sample (remove opts nil))))
                    (dialog-await
                     (format "<c:%:%> " name icon)
                     txt)
                  (busy icon name)))
              (busy icon name)))
      (dialog-await (tr "This crewmember doesn't seem very talkative...")))))

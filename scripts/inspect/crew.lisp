;;;
;;; scripts/inspect/crew.lisp
;;;

(tr-bind-current)

(lambda (chr room)
  (if-let ((icon (lookup 'icon (cddr chr))))
      (let ((fpath (format "/strings/%/character/%.ini" (lang) icon)))
        (if (file-exists? fpath)
            (let ((opts nil))
              (push opts (read-ini fpath "rooms" (string room)))
              (when adventure-log
                (push opts (read-ini fpath "adv-log" (string (caar adventure-log)))))
              (when (and (not opts) (choice 2))
                (push opts (read-init fpath "weather" (string (weather)))))
              (if-let ((txt (sample (remove opts nil))))
                  (dialog-await txt)
                (progn
                  (dialog-await (format (tr "<c:crewmember:%> ...") icon))
                  (dialog-await (tr "This crewmember seems busy...")))))
            (progn
              (dialog-await (format (tr "<c:crewmember:%> ...") icon))
              (dialog-await (tr "This crewmember seems busy...")))))
    (dialog-await (tr "This crewmember doesn't seem very talkative..."))))

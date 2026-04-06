;;;
;;; extract_strings.lisp
;;;
;;; This script extracts strings from all of the game's scripts and builds
;;; localization data files that you can then translate. To run this script,
;;; you'd need a desktop build of the game, where you may pass --init-locale as
;;; a command line argument.
;;;
;;; For example, on MacOS:
;;; ./Skyland.app/Contents/MacOS/Skyland --init-locale --output=../strings/spanish
;;;

(defn visit-strings (expr callback within-tr)
  (cond
    ((list? expr)
     (let ((is-tr (or within-tr (equal (car expr) 'tr))))
       (foreach (lambda (sub-expr)
                  (visit-strings sub-expr callback is-tr))
                expr)))
    ((pair? expr)
     (visit-strings (car expr) callback within-tr)
     (visit-strings (cdr expr) callback within-tr))
    ((string? expr)
     (when within-tr
       (callback expr)))))

(defn merge-existing (old new)
  (map (lambda (kvp)
         (if-let ((existing (assoc (car kvp) old)))
             existing
           kvp))
       new))

(defn visit-file (path output)
  (let ((strings nil))
    (when-let ((exprs (read-file path)))
      (foreach (lambda (expr)
                 (visit-strings expr
                                (lambda (str)
                                  (let ((new-l (cons str strings)))
                                    (setq strings (union new-l new-l))))
                                false))
               exprs))
    (when-let ((result (map (lambda (k)
                              (cons k 'TODO))
                            strings)))
      (output (string-join (cddr (split path "/")) "/")
              result
              merge-existing))))

(defn visit-files (subdir output)
  (filesystem-walk
   subdir
   (lambda (path)
     (when (ends-with path ".lisp")
       (visit-file path output)))))


(lambda (output)
  (visit-files "/scripts/autoload/" output)
  (visit-files "/scripts/challenges/" output)
  (visit-files "/scripts/event/" output)
  (visit-files "/scripts/sandbox/" output)
  (visit-files "/scripts/tutorials/" output))

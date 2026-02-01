;;;
;;; async-test.lisp
;;;

(global 'put 'temp)

(setq put log)

(global 'current-test)

(defn assert-v (v)
  (when (not v)
    (error (format "In test %: assert failed! %" current-test v))))

(defn assert-eq (lhs rhs)
  (when (not (equal lhs rhs))
    (error (format "In test %: expected % not equal %"
                   current-test
                   lhs
                   rhs))))

(defn begin-test (name)
  (setq current-test name)
  (let ((msg (string "Running tests: " name "...")))
    (when (bound? 'regr-print)
      (regr-print msg 1 3))
    (put msg)))

(defn end-test ()
  (put " passed!")
  (setq current-test nil))


(global 'async-done)
(setq async-done false)

(defn bad ()
  3)

;; Cannot await a non-promise object
(assert-v (error? ((lambda () (await (bad))))))

(enter-stress-gc-mode)

(defn async-test ()
  (begin-test "basic...")
  (let ((x 0))
    (await (test-delay 5000))
    (+= x 1)
    (await (test-delay 2000))
    (+= x 3)
    (await (test-delay 2000))
    (assert-eq x 4))
  (end-test)

  (begin-test "test loop")
  (let ((num 0))
    (while (< num 20)
      (await (test-delay 80))
      (setq num (+ num 1)))
    (assert-eq num 20))
  (end-test)

  (begin-test "nested await")
  (assert-eq 5 (length ((lambda (n)
                          (await (test-delay 90))
                          (range (/ n 2)))
                        10)))
  (end-test)

  (exit-stress-gc-mode)
  (setq async-done true))

;; Turn off macro expansion for defn. Needed when transpiling function
;; declarations to C.
(setq defn-literal 0)

(macro defn (NAME BODY)
       (if (not defn-literal)
           `(set ,(cons $q NAME) (lambda ,@BODY))
         (list 'c-fn NAME BODY)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setq function-bindings '())
(setq function-definitions '())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn compile-file
  (let ((expr (read (readfile "transpile-test.lisp"))))
    (compile-expr expr)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn compile-expr
  (cond
   ((equal (car $0) 'c-fn)
    (compile-fn (cdr $0)))
   ((equal (car $0) 'if)
    (compile-if (cdr $0)))
   ((equal (car $0) 'let)
    (compile-expr (car (cdr (cdr $0)))))
   ((equal (cdr $0) '())
    "")
   ((equal (type (cdr $0)) 'pair)
    (compile-fncall $0))
   (1 $0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn compile-fncall
  (cond
   ((equal (car $0) '<)
    (string (compile-expr (get $0 1))
            (compile-expr (get $0 0))
            (compile-expr (get $0 2))))
   ((equal (car $0) '>)
    (string (compile-expr (get $0 1))
            (compile-expr (get $0 0))
            (compile-expr (get $0 2))))
   ((equal (car $0) 'equal)
    (format "% == %"
            (compile-expr (get $0 1))
            (compile-expr (get $0 2))))
   (1
    (compile-expr $0))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn compile-if
  (if (equal (length $0) 3)
      (format "[&] { if (%) { return %; } else { return %; }}()"
              (compile-expr (get $0 0))
              (compile-expr (get $0 1))
              (compile-expr (get $0 2)))
    (format "[&] { if (%) { return %; }}()"
            (compile-expr (get $0 0))
            (compile-expr (get $0 1)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setq var-tab '())


(defn lookup-typeinfo
  (let ((lat var-tab)
        (result '()))
    (while lat
      (let ((found (assoc $0 (car lat))))
        (if found
            (progn
              (setq result (cdr found))
              (setq lat '()))
          (setq lat (cdr lat)))))
    result))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn lookup-returntype
  (let ((loop 1)
        (expr (car $0))
        (result '()))
    (while loop
      (cond
       ((not (equal (type expr) 'pair))
        (setq result (lookup-typeinfo expr))
        (setq loop 0))
       ((equal (car expr) 'if)
        ;; (print (cdr (cdr (car expr))))
        (setq expr (car (cdr (cdr expr)))))
       ((equal (car expr) 'let)
        (setq expr (car (cdr (cdr expr)))))
       ;; TODO: lookup function call typeinfo
       (1
        (setq result "void")
        (setq loop 0))))
    result))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn compile-fn

  (setq var-tab (list (car (car (cdr $0)))))

  (let ((fname (format "lispc_%" (car $0)))
        (rt (lookup-returntype (cdr (car (cdr $0))))))
    (let ((sig (format "% %%"
                       rt
                       fname
                       (compile-fn-args (car (car (cdr $0))))))
          (body (compile-fn-body (cdr (car (cdr $0))))))

      (setq function-bindings
            (cons (list fname
                        sig
                        (car (car (cdr $0))))
                  function-bindings))

      (setq function-definitions
            (cons (list sig
                        body)
                  function-definitions))

      (setq var-tab '()))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn compile-fn-args
  (let ((str-params '())
        (first-arg 1))
    (map
     (lambda
       (setq str-params
             (cons (format "%% %"
                           (if first-arg
                               (progn
                                 (setq first-arg 0)
                                 "")
                             ", ")
                           (cdr $0)
                           (car $0))
                   str-params)))
     $0)
    (format "(%)" (apply string (reverse str-params)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn compile-fn-body
  (let ((expr-cnt (length $0))
        (i 0))
    (format "{%}" (apply string (map (lambda
                                       (+= i 1)
                                       (format
                                        (if (equal i expr-cnt)
                                            "return %;"
                                          "%;")
                                        (compile-expr $0)))
                                     $0)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(print "#include ")
(printd "lisp.hpp")
(print-ln)
(print-ln "#define STR(X) #X")
(print-ln)

;; From here on, defn will not be macro-expanded.
(setq defn-literal 1)
(compile-file)

(map
 (lambda
   (print (get $0 1))
   (print-ln ";")
   (print (format "lisp::Value* %%" (get $0 0) "_glue(int argc)"))
   (print-ln ";"))
 function-bindings)

;; function name binding table

(print-ln)
(print-ln "std::pair<const char*, lisp::Value* (*)(int)> lisp_glue[] = {")
(map
 (lambda
   (print "    // ")
   (print-ln (get $0 2))
   (print "    {")
   (print "STR(")
   (print (get $0 0))
   (print "_glue), ")
   (print (get $0 0))
   (print "_glue")
   (print-ln "},"))
 function-bindings)

(print-ln "};")
(print-ln)


(map
 (lambda
   (print-ln (get $0 0))
   (print-ln (get $0 1)))
 function-definitions)


(print-ln)

;; Generate gluecode allowing dynamic runtime lisp code to interoperate with
;; transpiled lisp code. I.e. any dynamic code can call natively compiled code
;; seamlessly.
(map
 (lambda
   (print-ln (format "lisp::Value* %%" (get $0 0) "_glue(int argc)"))
   (print-ln "{")
   (print "    L_EXPECT_ARGC(argc, ")
   (print (length (get $0 2)))
   (print-ln ");")
   (let ((n (- (length (get $0 2)) 1)))
     (map
      (lambda
        (cond
         ((equal (cdr $0) 's32)
          (print "    s32 ")
          (print (car $0))
          (print " = ")
          (print-ln (format "L_LOAD_INT(%);" n))
          (setq n (- n 1)))))
      (get $0 2)))
   (print "    ")
   (print (get $0 0))
   (print "(")
   (let ((n 0))
     (map
      (lambda
        (if (> n 0)
            (print ", "))
        (print (car $0))
        (+= n 1))
      (get $0 2)))
   (print-ln ");")
   (print-ln "    return L_NIL;")
   (print-ln "}"))
 function-bindings)


(print-ln)
(print-ln "// complete, memory stats:")
(gc)
(print "// ")
(print-ln (interp-stat))

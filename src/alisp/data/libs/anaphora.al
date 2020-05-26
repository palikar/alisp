(defvar --doc-- "Anaphoric function")



(defmacro anaphoric-if (cond then &rest else)
  "Like `if', but the result of evaluating COND is bound to `it'.
The variable `it' is available within THEN and ELSE.
COND, THEN, and ELSE are otherwise as documented for `if'."
  `(let ((it ,cond))
     (if it ,then ,@else)))

;;;###autoload
(defmacro anaphoric-prog1 (first &rest body)
  "Like `prog1', but the result of evaluating FIRST is bound to `it'.
The variable `it' is available within BODY.
FIRST and BODY are otherwise as documented for `prog1'."
  `(let ((it ,first))
     (progn ,@body)
     it))

;;;###autoload
(defmacro anaphoric-prog2 (form1 form2 &rest body)
  "Like `prog2', but the result of evaluating FORM2 is bound to `it'.
The variable `it' is available within BODY.
FORM1, FORM2, and BODY are otherwise as documented for `prog2'."
  `(progn
     ,form1
     (let ((it ,form2))
       (progn ,@body)
       it)))

;;;###autoload
(defmacro anaphoric-when (cond &rest body)
  "Like `when', but the result of evaluating COND is bound to `it'.
The variable `it' is available within BODY.
COND and BODY are otherwise as documented for `when'."
  `(anaphoric-if ,cond
       (progn ,@body)))

;;;###autoload
(defmacro anaphoric-while (test &rest body)
  "Like `while', but the result of evaluating TEST is bound to `it'.
The variable `it' is available within BODY.
TEST and BODY are otherwise as documented for `while'."
  `(do ((it ,test ,test)) ((not it))
     ,@body))

;;;###autoload
(defmacro anaphoric-and (&rest conditions)
  "Like `and', but the result of the previous condition is bound to `it'.
The variable `it' is available within all CONDITIONS after the
initial one.
CONDITIONS are otherwise as documented for `and'.
Note that some implementations of this macro bind only the first
condition to `it', rather than each successive condition."
  (cond
   ((null conditions)
    t)
   ((null (cdr conditions))
    (car conditions))
   (t
    `(anaphoric-if ,(car conditions) (anaphoric-and ,@(cdr conditions))))))

;;;###autoload
(defmacro anaphoric-cond (&rest clauses)
  "Like `cond', but the result of each condition is bound to `it'.
The variable `it' is available within the remainder of each of CLAUSES.
CLAUSES are otherwise as documented for `cond'."
  (if (null clauses)
      nil
    (let ((cl1 (car clauses))
          (sym (gensym)))
      `(let ((,sym ,(car cl1)))
         (if ,sym
             (if (null ',(cdr cl1))
                 ,sym
               (let ((it ,sym)) ,@(cdr cl1)))
           (anaphoric-cond ,@(cdr clauses)))))))

;;;###autoload
(defmacro anaphoric-lambda (args &rest body)
  "Like `lambda', but the function may refer to itself as `self'.
ARGS and BODY are otherwise as documented for `lambda'."
  `(cl-labels ((self ,args ,@body))
     #'self))


;;;###autoload
(defmacro anaphoric-case (expr &rest clauses)
  "Like `case', but the result of evaluating EXPR is bound to `it'.
The variable `it' is available within CLAUSES.
EXPR and CLAUSES are otherwise as documented for `case'."
  `(let ((it ,expr))
     (cl-case it ,@clauses)))


;;;###autoload
(defmacro anaphoric-let (form &rest body)
  "Like `let', but the result of evaluating FORM is bound to `it'.
FORM and BODY are otherwise as documented for `let'."
  `(let ((it ,form))
     (progn ,@body)))

;;;###autoload
(defmacro anaphoric-+ (&rest numbers-or-markers)
  "Like `+', but the result of evaluating the previous expression is bound to `it'.
The variable `it' is available within all expressions after the
initial one.
NUMBERS-OR-MARKERS are otherwise as documented for `+'."
  (cond
   ((null numbers-or-markers)
    0)
   (t
    `(let ((it ,(car numbers-or-markers)))
       (+ it (anaphoric-+ ,@(cdr numbers-or-markers)))))))

;;;###autoload
(defmacro anaphoric-- (&optional number-or-marker &rest numbers-or-markers)
  "Like `-', but the result of evaluating the previous expression is bound to `it'.
The variable `it' is available within all expressions after the
initial one.
NUMBER-OR-MARKER and NUMBERS-OR-MARKERS are otherwise as
documented for `-'."
  (cond
   ((null number-or-marker)
    0)
   ((null numbers-or-markers)
    `(- ,number-or-marker))
   (t
    `(let ((it ,(car numbers-or-markers)))
       (- ,number-or-marker (+ it (anaphoric-+ ,@(cdr numbers-or-markers))))))))

;;;###autoload
(defmacro anaphoric-* (&rest numbers-or-markers)
  "Like `*', but the result of evaluating the previous expression is bound to `it'.
The variable `it' is available within all expressions after the
initial one.
NUMBERS-OR-MARKERS are otherwise as documented for `*'."
  (cond
   ((null numbers-or-markers)
    1)
   (t
    `(let ((it ,(car numbers-or-markers)))
       (* it (anaphoric-* ,@(cdr numbers-or-markers)))))))

;;;###autoload
(defmacro anaphoric-/ (dividend divisor &rest divisors)
  "Like `/', but the result of evaluating the previous divisor is bound to `it'.
The variable `it' is available within all expressions after the
first divisor.
DIVIDEND, DIVISOR, and DIVISORS are otherwise as documented for `/'."
  (cond
   ((null divisors)
    `(/ ,dividend ,divisor))
   (t
    `(let ((it ,divisor))
       (/ ,dividend (* it (anaphoric-* ,@divisors)))))))



;; (anaphoric-if (+ 10 42)
;;     (println it))

;; (anaphoric-prog1 (+ 10 42)
;;   (println it))

;; (anaphoric-prog2 (+ 10 43) (+ 10 43)
;;   (println it))

;; (anaphoric-when (+ 10 42)
;;   (println it))
;; (defvar cnt 0)
;; (anaphoric-while (< cnt 10 )
;;   (println it)
;;   (1+ cnt))



(dump ((anaphoric-and 't it)))

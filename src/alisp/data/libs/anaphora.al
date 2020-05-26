(defvar --doc-- "Anaphoric expressions for, providing implicit temporary variables.")


(defmacro anaphoric-if (condition then &rest else)
  "(anaphoric-if condition then &rest else )

Like `if', but the result of evaluating COND is bound to `it'.
The variable `it' is available within THEN and ELSE.
COND, THEN, and ELSE are otherwise as documented for `if'."
  `(let ((it ,condition))
     (if it ,then ,@else)))

(defmacro anaphoric-prog1 (first &rest body)
  "(anaphoric-prog1 first &rest body )

Like `prog1', but the result of evaluating FIRST is bound to `it'.
The variable `it' is available within BODY.
FIRST and BODY are otherwise as documented for `prog1'."
  `(let ((it ,first))
     (progn ,@body)
     it))

(defmacro anaphoric-prog2 (form1 form2 &rest body)
  "(anaphoric-prog2 form1 form2 &rest body )

Like `prog2', but the result of evaluating FORM2 is bound to `it'.
The variable `it' is available within BODY.
FORM1, FORM2, and BODY are otherwise as documented for `prog2'."
  `(progn
     ,form1
     (let ((it ,form2))
       (progn ,@body)
       it)))

(defmacro anaphoric-when (condition &rest body)
  "(anaphoric-when condition &rest body )

Like `when', but the result of evaluating COND is bound to `it'.
The variable `it' is available within BODY.
COND and BODY are otherwise as documented for `when'."
  `(anaphoric-if ,condition
       (progn ,@body)))

(defmacro anaphoric-and (&rest conditions)
  "(anaphoric-and &rest conditions )

Like `and', but the result of the previous condition is bound to `it'.
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

(defmacro anaphoric-cond (&rest clauses)
  "(anaphoric-cond &rest clauses )

Like `cond', but the result of each condition is bound to `it'.
The variable `it' is available within the remainder of each of CLAUSES.
CLAUSES are otherwise as documented for `cond'."
  (if (null clauses)
      nil
    (let ((cl1 (car clauses))
          sym)
      `(let ((sym ,(car cl1)))
         (if sym
             (if (null ',(cdr cl1))
                 sym
               (let ((it sym)) ,@(cdr cl1)))
           (anaphoric-cond ,@(cdr clauses)))))))

(defmacro anaphoric-let (form &rest body)
  "(anaphoric-let form &rest body )

Like `let', but the result of evaluating FORM is bound to `it'.
FORM and BODY are otherwise as documented for `let'."
  `(let ((it ,form))
     (progn ,@body)))

(defmacro anaphoric-+ (&rest numbers-or-markers)
  "(anaphoric-+ &rest numbers-or-markers )

Like `+', but the result of evaluating the previous expression is bound to `it'.
The variable `it' is available within all expressions after the
initial one.
NUMBERS-OR-MARKERS are otherwise as documented for `+'."
  (cond
   ((null numbers-or-markers)
    0)
   (t
    `(let ((it ,(car numbers-or-markers)))
       (+ it (anaphoric-+ ,@(cdr numbers-or-markers)))))))

(defmacro anaphoric-- (&optional number-or-marker &rest numbers-or-markers)
  "(anaphoric-- &optional number-or-marker &rest numbers-or-markers )

Like `-', but the result of evaluating the previous expression is bound to `it'.
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

(defmacro anaphoric-* (&rest numbers-or-markers)
  "(anaphoric-* &rest numbers-or-markers )

Like `*', but the result of evaluating the previous expression is bound to `it'.
The variable `it' is available within all expressions after the
initial one.
NUMBERS-OR-MARKERS are otherwise as documented for `*'."
  (cond
   ((null numbers-or-markers)
    1)
   (t
    `(let ((it ,(car numbers-or-markers)))
       (* it (anaphoric-* ,@(cdr numbers-or-markers)))))))

(defmacro anaphoric-/ (dividend divisor &rest divisors)
  "(anaphoric-/ dividend divisor &rest divisors )

Like `/', but the result of evaluating the previous divisor is bound to `it'.
The variable `it' is available within all expressions after the
first divisor.
DIVIDEND, DIVISOR, and DIVISORS are otherwise as documented for `/'."
  (cond
   ((null divisors)
    `(/ ,dividend ,divisor))
   (t
    `(let ((it ,divisor))
       (/ ,dividend (* it (anaphoric-* ,@divisors)))))))



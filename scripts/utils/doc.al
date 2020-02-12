(import 'fileio)

(defun heading-1 (name)
  "Creates a heading"
  (println "# " name))

(defun heading-2 (name)
  "Creates a heading"
  (println "## " name))

(defun heading-3 (name)
  "Creates a heading"
  (println "### " name))

(defun bold (name)
  (print "*" name "*"))

(defun dump-doc-list (&rest sym)
  (dolist (el sym)    
    (print "+ ")
    (bold (prop-get el "--name--" ))
    (print ": ")
    (let ((lines (string-splitlines (prop-get el "--doc--" ))))
      (print (nth lines 0) "\n")
      (mapc println (tail lines)))
    (print "\n")))

(defmacro expand-and-dump (syms)
  `(dump-doc-list ,@(eval syms)))

(defvar constructs-preamble "Basic fuctions that provide the backbone of the language. These include global and local variable definition, flow control structures and loops.")
(defvar language-constructs-list
  '(import
    modref
    defun
    eval
    setq
    set
    setq
    quote
    function
    lambda
    if
    while
    dolist
    cond
    when
    unless
    let
    let*
    or
    and
    not
    parse-int
    parse-float
    to-string
    to-char
    funcall
    backquote
    return
    exit))

(defvar printing-preamble "Functions to interact with the stanard input and output.")
(defvar printing-list
  '(
    print
    println
    eprint
    eprintln
    read-line
    ))

(defvar lists-preamble "Functions to interact with the stanard input and output.")
(defvar lists-list
  '(
    length
    cons
    head
    last
    init
    tail
    nth
    mapc
    mapcar
    push
    delete
    remove
    range))

(defvar props-preamble "")
(defvar props-list '(prop-get))

(defvar predicates-preamble "")
(defvar predicates-list '(pstring))

(defvar strings-preamble "")
(defvar strings-list '(string-length))

(defvar math-preamble "")
(defvar math-list '(+ - / *))

(defvar alg-preamble "")
(defvar alg-list '(sort))

(heading-2 "Language constructs")
(println "\n" constructs-preamble "\n")
(expand-and-dump language-constructs-list)

(heading-2 "Printing")
(println "\n" printing-preamble "\n")
(expand-and-dump printing-list)

(heading-2 "Lists")
(println "\n" lists-preamble "\n")
(expand-and-dump lists-list)

(heading-2 "Object Properties")
(println "\n" props-preamble "\n")
(expand-and-dump props-list)

(heading-2 "Object Properties")
(println "\n" predicates-preamble "\n")
(expand-and-dump predicates-list)

(heading-2 "Strings")
(println "\n" strings-preamble "\n")
(expand-and-dump strings-list)

(heading-2 "Basic Math")
(println "\n" math-preamble "\n")
(expand-and-dump math-list)

(heading-2 "Algorithms")
(println "\n" alg-preamble "\n")
(expand-and-dump alg-list)



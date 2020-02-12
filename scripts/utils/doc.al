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



(heading-2 "Language constructs")
(println "\n" constructs-preamble "\n")
(expand-and-dump language-constructs-list)

(heading-2 "Printing")
(println "\n" printing-preamble "\n")
(expand-and-dump printing-list)




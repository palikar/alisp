;; (import 'fileio)
;; (import 'math)
;; (import 'memory)
;; (import 'platform)
;; (import 'system)
;; (import 'time)

;; (defun heading-1 (name)
;;   "Creates a heading"
;;   (println "# " name))

;; (defun heading-2 (name)
;;   "Creates a heading"
;;   (println "## " name))

;; (defun heading-3 (name)
;;   "Creates a heading"
;;   (println "### " name))

;; (defun warning (name)
;;   "Creates a warning"
;;   (print "!!! Warning\n\t" name "\n"))

;; (defun tip (name)
;;   "Creates a not"
;;   (print "!!! Tip\n\t" name "\n"))

;; (defun link (to text)
;;   "Creates a not"
;;   (print "[" text "](" to ")" ))

;; (defun line (&optional n)
;;   (if n (dolist (el (range 0 n))
;;           (print "\n"))
;;     (print "\n")))

;; (defun bold (name)
;;   (print "**" name "**"))

;; (defun dump-doc-list (&rest sym)
;;   (dolist (el sym)    
;;     (print "- ###")
;;     (bold (prop-get el "--name--" ))
;;     (print " : ")
;;     (let ((lines (string-splitlines (prop-get el "--doc--" ))))
;;       (print "*" (nth lines 0) "*" "\n")
;;       (mapc println (tail lines)))
;;     (print "\n")))

;; (defmacro expand-and-dump (syms)
;;   `(dump-doc-list ,@(eval syms)))

;; (defvar root-dir "")
;; (if (== 0 (length --argv--)) (exit 1)
;;   (setq root-dir (fileio.f-canonical (nth --argv-- 0))))

;; (defvar constructs-preamble "Basic fuctions that provide the backbone of the language. These include global and local variable definition, flow control structures and loops.")
;; (defvar language-constructs-list
;;   '(import
;;     modref
;;     defun
;;     eval
;;     setq
;;     set
;;     setq
;;     quote
;;     function
;;     lambda
;;     if
;;     while
;;     dolist
;;     cond
;;     when
;;     unless
;;     let
;;     let*
;;     or
;;     and
;;     not
;;     parse-int
;;     parse-float
;;     to-string
;;     to-char
;;     funcall
;;     backquote
;;     return
;;     exit))

;; (defvar printing-preamble "Functions to interact with the stanard input and output.")
;; (defvar printing-list
;;   '(print
;;     println
;;     eprint
;;     eprintln
;;     read-line))

;; (defvar lists-preamble "Functions to interact with the stanard input and output.")
;; (defvar lists-list
;;   '(
;;     length
;;     cons
;;     head
;;     last
;;     init
;;     tail
;;     nth
;;     mapc
;;     mapcar
;;     push
;;     delete
;;     remove
;;     range))

;; (defvar props-preamble "Functions for accessing the properties of objects.")
;; (defvar props-list '(prop-get prop-set prop-list))

;; (defvar predicates-preamble "Functions for type inspecting. These functions can be used to check whether an object is from a certain type.")
;; (defvar predicates-list '(pstring plist pint preal psym pfunction))

;; (defvar strings-preamble "Functions for basic string handling.")
;; (defvar strings-list
;;   '(string-length
;;     string-contains
;;     string-endswith
;;     string-startswith
;;     string-length
;;     string-capitalize
;;     string-find
;;     string-replace
;;     string-replaceall
;;     string-split
;;     string-substring
;;     string-splitlines
;;     string-upper
;;     string-lower
;;     string-strip
;;     string-join
;;     char-isalpha
;;     char-isdigit))

;; (defvar math-preamble "Functions that realise simple math operations.")
;; (defvar math-list '(+ - / * < <= > >= == != mod pow min max round))

;; (defvar alg-preamble "Several functions of basic algorithms for working with lists.")
;; (defvar alg-list '(slice sort sort zip filter any all))

;; (defun generate-basic-reference ()
;;   (heading-1 "Basic builtin functions.")

;;   (heading-2 "Language constructs")
;;   (println "\n" constructs-preamble "\n")
;;   (expand-and-dump language-constructs-list)

;;   (heading-2 "Printing")
;;   (println "\n" printing-preamble "\n")
;;   (expand-and-dump printing-list)

;;   (heading-2 "Lists")
;;   (println "\n" lists-preamble "\n")
;;   (expand-and-dump lists-list)

;;   (heading-2 "Object Properties")
;;   (println "\n" props-preamble "\n")
;;   (expand-and-dump props-list)

;;   (heading-2 "Object Properties")
;;   (println "\n" predicates-preamble "\n")
;;   (expand-and-dump predicates-list)

;;   (heading-2 "Strings")
;;   (println "\n" strings-preamble "\n")
;;   (expand-and-dump strings-list)

;;   (heading-2 "Basic Math")
;;   (println "\n" math-preamble "\n")
;;   (expand-and-dump math-list)

;;   (heading-2 "Algorithms")
;;   (println "\n" alg-preamble "\n")
;;   (expand-and-dump alg-list))

;; (defun generate-streams-reference ()

;;   (heading-1 "Streaming system.")
;;   (line)
;;   (println "Alisp provides a mechanism for working with streams. Streams are abstraction that supports writing and reading and provide unified interface for these operation. In ALisp streams are handled through *resource objects*. This means that every strema is identified through a int-object that acts like a pointer to the underlying stream. The intrpterer keeps track of every opened stream and provides acces to each of them throught the resource object (the int value).")
;;   (line)

;;   (let ((basic-streams-list '(stream stream-close))
;;         (basic-streams-preamble "These functions are used to open and close a stream."))
;;     (heading-2 "Opening and closing streams")
;;     (println "\n" basic-streams-preamble "\n")
;;     (warning "Every stream that was opened must be closed. If a stream is not closed, memory could be leaked.")
;;     (line)
;;     (expand-and-dump basic-streams-list))

;;   (let ((writing-streams-list '(stream-write stream-write-line stream-write-lines ))
;;         (writing-streams-preamble "Functions for writing to streams"))
;;     (heading-2 "Writing.")
;;     (println "\n" writing-streams-preamble "\n")
;;     (line)
;;     (expand-and-dump writing-streams-list))

;;   (let ((reading-streams-list '(stream-read stream-read-line stream-read-lines))
;;         (reading-streams-preamble "Functions for reading from streams"))
;;     (heading-2 "Reading.")
;;     (println "\n" reading-streams-preamble "\n")
;;     (line)
;;     (expand-and-dump reading-streams-list))

;;   (let ((redirecting-streams-list '(with-cout with-cin))
;;         (redirecting-streams-preamble "The sandard input and output streams of the process can be redirected from any compatable stream. This way, you can use functions that will normaly pring to the standard output, but have the stream redirected to a file for example."))
;;     (heading-2 "Redirecting standrad output and input.")
;;     (println "\n" redirecting-streams-preamble "\n")
;;     (line)
;;     (expand-and-dump redirecting-streams-list))

;;   (heading-2 "Utilities")
;;   (line)
;;   (print "\n" "Some utility functions for working with streams" "\n")
;;   (line)
;;   (expand-and-dump '(stream-content)))

;; (defun generate-files-reference ()
;;   (heading-1 "File system.")
;;   (line)
;;   (println "Similar to stream, files are also resource objects. From a programmer perspecive a file is accesed through a poineter line integer object. As in other languages, files can be opend, closed, written to and read from.")
;;   (line)

;;   (let ((basic-files-list '(file-open file-close))
;;         (basic-files-preamble "Opening and closing a file is done through these two simple to use functions."))
;;     (heading-2 "Opening and closing files")
;;     (println "\n" basic-files-preamble "\n")
;;     (warning "Every file that was opened must be closed. If a file is not closed properly, memory could be leaked.")
;;     (line)
;;     (expand-and-dump basic-files-list))


;;   (let ((io-files-list '(file-read-line file-write-line file-has-more))
;;         (io-files-preamble ""))
;;     (heading-2 "Funcitons for basic reading from and writing to files.")
;;     (println "\n" io-files-preamble "\n")
;;     (tip "If you need more reading and writing functions, attach a stram to the file and work the the stream itself.")
;;     (line)
;;     (expand-and-dump io-files-list)))

;; (defun generate-modules-index ()
;;   (heading-2 "Builtin Modules")
;;   (line)

;;   (let ( (modules '(fileio math memory platform system time))
;;          (preamble "Builtin Modules are built into the Alisp interpreter and can be always imported. These modules are meant to provide common functionality like working with files, basic OS-operations, math functions, etc."))
;;     (println preamble)
;;     (line)

;;     (dolist (mod modules)
;;       (print "* ")
;;       (link (string-join "./modules/" (to-string mod) ".md") (to-string mod))
;;       (print " - " (modref mod '--doc--))
;;       (line))))

(defun generate-fileio-module ()
  "assad"
  )

;; (println "Genrating basic...: " "basic_doc.md")
;; (std-redirect (fileio.f-join root-dir "basic_doc.md") (generate-basic-reference))

;; (println "Genrating steams...: " "streams_doc.md")
;; (std-redirect (fileio.f-join root-dir "streams_doc.md") (generate-streams-reference))

;; (println "Genrating files...: " "files_doc.md")
;; (std-redirect (fileio.f-join root-dir "files_doc.md") (generate-files-reference))

;; (println "Generating builting modules index..." "modules/index.md")
;; (std-redirect (fileio.f-join root-dir "modules/index.md") (generate-modules-index))

(generate-fileio-module)

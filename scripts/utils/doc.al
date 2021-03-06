(import 'fileio)
(import 'math)
(import 'memory)
(import 'platform)
(import 'system)
(import 'time)
(import 'json)

(import 's)
(import 'dash)
(import 'stack)
(import 'queue)
(import 'setc)

(import 'base64)
(import 'fmt)
(import 'func)
(import 'http)
(import 'json)
(import 'xml)
(import 'locale)
(import 'process)
(import 'random)
(import 're)

                                        ; Markddown utils

(defun heading-1 (name)
  "Creates a heading"
  (println "# " name))

(defun heading-2 (name)
  "Creates a heading"
  (println "## " name))

(defun heading-3 (name)
  "Creates a heading"
  (println "### " name))

(defun heading-4 (name)
  "Creates a heading"
  (println "#### " name))

(defun warning (text)
  "Creates a warning"
  (print "!!! Warning\n\t" text "\n"))

(defun tip (text)
  "Creates a not"
  (print "!!! Tip\n\t" text "\n"))

(defun link (to text)
  "Creates a not"
  (print "[" text "](" to ")" ))

(defun line (&optional n)
  (if n (dolist (el (range 0 n))
          (print "\n"))
    (print "\n")))

(defun bold (name)
  (print "**" name "**"))


(defun dump-doc-list (&rest sym)
  (dolist (el sym)
    (let ((name (prop-get el "--name--" )))
      (when (not (string-startswith name "--"))
        (bold name)
        (print " : ")
        (let ((lines (string-splitlines (prop-get el "--doc--" ))))
          (print "*" (nth lines 0) "*" "\n")
          (mapc println (tail lines)))))
    (print "\n")))

(defun dump-doc-list-var (&rest sym)
  (dolist (el sym)
    (let ((name (prop-get el "--name--" )))
      (when (not (string-startswith name "--"))
        (bold name)
        (print " : ")
        (print (prop-get el "--doc--" ))))
    (print "\n")))


(defmacro expand-and-dump (syms)
  `(dump-doc-list ,@(eval syms)))




(defun generate-basic-reference ()

  (heading-1 "Basic builtin functions.")

  (let ((constructs-preamble (prop-get --language-all-- "--doc--"))
        (language-constructs-list --language-all--))
    (heading-2 "Language constructs")
    (println "\n" constructs-preamble "\n")
    (expand-and-dump language-constructs-list))

  (let ((printing-preamble (prop-get --printing-all-- "--doc--"))
        (printing-list --printing-all--))
    (heading-2 "Printing")
    (println "\n" printing-preamble "\n")
    (expand-and-dump printing-list))

  (let ((lists-preamble (prop-get --lists-all-- "--doc--"))
        (lists-list --lists-all--))
    (heading-2 "Lists")
    (println "\n" lists-preamble "\n")
    (expand-and-dump lists-list))
  
  (let ((props-preamble (prop-get --props-all-- "--doc--"))
        (props-list --props-all--))
    (heading-2 "Object Properties")
    (println "\n" props-preamble "\n")
    (expand-and-dump props-list))

  (let ((predicates-preamble (prop-get --predicates-all-- "--doc--"))
        (predicates-list --predicates-all--))
    (heading-2 "Object predicates")
    (println "\n" predicates-preamble "\n")
    (expand-and-dump predicates-list))

  (let ((strings-preamble (prop-get --strings-all-- "--doc--"))
        (strings-list --strings-all--))
    (heading-2 "Strings")
    (println "\n" strings-preamble "\n")
    (expand-and-dump strings-list))

  (let ((casts-preamble (prop-get --casts-all-- "--doc--"))
        (casts-list --casts-all--))
    (heading-2 "Casting")
    (println "\n" casts-preamble "\n")
    (expand-and-dump casts-list))

  (let ((math-preamble (prop-get --math-all-- "--doc--"))
        (math-list --math-all--))
    (heading-2 "Basic Math")
    (println "\n" math-preamble "\n")
    (expand-and-dump math-list))

  (let ((logic-preamble (prop-get --logic-all-- "--doc--"))
        (logic-list --logic-all--))
    (heading-2 "Logical operations")
    (println "\n" logic-preamble "\n")
    (expand-and-dump logic-list))

  (let ((alg-preamble (prop-get --alg-all-- "--doc--"))
        (alg-list --alg-all--))
    (heading-2 "Algorithms")
    (println "\n" alg-preamble "\n")
    (expand-and-dump alg-list))

  )

(defun generate-streams-reference ()

  (heading-1 "Streaming system.")
  (line)
  (println "Alisp provides a mechanism for working with streams. Streams are abstraction that supports writing and reading and provide unified interface for these operation. In ALisp streams are handled through *resource objects*. This means that every strema is identified through a int-object that acts like a pointer to the underlying stream. The intrpterer keeps track of every opened stream and provides acces to each of them throught the resource object (the int value).")
  (line)

  (let ((basic-streams-list '(stream stream-close))
        (basic-streams-preamble "These functions are used to open and close a stream."))
    (heading-2 "Opening and closing streams")
    (println "\n" basic-streams-preamble "\n")
    (warning "Every stream that was opened must be closed. If a stream is not closed, memory could be leaked.")
    (line)
    (expand-and-dump basic-streams-list))

  (let ((writing-streams-list '(stream-write stream-write-line stream-write-lines ))
        (writing-streams-preamble "Functions for writing to streams"))
    (heading-2 "Writing.")
    (println "\n" writing-streams-preamble "\n")
    (line)
    (expand-and-dump writing-streams-list))

  (let ((reading-streams-list '(stream-read stream-read-line stream-read-lines))
        (reading-streams-preamble "Functions for reading from streams"))
    (heading-2 "Reading.")
    (println "\n" reading-streams-preamble "\n")
    (line)
    (expand-and-dump reading-streams-list))

  (let ((redirecting-streams-list '(with-cout with-cin))
        (redirecting-streams-preamble "The sandard input and output streams of the process can be redirected from any compatable stream. This way, you can use functions that will normaly pring to the standard output, but have the stream redirected to a file for example."))
    (heading-2 "Redirecting standrad output and input.")
    (println "\n" redirecting-streams-preamble "\n")
    (line)
    (expand-and-dump redirecting-streams-list))

  (heading-2 "Utilities")
  (line)
  (print "\n" "Some utility functions for working with streams" "\n")
  (line)
  (expand-and-dump '(stream-content)))

(defun generate-files-reference ()
  (heading-1 "File system.")
  (line)
  (println "Similar to stream, files are also resource objects. From a programmer perspecive a file is accesed through a poineter line integer object. As in other languages, files can be opend, closed, written to and read from.")
  (line)

  (let ((basic-files-list '(file-open file-close))
        (basic-files-preamble "Opening and closing a file is done through these two simple to use functions."))
    (heading-2 "Opening and closing files")
    (println "\n" basic-files-preamble "\n")
    (warning "Every file that was opened must be closed. If a file is not closed properly, memory could be leaked.")
    (line)
    (expand-and-dump basic-files-list))


  (let ((io-files-list '(file-read-line file-write-line file-has-more))
        (io-files-preamble ""))
    (heading-2 "Funcitons for basic reading from and writing to files.")
    (println "\n" io-files-preamble "\n")
    (tip "If you need more reading and writing functions, attach a stram to the file and work the the stream itself.")
    (line)
    (expand-and-dump io-files-list)))

(defun generate-modules-index ()
  (heading-2 "Builtin Modules")
  (line)
  (let ( (modules '(fileio math memory platform system time
                           json base64 fmt func json xml locale process
                           re dash s stack queue setc))
         (preamble "Builtin Modules are built into the Alisp interpreter and can be always imported. These modules are meant to provide common functionality like working with files, basic OS-operations, math functions, etc."))
    (println preamble)
    (line)

    (dolist (mod modules)
      (print "* ")
      (link (string-join "" (to-string mod) "") (to-string mod))
      (line))))





(defmacro generate-module (title module)
  (let* ((syms (symbols-list module))
         (len (length syms)))
    (heading-3 title)
    (line)
    (heading-4 "Description")
    (line)
    (when (contains (symbols-list module) '--doc--)
      (print (modref module '--doc--)))
    
    (line)
    (heading-4 "Functions")
    (line)
    (dolist (i (range 0 len))
      (let ((sym (modref module (nth syms i))))
        (when (pfunction sym)
          (dump-doc-list sym))))

    (heading-4 "Constants")
    (dolist (i (range 0 len))
      (let ((sym (modref module (nth syms i))))
        (when (or (pint sym) (pstring sym) (preal sym) (psym sym))
          (dump-doc-list-var sym)
          (line))))))




(defvar root-dir
  (if (== 0 (length --argv--))
      (progn
        (println "Usage: doc.al <root_dir>")
        (exit 1))
    (fileio.f-canonical (nth --argv-- 0))))

;; Basic language documentation
(println "Genrating basic...: " "basic_doc.md")
(std-redirect (fileio.f-join root-dir "basic_doc.md") (generate-basic-reference))

(println "Genrating steams...: " "streams_doc.md")
(std-redirect (fileio.f-join root-dir "streams_doc.md") (generate-streams-reference))

(println "Genrating files...: " "files_doc.md")
(std-redirect (fileio.f-join root-dir "files_doc.md") (generate-files-reference))



;; Index page for the modules
(println "Generating modules index...: " "modules/index.md")
(std-redirect (fileio.f-join root-dir "modules/index.md") (generate-modules-index))


;; Index page for the modules
(println "Generating module...: " "modules/fileio.md")
(std-redirect (fileio.f-join root-dir "modules/fileio.md") (generate-module "Filio" fileio))

(println "Generating module...: " "modules/system.md")
(std-redirect (fileio.f-join root-dir "modules/system.md") (generate-module "System" system))

(println "Generating module...: " "modules/math.md")
(std-redirect (fileio.f-join root-dir "modules/math.md") (generate-module "Math" math))

(println "Generating module...: " "modules/memory.md")
(std-redirect (fileio.f-join root-dir "modules/memory.md") (generate-module "Memory" memory))

(println "Generating module...: " "modules/platform.md")
(std-redirect (fileio.f-join root-dir "modules/platform.md") (generate-module "Platform" platform))

(println "Generating module...: " "modules/time.md")
(std-redirect (fileio.f-join root-dir "modules/time.md") (generate-module "Time" time))


(println "Generating module...: " "modules/base64.md")
(std-redirect (fileio.f-join root-dir "modules/base64.md") (generate-module "Base64" base64))

(println "Generating module...: " "modules/fmt.md")
(std-redirect (fileio.f-join root-dir "modules/fmt.md") (generate-module "Fmt" fmt))

(println "Generating module...: " "modules/func.md")
(std-redirect (fileio.f-join root-dir "modules/func.md") (generate-module "Func" func))

(println "Generating module...: " "modules/json.md")
(std-redirect (fileio.f-join root-dir "modules/json.md") (generate-module "Json" json))

(println "Generating module...: " "modules/xml.md")
(std-redirect (fileio.f-join root-dir "modules/xml.md") (generate-module "XML" xml))

(println "Generating module...: " "modules/locale.md")
(std-redirect (fileio.f-join root-dir "modules/locale.md") (generate-module "Locale" locale))

(println "Generating module...: " "modules/process.md")
(std-redirect (fileio.f-join root-dir "modules/process.md") (generate-module "Process" process))

(println "Generating module...: " "modules/random.md")
(std-redirect (fileio.f-join root-dir "modules/random.md") (generate-module "Random" random))

(println "Generating module...: " "modules/re.md")
(std-redirect (fileio.f-join root-dir "modules/re.md") (generate-module "re" re))



(println "Generating module...: " "modules/dash.md")
(std-redirect (fileio.f-join root-dir "modules/dash.md") (generate-module "Dash" dash))

(println "Generating module...: " "modules/s.md")
(std-redirect (fileio.f-join root-dir "modules/s.md") (generate-module "S" s))

(println "Generating module...: " "modules/stack.md")
(std-redirect (fileio.f-join root-dir "modules/stack.md") (generate-module "Stack" stack))

(println "Generating module...: " "modules/queue.md")
(std-redirect (fileio.f-join root-dir "modules/queue.md") (generate-module "Queue" queue))

(println "Generating module...: " "modules/setc.md")
(std-redirect (fileio.f-join root-dir "modules/setc.md") (generate-module "Setc" setc))

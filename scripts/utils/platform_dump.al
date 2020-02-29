(import 'platform :all)

(print "The current Alisp is running on " os " and its version is " alisp-version ". The interpteter was compiled with " compiler-name " and the version of the compiler has been " compiler-version ". The architecture of the host machine is " arch ".\n")

(print "\n")
(if debug-build
    (print "This is a debug build. ")
  (print "This is a release build. "))

(print "The interpreter has been compiled ")
(if enabled-stack-trace
    (print "with stack tracing support, ")
  (print "without stack tracing support, "))

(if enabled-documentation
    (print "documentation was not included in build and")
  (print "documentation was included in build and "))

(if enabled-line-trace
    (print "line tracing for list objects is enabled.")
  (print "line tracing for list objects is enabled."))

(if disabled-checks
    (print "No run time checks are preformed while evaluating forms. ")
  (print "Runtime checks are performed while evaluating forms. "))

(print "The maximum call depth is set to " max-evaluation-depth " while the maximum evalution depth is " max-evaluation-depth ".\n")

(println "\nBuild info:\n" build-info)

(println "\nThe Alisp interpreter is free and open source software distributed under the " --al-license-- " license.")

(println "\nCredits:")
(dumpcredits)

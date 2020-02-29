(import 'platform :all)

(print "The current Alisp is running on " os " and its version is " alisp-version ". The interpteter was compiled with " compiler-name " and the version of the compiler has been " compiler-version ". The architecture of the host machine is " arch ". ")

(if debug-build
    (print "This is a debug build")
  (print "This is a release build"))

(print ", the maximum call depth is set to " max-evaluation-depth " while the maximum evalution depth is " max-evaluation-depth ".\n")


(println "\n\nThe Alisp interpreter is free and open source software distributed under the " --al-license-- " license.")

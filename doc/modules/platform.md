### Platform

#### Description

The `platform` module exposes infomration about the Alisp
interpreter, the underlying operating system and information about it
as well as how the intrpterter was compiled.#### Functions

#### Constants
**enabled-stack-trace** : 

`t` if the interpreter was compiled with support for keeping of the
called functions. If stack tracing is enabled, on error the
interpreter will print out the state of the stack at the moment of the
error.


**enabled-stack-trace** : 

`t` if the interpreter was compiled with support for keeping of the
called functions. If stack tracing is enabled, on error the
interpreter will print out the state of the stack at the moment of the
error.


**enabled-stack-trace** : 

`t` if the interpreter was compiled with support for keeping of the
called functions. If stack tracing is enabled, on error the
interpreter will print out the state of the stack at the moment of the
error.


**disabled-checks** : 

`t` if the interpreter was compiled without checks of argument types
by functions. This includes arity checks as well some other sanity
checks that keep the interpreter stable. Without those, you can expect
segmentaion faults when the interpreted code is invalid. However,
disabling checkes may or may not increase performance.




**os** : 

The name of the current os. The value can be:
* linux
* windows-32
* windows-64
* max-osx
* max-osx
* freebsd
* unix
* unknown



**alisp-version** : 

A string of the version of the alisp interpreter.


**alisp-version-minor** : 

The minor versin of the alisp interpreter.


**max-evaluation-depth** : 

The maximum number of evalution nesting that the evaluator
supports. This is the depth limit of drcptrddoind in alisp.


**alisp-version-major** : 

The major versin of the alisp interpreter.


**alisp-version-patch** : 

The patch versin of the alisp interpreter.


**max-call-depth** : 

The maximum number of function calls that can be nested in one another.


**compiler-name** : 

The name of the compiler with which the interpreter was compiled.


**enabled-stack-trace** : 

`t` if the interpreter was compiled with support for keeping of the
called functions. If stack tracing is enabled, on error the
interpreter will print out the state of the stack at the moment of the
error.


**compiler-version** : 

The vesion of the compiler (in a string format) with which the interpreter was compiled.


**arch** : 

The computer architecture that the interpreter is running on. Possible
values are:
* i386
* x86_64
* arm
* power64pc
* aarch64
* unknown

 

**build-info** : 

Info string about the build of the interpreter. This gets printed out
when the interpreter is started.



### Process

#### Description

The `process` module enables the starting and communicating with
external processes. It is similar to the `subprocess` module of
pyhton. The module tries, in fact, to stay close the the api and
privide similar functions for starting and communicating with external
processes.

Internaly `process` uses the [cpp-subprocess](https://github.com/arun11299/cpp-subprocess)
library.

#### Functions

**check-output-bytes** : *(check-output [COMMAND_PART]...)*

Convenience function. Execute the command with the given parts and
return the contents of the standard output as a byte array.


**check-output** : *(check-output [COMMAND_PART]...)*

Convenience function. Execute the command with the given parts and
return the contents of the standard output of the process once its
finished.


**wait** : *(wait PROCESS)*

Block until a process has finished its execution.


**popen** : *(open COMMAND_PARTS OPTIONS )*

Execute a process. `COMMAND_PARTS` must be a list of strings that will
become the parts of the command that should be executed.

`OPTIONS` is also a list with options on how to execute the
process. Possible options are:

  * `:defer` - if present, don't start the process immediately but only when the `start` function is called.
  * `:buff-size` - the buffer size of the stdin/stdout/stderr streams of the child process. Default value is 0.
  * `:close-fds` - if present, close all file descriptors when the child process is spawned.
  * `:cwd` - the working directory where the process should be executed.
  * `:shell` - if present, spawn the process in a sub-shell.
  * `:env` - a list of pairs `(VAR VALUE)`. For the spawned process, the env variable `VAR` will be set to `VALUE`.
  * `:input` - specify the input channel fot the child process. This can be `pipe`\`stdout`\`stderr` or a file name.
  * `:output` - specify the output channel fot the child process. This can be `pipe`\`stdout`\`stderr` or a file name.
  * `:error` - specify the error channel fot the child process. This can be `pipe`\`stdout`\`stderr` or a file name.

Return the new process as a resource object.


**start** : *(start PROCESS)*

Start a process that has been created with `open`.


**send** : *(send PROCESS STRING)*

Write a string to the standard input stream of a child process.


**pid** : *(pid PROCESS)*

Return the process id of a process that has been created with `open`.


**poll** : *(poll PROCESS)*



**kill** : *(kill PROCESS [SIGNAL])*

Send a signal (by default SIGKILL) to a running process.


**call** : *(check-output [COMMAND_PART]...)*

Convenience function. Execute the command with the given parts and
return the exit code of the process once its finished.


**retcode** : *(retcode PROCESS)*

Wait for a process to finish and return its return code.


**communicate** : *(communicate PROCESS STRING)*

Write a string to the standard input stream of a child process. Return
the contents of the standard output and standard error of the process.


#### Constants
**stdout** : Symbol used to signify the standard output stream. It is used in some of the functions of the module. 

**stderr** : Symbol used to signify the standard input stream. It is used in some of the functions of the module. 



**pipe** : Symbol used to signify a link between the spawn process and the interpreter. It is used in some of the functions of the module. 


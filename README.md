[![Codacy Badge](https://api.codacy.com/project/badge/Grade/283df2678f0a4eaeb93c35b48ed0c483)](https://app.codacy.com/manual/palikar/alisp?utm_source=github.com&utm_medium=referral&utm_content=palikar/alisp&utm_campaign=Badge_Grade_Dashboard)
[![Documentation Status](https://readthedocs.org/projects/alisp/badge/?version=latest)](https://alisp.readthedocs.io/en/latest/?badge=latest)


# ALisp

![img](./logo.png)


## Abstract

ALisp is a interpreted [Lisp Scheme](https://en.wikipedia.org/wiki/Scheme_(programming_language)) language. I&rsquo;ve developed it on my own around 2019-2020. This is my first try in language design and I have focused on simplicity and ease of development. The design follows relatively closely the one of [Emacs Lisp](https://www.gnu.org/software/emacs/manual/html_node/eintr/). A lot of conventions, syntax and concepts are borrowed from there. ALisp does, however, has it&rsquo;s own features like runtime dynamic modules loading, proper file importing as well as general programming language features that are expected.



By now I&rsquo;ve developed the core features and even written several small library like modules that enable some &ldquo;real&rdquo; programming in the language. Alisp comes with a interpret that is just a binary executable, much like the one of [Python](https://www.python.org/). The interpreter can evaluate files and provides a [REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop) environment.


## Building

The project requires C++17 compliant compiler. It is tested with clang-7, gcc-7, gcc-8 and gcc-9. CMake is used as a build system and [conan](https://conan.io/) is required as one package is pulled form there. Conan can be isntalled through pip with:

```sh
pip install conan
```

This is more or less the only requirement for build the project. Having the [GNU Readline](https://tiswww.case.edu/php/chet/readline/rltop.html) on your system is nice but it is not necessary.



Once this is done, the building is &ldquo;standard&rdquo;:

```sh
git clone https://github.com/palikar/
mkdir build
cd build
cmake .. -DCMAKE_BUILD_TYPE="Release" -DBUILD_SHARED_LIBS=ON -DCMAKE_INSTALL_PREFIX="/usr"
make -j8 && make install
```

This should be enough to get your started with ALisp. You should care to adjust the flags as you desire.


### Building tests

The project can also be build with variety of tests and checks. This, however, requires some more tools to be available on the system. Those include [gcovr](https://gcovr.com/en/stable/) and [valgrind](https://valgrind.org/). If you have those, you should be ok while running something like:

```sh
mkdir build_test
cd build_test
cmake .. -DCMAKE_BUILD_TYPE="Debug" -DBUILD_SHARED_LIBS=ON -DCMAKE_INSTALL_PREFIX="/usr" -DBUILD_TESTING=ON -DBUILD_EXAMPLES=ON -DENABLE_COVERAGE=ON -DVALGRIND_CHECKS=ON
make -j8
```

Now you can run several make targets to perform all the tests, checks and coverage.

```sh
make test             # runs all of the tests
make valgrind_checks  # runs all of the valgrind checks and generates reports
make test_examples    # runs all of the scripts meant for testing
```



CMake can also be executed with `-DRUN_PERFORMANCE_TESTS` which will include several tests that are meant to stress the interpreter a little bit and evaluate its performance.


### Building the documentation

Currently the project does not have the most stellar documentation but the setup is there. There are two typed of documentations. The first one is the language specification, references and general description on how to work with the interpreter. This one can be build trough executing:

```sh
make dog_gen    # generates the website with the documentation
```

in the root directory.



The source code has its own documentation that is built with [Doxygen](http://www.doxygen.nl/).

```sh
mkdir build_doc
cd build_doc
cmake .. -DBUILD_DOC=ON
make doc
```


## Usage

For a very simple demo of the language, create a file named `hello.al` with the following contents:

```emacs-lisp
(defvar a 42)
(if (== a 42)(println "Hello World!")
  (println "Hello World, something is wrong with this language :/"))
```

The script can be executed like:

```
alisp hello.al
```



The interpreter has a man-page-like help that can be useful but it&rsquo;s not as polished as I wanted it to be. The useful parts of the help are:

    DESCRIPTION
    The alisp programming language.
    
    SYNOPSIS
            alisp [-v] [-h] [-i] [-d] [-l] [-Q] [-I <include>]... [-W <warnings>]...
                  [-e <expr>] [<file> [<args>]...]
    
    OPTIONS
            -v, --version
                        Show the version and build information of the current
                        executable
    
            -h, --help  Print help information
            -i, --interactive
                        Start interactive mode after file evaluation
    
            -d, --parse-debug
                        Debug output from the parser
    
            -l, --eval-debug
                        Debug output from the evaluator
    
            -Q, --quick-start
                        Do not loady any scripts on initialization
    
            -I <include>
                        Extra include directories for module imports.
    
            -W <warnings>
                        Warning types that should be enabled.
    
            <expr>      Input string to evaluate
            <file>      Input file
            <args>...   Arguments for the script being ran.

The `-l` and `-d` flags are there for debugging purposes.



See the [documentation](https://alisp.readthedocs.io/en/latest/) for more information.


## Acknowledgments

I&rsquo;ve learned a ton while developing Lisp. Here are some of the sources that I&rsquo;ve taken inspiration from:

-   [ChaiScript](https://github.com/ChaiScript/ChaiScript/) - a lot of the things I&rsquo;ve leaned about C++ comes from this project. It also taught me about practical parsing, interpreter design and a good dose of template meta programming.
-   [Catch2](https://github.com/ChaiScript/ChaiScript/) - This is the library ALisp uses for its tests
-   [Clipp](https://github.com/muellan/clipp) - This is the library ALisp uses for command line arguments handling
-   [Rang](https://www.google.com/search?q=c%2B%2B+rang&ie=utf-8&oe=utf-8&client=firefox-b-e) - A simple library for printing text with color on the console.
-   [GNU Emacs](https://www.gnu.org/software/emacs/manual/html_node/eintr/) - The whole project is written in Emacs. But, more importantly, the source code of Emacs taught me a lot of things about Lisp interpretation.

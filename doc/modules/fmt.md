### Fmt

#### Description

The `fmt` module helps you format strings.

The formating is based on the python's [Format Specification Mini-Language](https://docs.python.org/3.4/library/string.html#formatspec)
as well as the C++ library [FMT](https://fmt.dev/latest/syntax.html). For most the things, internally alisp uses the mentioned library, but
the `fmt` module implements its own formating from scratch.

The syntax for the formating strings is as close to [FMT's syntax](https://fmt.dev/latest/syntax.html) as possible.
In most cases you can simply use `{}` for a replacement field. For further detail you can check out the link with the FMT's syntax.

To note is that the `printf` and `fmt` functions in the `fmt` module use the same syntax.

#### Functions

**eprintfln** : *(eprintfln FORMAT_STRING [ARG]...)*

Print the string FORMAT_STRING formated with the given arguments on
the standard error stream followed by a new line.


**eprintf** : *(eprintf FORMAT_STRING [ARG]...)*

Print the string FORMAT_STRING formated with the given arguments on
the standard error stream.


**printfln** : *(printf FORMAT_STRING [ARG]...)*

Print the string FORMAT_STRING formated with the given arguments on
the standard output followed by a new line.


**printf** : *(printf FORMAT_STRING [ARG]...)*

Print the string FORMAT_STRING formated with the given arguments on
the standard output.



**fmt** : *(fmt FORMAT_STRING [ARG]...)*

Return a string resulting from the formating FORMAT_STRING with the
given arguments.

```elisp
(fmt "this is formated wiht {} and {}" 42 "some words")
```


#### Constants



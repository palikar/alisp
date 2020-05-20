### Func

#### Description

 The `func` modules provides support for working with higher order
functions. It aims to bring more "functional" features to alisp.

#### Functions

**identity** : *(identity ARG)*

Return ARG unchanged.


**thread-first** : *(thread-first FORMS)*

Thread FORMS elements as the first argument of their successor.

Example:
```elisp
(thread-first
      5
      (+ 20)
      (/ 25)
      (-)
      (+ 40))
```

Is equivalent to: `(+ (- (/ (+ 5 20) 25)) 40)` 

**thread-last** : *(thread-last FORMS)*

Thread FORMS elements as the last argument of their successor.

Example:
```elisp
    (thread-last
      5
      (+ 20)
      (/ 25)
      -
      (+ 40))
```

Is equivalent to: `(+ 40 (- (/ 25 (+ 20 5))))`

**compose** : *(compose [FUNCTION]...)*

Create a new function by composing several ones. The last function
will be the innter most funciton in the composition.

Example:
```elisp
((compose (lambda (x) (* 2 x)) (lambda (x) (* 3 x)) ) 10) ; -> 60
```

In the above example, the compose function will return a function that
is equivalent to `(lambda (x) ((lambda (x_2) (* 2 x_2) ) ((lambda (x_1) (* 3 x_1)) x)))`.
This measns that the last function will be evalued first and then the
result of that will be used as input for the next function.


**reduce** : *(reduce FUNCTION LIST)*

Apply function of two arguments cumulatively to the items of LIST,
from left to right, so as to reduce the iterable to a single value.The
left argument is the accumulated value and the right argument is the
update value from the list.

```elisp
(reduce (lambda (x y) (+ x y)) '(1 2 3 4 5)) ; -> 15
```
 

**ignore** : *(ignore [ANY]...)*

Return nil and ignore all of the given arguments.


**partial** : *(partial FUNCTION [ARGUMENT] ...)*

Create a new function by partially applying arguments to a
function. The return function can be called normally, either without
arguments (if every argument was partially applied) or with the
unapplied arguments.

Example:
```elisp
((partial (lambda (x y) (x + y)) 5) 2) ; -> 7
```
In the example, `(partial (lambda (x y) (x + y)) 5)` is equivalent to
`(lambda (x) ((lambda (x y) (x + y)) x 5))`. This is a function that 
takes a single argument and adds 5 to it.


 

#### Constants
**_** : Can be used as a placeholder object at certain places.




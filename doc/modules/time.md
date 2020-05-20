### Time

#### Description

The `time` module provides utility functions for working with time
and dates.

The module provides access to several internal clocks of the C++
standard library.

#### Functions

**t-s** : *(t-s TIME)*

Convert `TIME` in seconds to seconds as as real number.


**t-hr** : *(t-hr TIME)*

Convert `TIME` in hours to seconds as as real number.


**t-ms** : *(t-ms TIME)*

Convert `TIME` in miliseconds to seconds as as real number.


**t-ns** : *(t-ns TIME)*

Convert `TIME` in nanoseconds to seconds as as real number.


**t-clock-time-ns** : *Return the current time in nanoseconds as a real number according to*
the given clock. `CLOCK` can be:
* system-clock
* steady-clock
* high-res-clock

**t-clock-time** : *(t-clock-time CLOCK)*

Return the current time in seconds as a real number according to the
given clock. `CLOCK` can be:
* system-clock
* steady-clock
* high-res-clock
 

**t-localtime** : *(t-localtime TIME)*

Return a list of the form (seconds, minutes, hours month day, month,
year, week day, year day, leap year) representing the time `TIME` as a local time.


**t-sleep** : *(t-sleep TIME)*

Block the current thread for `TIME` miliseconds.


**t-gmtime** : *(t-gmtime TIME)*

Return a list of the form (seconds, minutes, hours month day, month,
year, week day, year day, leap year) representing the time `TIME` as a GM time.


**t-mktime** : *(t-mktime TIME-LIST)*

Convert a time list of the form (seconds, minutes, hours month day,
month, year, week day, year day, leap year) to time (seconds) since
the beginning of the epoch. The values in the time list are permitted
to be outside their normal ranges.  

**t-ctime** : *(t-ctime [TIME])*

Return a textural representation of the current time. If `TIME` is
given, use this time to construct the string.


**t-time** : *(t-time)*

Return the current calendar time in seconds.


**t-process-time** : *(t-process-time)*

Returns the approximate processor time used by the process since the
beginning of an implementation-defined era related to the program's
execution. To convert result value to seconds divide it by
`clocks-pre-second`.
 

**t-strftime** : *(t-strftime FORMAT-STRING TIME-LIST)*

Return the restulting string by formating `FROMAT-STRING` with the
time list `TIME-LIST`. The ruls for formating are the same as in the
[C++ page for the strftime function ](https://en.cppreference.com/w/cpp/chrono/c/strftime).
The time list is of the form as by the `t-mktime` and `t-gmtime` functions.


#### Constants


**system-clock** : Integer representing the system-wide real time wall clock.


**high-res-clock** : Integer representing a clock with the smallest tick period provided
by the implementation.


**steady-clock** : Integer representing a monotonic clock. The time points of this clock
cannot decrease as physical time moves forward and the time between
ticks of this clock is constant.


**clocks-pre-second** : Number of clock ticks per second. Clock ticks are units of time of a
constant but system-specific length.


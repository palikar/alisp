### System

#### Description

The `os` modules allows you to access common OS functions through Alisp.
#### Functions

**chwd** : *(chwd PATH)*

Change the currnt working directory to `PATH`.


**list-env** : * Return the value of `env-vars`.*

**check-env** : *(check-env VAR)*

Return the `t` if the environment variable `VAR` is defined. Return `nil` otherwise.


**sys** : *(sys COMMAND)*

Execute the command `COMMAND` in a shell of the host system.


**set-env** : *(set-env VAR VALUE)*

Set the value of the environment variable `VAR` to `VALUE`


**get-env** : *(get-env VAR)*

Return the value of the environment variable `VAR` if avaialble. Return `nil` otherwise.


#### Constants



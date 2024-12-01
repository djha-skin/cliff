# Command Line Interface Framework for Functions (CLIFF)

Do you want to have your app look at config files, the environment, and command
line to get your app's options, but don't want to have to actually _code_ any of
that stuff? Feel bogged down by write the same old I/O code for every app?

Then CLIFF is for you!

## Install

It's on OCICL and Quicklisp.

# # CLIFF Tutorial

The code discussed in this tutorial can be found
[here](https://github.com/djha-skin/calc).

### Up and Running

This is a CLI framework/library for the _impatient_, so
let's make a CLI tool in Common Lisp _really fast_.

We'll make a CLI math calculator.

```lisp
;;; calculator.lisp -- A CLI calculator.
;;;;
;;;; SPDX-FileCopyrightText: 2024 Daniel Jay Haskin
;;;; SPDX-License-Identifier: MIT
;;;;

(in-package #:cl-user)

(defpackage
  #:com.djhaskin.calc (:use #:cl)
  (:documentation
    "
    A CLI calculator.
    ")
  (:import-from #:com.djhaskin.cliff)
  (:local-nicknames
    (#:cliff #:com.djhaskin.cliff))
  (:export #:main))

(in-package #:com.djhaskin.calc)


(defun main ()
  (cliff:execute-program
    "calc"))
```

We just made a CLI tool.

This is what happens when we run it:

```
* (main)
Welcome to `calc`!

This is a CLIFF-powered program.

Configuration files and, optionally, environment variables can be set
using NRDL, a JSON superset language. Output will be a NRDL document.
More information about NRDL can be found here:

https://github.com/djha-skin/nrdl

Options can be given via:
  - Configuration file, as in `{ <option> <value> }`
  - Environment variable, as in `CALC_<KIND>_<OPTION>`
  - Command line, as in `--<action>-<option>`

Configuration files are consulted first, then environment variables, then
the command line, with later values overriding earlier ones.

Configuration files can be in the following locations:
  - A file named `.calc.nrdl` in the directory `<YOUR-CURRENT-PWD>`
    (or any of its parents)
  - A home-directory config file in an OS-specific location:
      - On Linux: `${XDG_CONFIG_HOME}/calc/config.nrdl`
        (by default ~/.config/calc/config.nrdl)`
      - On Mac:   `~/Library/Preferences/calc/config.nrdl`
      - On Windows: `%LOCALAPPDATA%\calc\config.nrdl`
        (by default `%USERPROFILE%\AppData\Local\calc\config.nrdl`)

Options can be set via environment variable as follows:
  - `CALC_FLAG_<OPTION>=1` to enable a flag
  - `CALC_ITEM_<OPTION>=<VALUE>` to set a string value
  - `CALC_LIST_<OPTION>=<VAL1>,<VAL2>,...` to set a list option
  - `CALC_TABLE_<OPTION>=<KEY1>,<VAL1>=<KEY2>,<VAL2>=...` to set
     a key/value table option
  - `CALC_NRDL_<OPTION>=<NRDL_STRING>` to set a value using
     a NRDL string
  - `CALC_FILE_<OPTION>=<FILE_PATH>` to set a value using the contents
     of a NRDL document from a file as if by the `--file-*` flag
  - `CALC_RAW_<OPTION>=<FILE_PATH>` to set a value using the raw bytes
    of a file as if by the `--raw-*` flag

Options can be changed or set on the command line in the following ways:
  - To enable a flag, use `--enable-<option>`.
  - To disable a flag, use `--disable-<option>`.
  - To reset any value, use `--reset-<option>`.
  - To add to a list, use `--add-<option> <value>`.
  - To set a string value, use `--set-<option> <value>`.
  - To set using a NRDL string, use `--nrdl-<option> <value>`.
  - To set using NRDL contents from a nrdl document from file,
    use `--file-<option> <url>`.
  - To set using the raw bytes of a file, use `--raw-<option> <url>`.

  - For `--file-<option>` and `--raw-<option>`, URLs are also supported:
      - `http(s)://user:password@url` for basic auth
      - `http(s)://header=val@url` for a header
      - `http(s)://token@url` for a bearer token
      - `file://location` for a local file
      - `-` for standard input
    Anything else is treated as a file name.

The following options have been detected:
{
}
Documentation not found.

No action exists for the command.

{
    status successful
}
0
```

CLIFF generated a generous help page for us. Since we didn't tell it to _do_
anything, it figured it should print the help page.

First, let's compile our program into an executable image and run `main` as the
entry point. There are lots of ways to do this. On SBCL, the typical tool of
choice is [`save-lisp-and-die`](https://www.sbcl.org/manual/#Function-sb_002dext-save_002dlisp_002dand_002ddie), like this:


```lisp
(sb-ext:save-lisp-and-die
  "calc"
  :toplevel #'main
  :executable t
  :compression t
  :save-runtime-options t)
```

Then from the command line:

```
$ calc
```

We get the same output as before.

### CLIFF: An Open World Configuration CLI Framework

CLIFF is a different kind of CLI framework. It assumes what we'll call an "open
world" configuration. It defines a correct format for options to appear
in configuration files, the environment, and the command line, and gathers them
as it sees them. This frees the developer from having to over-specify their
command line options when just starting out. It also enables some interesting
workflows where the developer can pass the options hash table down to lower
libraries, which may or may not expect certain options set in the table.

Since we didn't tell it to do anything yet, it just prints the help page in
addition to what it finds. This part of the output shows what options have been
found:

```
The following options have been detected:
{
}
```

Looks like it didn't find anything. Let's help it find some options.

Now let's add some options:

```
$ calc --enable-floating-point
```

When we run this, CLIFF's help page prints this at the bottom:

```
{
    floating-point true
}
```

It's printing out that big nested hash table of options it finds, but in
[NRDL](https://github.com/djha-skin/nrdl), a JSON superset data language
designed to work well for configuration files and as well as command line output for
programs written in lisp. It was specially built to power CLIFF.

The equivalent alist might be

```lisp
'((:FLOATING-POINT . t))
```

Cool. Let's try to add more options.

```
export CALC_LIST_OPERANDS=1,2,3,4,5
export CALC_TABLE_NAMES=plus=+,minus=-,times=*
./calc
```

The help page now shows this:

```
{
    floating-point true
    names {
        minus "-"
        plus "+"
        times "*"
    }
    operands [
        "1"
        "2"
        "3"
        "4"
        "5"
    ]
}
```

Notice that the `operands` are added as strings. This is true of most options
added to the options hash table via command line arguments or environment
variables. However, there is a workaround: using `--nrdl-*` on the CLI or the
`CALC_NRDL_<thing>` environment variable.

To demonstrate this, we run our command with a `--nrdl-*` option as follows:

```
export CALC_NRDL_OPERANDS=[1,2,3,4,5]
./calc --nrdl-divisors '{"one": 1, "two": 2, "three": 3}'
```

Doing so yields this output at the end of the help page:

```
{
    divisors {
        "one" 1
        "three" 3
        "two" 2
    }
}
```

The user can specify _any_ option in this way, _even if the program doesn't use
it_. This way, the command can take some options verbatim and pass it on, print
it out, return it, or change it in arbitrary ways. This open-world assumption of
opptions allows the program to compose better with libraries and other programs.

### The Option Tower: Config Files, Environment Variables, and CLI Flags

CLIFF gathers options for the calling code tool (in this case `calc`) from
configuration files, environment variables, and command-line flags. It merges
what it finds from various sources into one single, nested hash table.

Let's add something from a configuration file now. CLIFF looks in the present
working directory, as well as an OS-dependent, home-based location, which is
printed out in the help page.

Let's make a file corresponding to the "home directory" option. Make a file
called `~/.config/calc/config.nrdl` (on Linux) and put the following content in
it (if you are on a different OS, consult the help page above):

```nrdl
{
    floating-point-size double
    scale 11
    name-of-calculation "fizzle"
    calculations [
       {
          operands [
            1
            2
            3
            4
            5
          ]
          operator +
        }
        {
          operands [
            10
            20
            30
            40
            50
          ]
          operator *
        }
      ]
}
```

Now we run `./calc` and it yields this output:

```
{
    calculations [
        {
            operands [
                1
                2
                3
                4
                5
            ]
            operator +
        }
        {
            operands [
                10
                20
                30
                40
                50
            ]
            operator *
        }
    ]
    floating-point-size double
    name-of-calculation "fizzle"
    scale 11
}
```

Now we add a file called `.calc.nrdl` in the current directory with this
content:

```
{
  flaoting-point false
  sumall true
}
```

Now when we run `./calc`, we see that it sees these options:

```nrdl
{
    calculations [
        {
            operands [
                1
                2
                3
                4
                5
            ]
            operator +
        }
        {
            operands [
                10
                20
                30
                40
                50
            ]
            operator *
        }
    ]
    floating-point false
    floating-point-size double
    name-of-calculation "fizzle"
    names {
        minus "-"
        plus "+"
        times "*"
    }
    operands [
        "1"
        "2"
        "3"
        "4"
        "5"
    ]
    scale 11
    sumall true
}
```

Note the `sumall` key we added in `.calc.nrdl` and the `floating-point` key. The
`floating-point` key was true in the home config file, but was overridden in the
current directory config file. Now, CLIFF looks both in the present working
directory _and all its parents_, and uses the file it may or may not find as an
override to the home directory config file, which serves as a base.

Let's say we want to override the `scale` key with environment variables. We set
`CALC_NRDL_SCALE=10` (we use NRDL to ensure that, as a number, it is parsed).

Now we see that `scale` is 10 in the output.

To override environment variables, set command line flags.


To see this, we will call `calc` with an option override for
`set-name-of-calculation`:

```
./calc --set-name-of-calculation dizzle
```

We see that `name-of-calculation` is set to `"dizzle"` in the printed out help
page.


Sweet. We have a bunch of options. Our app observes 12-factor goodness by
default, checking for config files, environment variables, and even harvesting
options for us from the command line, all without us having to write much.

### Add a Default Function

Now let's do something with this information. Right now the command just prints
out a help page when the command line tool is called. Change the lisp file which calls
`execute-program` and add a default function, like this:


```lisp
;;;; calculator.lisp -- A CLI calculator.
;;;;
;;;; SPDX-FileCopyrightText: 2024 Daniel Jay Haskin
;;;; SPDX-License-Identifier: MIT
;;;;
;;;;

(in-package #:cl-user)

(defpackage
  #:com.djhaskin.calc (:use #:cl)
  (:documentation
    "
    A CLI calculator.
    ")
  (:import-from #:com.djhaskin.cliff)
  (:local-nicknames
    (#:nrdl #:com.djhaskin.nrdl)
    (#:cliff #:com.djhaskin.cliff))
  (:export #:main))

(in-package #:com.djhaskin.calc)


(defparameter operators
    `(("+" . ,#'+)
      ("-" . ,#'-)
      ("*" . ,#'*)
      ("/" . ,#'/)))

(defun calc (options)
  (let* ((result (make-hash-table :test #'equal))

         (operands (cliff:ensure-option-exists :operands options))
         (operator (cliff:ensure-option-exists :operator options))
         (func (cdr (assoc operator operators :test #'equal))))
    (setf (gethash :result result) (apply func operands))
    (setf (gethash :status result) :successful)
    result))


(defun main ()
  (sb-ext:exit
    :code
    (cliff:execute-program
      "calc"
      :default-function #'calc)))
```

We have some changes here in `calculator.lisp` from our original listing.

First, we create a new function called `calc`. It takes one argument, `options`,
which will be that hash table of options we have been discussing building.

It calls `cliff:ensure-option-exists` on specific options that it expects to be
present in the hash table. This function ensures a particular key exists in the
hash table, and if it doesn't, it signals an error.

We then set it as the `:default-function` when we call `cliff:execute-program`.

We also add `sb-ext:exit` and set `:code` as the return value of
`cliff:execute-program` to ensure the exit code is propogated to the OS.

Now we compile and run our program:

```
$ ./calc
{
    error-message
        |Abnormal exit error
        |
        ^
    missing-option operands
    status cl-usage-error
}
$ echo $?
64
```

We see that a `cl-usage-error` error has been signaled and that the command
returned a non-zero status code corresponding to the type of error signaled. The
error is printed to standard output in the form of a NRDL document.

In fact, by default, everything CLIFF prints out will be in the form of a NRDL
document, though as we'll see, this can be turned off. This default is to enable
CLIFF to fulfill its mission: just plug a few functions into CLIFF, and it'll
handle the rest. I/O is all taken care of by default, in a human-friendly
machine readable format.

Let's call `./calc` again, with operands and an operator.

Because CLIFF doesn't care where it gets its options, we can mix and match where
they come from. In our example, we'll say that we pretty much always want `calc`
to use `+` as an operator, unless we want to override it. We'll put that in our
configuration file, and specify the operands on the CLI.

We put this in our `.calc.nrdl` in the current directory:

```
{
    operator "+"
}
```

And then we call `./calc`:

```
$ ./calc --nrdl-operands '[1,2,3,4]'
{
   result 10
   status successful
}
```

### Use Roswell with CLIFF

Now that we have a semi-working example, it's time to get it cleaned up and put
into a proper ASDF package. We'll also add a roswell endpoint.

We make a new directory called `calc`, change directory into it with a terminal,
and run `ros init calc`.

We then put this in the file `com.djhaskin.calc.asd`:

```lisp
(defsystem "com.djhaskin.calc"
  :version "0.1.0"
  :author "Daniel Jay Haskin"
  :license "MIT"
  :depends-on ("com.djhaskin.cliff")
  :components ((:module "src"
                :components
                ((:file "calculator"))))
  :description "CLI calculator, a demonstration project for Common Lisp CLIFF")
```

We then create a `src` directory inside of the `calc` directory and move
`calculator.lisp` into it.

Editing our `calc.ros` file, we make sure it calls our `main` function and that
it loads our system in the init forms:

```lisp
#!/bin/sh
#|-*- mode:lisp -*-|#
#|
exec ros -Q -- $0 "$@"
|#
(progn ;;init forms
  (ros:ensure-asdf)
  (asdf:load-system "com.djhaskin.calc"))

(defpackage :ros.script.calc.3941919262
  (:use :cl)
  (:import-from #:com.djhaskin.calc)
  (:local-nicknames
    (#:calc #:com.djhaskin.calc)))

(in-package :ros.script.calc.3941919262)

(defun main (&rest argv)
  (declare (ignorable argv))
  (calc:main argv))

;;; vim: set ft=lisp lisp:
```

We change our `main` function in `calculator.lisp` to accept an `argv` argument,
passing it into CLIFF with the optional `:cli-arguments` option. We also get rid
of `sb-ext:exit`, since ros will take care of the exit code for us:

```lisp
(defun main (argv)
    (cliff:execute-program
      "calc"
      :default-function #'calc
      :cli-arguments argv))
```

Now we can run `ros build calc.ros` from the `calc` directory.

Our `calc` program is shaping up, but there's some cosmetic/ergonomic changes we
can make to make the user's experience a bit better.

### Defaults

First, we can decide that, as a sensible default, `calc` should just assume that
the operator will be `+`.

Let's set that as a default by adding it to the `:defaults` option to
`cliff:execute-program`:

```lisp
(defun main (argv)
    (cliff:execute-program
      "calc"
      :default-function #'calc
      :cli-arguments argv
      :defaults '((:operator "+"))))
```

The option `:defaults` takes an alist mapping options to values because it is
easier to type in an alist than a hash table.

Now we run again, this time we remove the `.calc.nrdl` that has that default
operator in it.

```
$ ./calc --nrdl-operands '[1,2,3,4]'
{
    result 10
    status successful
}
```

It works as before since the option now has a default.

### String Conversion

Next, we observe that specifying operands might be more convenient if they were
specified one at a time, like this:


```
./calc --add-operands 1 --add-operands 2 --add-operands 3 --add-operands 4
```

If we run that though, we get an error:

```
$ ./calc --add-operands 1 --add-operands 2 --add-operands 3 --add-operands 4
{
    error-datum "\"4\""
    error-expected-type "NUMBER"
    error-message
        |The value
        |  "4"
        |is not of type
        |  NUMBER
        ^
    status data-format-error
}
```

Calc doesn't know what "type" arguments are when they are specified on the
command line, so it assumes they are a string. We can see this when we run

```
./calc help --add-operands 1 --add-operands 2
```

The help page shows what `calc` actually sees:

```
The following options have been detected:
{
    operands [
        "2"
        "1"
    ]
    operator [
        "+"
    ]
}
```

It shows our default operator, but it also shows our operands as a list of
strings.

If we expect that our operands will always be specified on the command line
rather than the environment or via config file, we may wish to check for strings
and convert them to non-strings if possible.

To allow for this, CLIFF provides `:setup` and `:teardown` optional arguments to
`execute-program`. The `:setup` function takes an options map and return a modified
version. This is the version which the main logic functions will see. The `:teardown`
function takes the map that the main logic functions create, changes or creates
a new version based on it, and returns that. This modified map will be what
CLIFF sees when it starts to try to wrap up the program.

These functions provide a lot of power in terms of what we can do or how we can
interact with CLIFF.

To accomplish the string to number transformation, we add a `:setup` lambda:


```lisp
(defun main (argv)
  (cliff:execute-program
    "calc"
    :default-function #'calc
    :cli-arguments argv
    :defaults '((:operator "+"))
    :setup (lambda (options)
             (let ((operands (gethash :operands options)))
               (setf (gethash :operands options)
                     (map 'list #'parse-integer operands))
               options))))
```

Then we recompile using `ros` and run again:

```
$ ./calc --add-operands 1 --add-operands 2
{
    result 3
    status successful
}
```

The downside is that operands would need to be specified as strings if there
ever were a need to put them in a configuration file, but if the target audience
typically uses the command line to specify the arguments, then maybe this is a
good trade-off.

### Provide Command-Line Aliases

It feels bad to make the user punch in `--add-operands` for every operands. We
would like to enable a single letter for that option, so we will add a CLI alias
for it using `execute-program`'s optional `:cli-aliases` option:

```
(defun main (argv)
  (cliff:execute-program
    "calc"
    :default-function #'calc
    :cli-arguments argv
    :defaults '((:operator "+"))
    :cli-aliases
    '(("-h" . "help")
      ("--help" . "help")
      ("-o" . "--add-operands"))
    :setup (lambda (options)
             (let ((operands (gethash :operands options)))
               (setf (gethash :operands options)
                     (map 'list #'parse-integer operands))
               options))))
```

Note, we also added `--help` and `-h` aliases.

CLI Aliases are simple substitutions. If CLIFF sees what is specified as a key
in the alist on the command line, it will replace it with the value.

CLIFF provides a `help` subcommand, but not a `--help` or `-h` option. Providing
these aliases will help the user if they don't know what to do.

We also added the `-o` alias to mean `--add-operands`.

Now we can recompile and run with the new, nice shorter arguments:

```
$ ./calc -o 1 -o 2 -o 3
{
    result 6
    status successful
}
```

### Override Default Output

The main mission of CLIFF is to enable users to write potentially pure
functions, and hook them up to the command-line, configuration files, and the
environment using `execute-program` so that the function can just do what
functions do betst: compute. However, if more control over output is desired, it
is easy to take back control.

We first add `:suppress-final-output t` to the call to `execute-program`:

```lisp
(defun main (argv)
  (cliff:execute-program
    "calc"
    :default-function #'calc
    :cli-arguments argv
    :defaults '((:operator "+"))
    :cli-aliases
    '(("-h" . "help")
      ("--help" . "help")
      ("-o" . "--add-operands"))
    :setup (lambda (options)
             (let ((operands (gethash :operands options)))
               (setf (gethash :operands options)
                     (map 'list #'parse-integer operands))
               options))
    :suppress-final-output t))
```

We also add a `format` call to our `calc` function:

```lisp
(defun calc (options)
  (let* ((result (make-hash-table :test #'equal))
         (operands (cliff:ensure-option-exists :operands options))
         (operator (cliff:ensure-option-exists :operator options))
         (func (cdr (assoc "+" operators :test #'equal)))
         (out (apply func operands)))
    (format t "~A~%" out)
    (setf (gethash :result result) out)
    (setf (gethash :status result) :successful)
    result))
```

Now our output is much simpler:

```
$ ./calc -o 1 -o 2 -o 3
6
```

Also, the CLI aliases we defined were added automatically to the help page:

```
$ ./calc help
...
The following command line aliases have been defined:

      This                                     Translates To
        -h                                              help
    --help                                              help
        -o                                    --add-operands
...
```

### Add Subcommands

As a means of demonstration, we will add different calculation operators
(multiply, divide, add, subtract) as subcommands and get rid of the default
action, ensuring the command run with no subcommands will print the help page.

All we need to do is provide an alist mapping different collections of
subcommands with different functions, where the functions expect an option table
and return a result table.


First, we'll add some additional operators:


```lisp
(defparameter operators
    `(("+" . ,#'+)
      ("-" . ,#'-)
      ("*" . ,#'*)
      ("/" . ,#'/)
      ("&" . ,#'logand)
      ("%" . ,(lambda (&rest args)
                (multiple-value-bind
                    (quotient remainder)
                    (apply #'truncate args)
                  remainder)))))
```

For demonstration purposes, we'll create some functions that just set the
operator and then pass execution into our previously created `calc` function:

```
(defun programmer-and (options)
  (setf (gethash :operator options) "&")
  (calc options))

(defun modulus (options)
  (setf (gethash :operator options) "%")
  (calc options))
```


Then we just add these new functions as subcommands:

```lisp
(defun main (argv)
  (cliff:execute-program
    "calc"
    :subcommand-functions
    `((("programmer" "and") . ,#'programmer-and)
      (("modulus") . ,#'modulus))
    :default-function #'calc
    :cli-arguments argv
    :defaults '((:operator "+"))
    :cli-aliases
    '(("-h" . "help")
      ("--help" . "help")
      ("-o" . "--add-operands"))
    :setup (lambda (options)
             (let ((operands (gethash :operands options)))
               (setf (gethash :operands options)
                     (map 'list #'parse-integer operands))
               options))
    :suppress-final-output t))
```

Not the `:subcommand-functions` argument above. It maps collections of
subcommands to the function that should be called when that subcommand is
specified.

After compiling again, we can now do this:

```lisp
$ ./calc programmer and -o 1 -o 3
1
$ ./calc modulus -o 3 -o 5
2
```

### Add More Help Documentation

CLIFF is pretty good at adding general documentation around the option tower,
but not really around each individual function.

Specifying the default function's help is pretty easy, just give a string
argument to the `:default-func-help` option. Specifying help strings for the
different subcommands are likewise easy; just give an alist that map subcommand
strings to help strings in the `:subcommand-helps` option:

```
(defun main (argv)
  (cliff:execute-program
    "calc"
    :subcommand-functions
    `((("programmer" "and") . ,#'programmer-and)
      (("modulus") . ,#'modulus))
    :subcommand-helps
    `((("programmer" "and") . "Sets the operator to `&`")
      (("modulus") . "Sets the operator to `%`"))
    :default-function #'calc
    :default-func-help
    (format
      nil
      "~@{~@?~}"
      "Welcome to calc.~%"
      "~%"
      "This is a calculator CLI.~%"
      "~%"
      "The default action expects these options:~%"
      "  operand           Specify operand~%"
      "                    (may be specified multiple times)~%"
      "  operator (string) Specify operator~%")
    :cli-arguments argv
    :defaults '((:operator "+"))
    :cli-aliases
    '(("-h" . "help")
      ("--help" . "help")
      ("-o" . "--add-operands"))
    :setup (lambda (options)
             (let ((operands (gethash :operands options)))
               (setf (gethash :operands options)
                     (map 'list #'parse-integer operands))
               options))
    :suppress-final-output t))
```

Now running `./calc help` shows the default function help:

```
$ ./calc help
...
Documentation:

Welcome to calc.

This is a calculator CLI.

The default action expects these options:
  operand           Specify operand
                    (may be specified multiple times)
  operator (string) Specify operator
```

Likewise, running `./calc help modulus` and `./calc help programmer and` return
their respective helps at the bottom of the help page:

```
Documentation for subcommand `modulus`: 

Sets the operator to `%`
```

```
Documentation for subcommand `programmer and`: 

Sets the operator to `&`
```

## Wrap-up

We have created a fully-functional CLI tool. It is a 12 factor app that looks in
config files, the environment, and the command line for its options and merges
them together. We have easily been able to add commands, subcommands, and
documentation for them. We have also shown how we can have simpler arguments on
the command line.

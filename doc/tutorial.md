# CLIFF Tutorial

## Quick Start

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
    (#:nrdl #:com.djhaskin.nrdl)
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

## CLIFF: An Open World Configuration Framework

CLIFF is a different kind of CLI framework. It assumes what we'll call an "open
world" configuration. Instead, it defines a correct format for options to appear
on the command line and gathers them as it sees them. This frees the developer
from having to over-specify their command line options when just starting out.
It also enables some interesting workflows where the developer can pass the
options hash table down to lower libraries, which may or may not expect certain
options set in the table.

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

## The Option Tower: Config Files, Environment Variables, and CLI Flags

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

Now when we run calc, we see that it sees these options:
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

## Add a Default Function

Now let's do something with this information. Right now the command just prints
out a help page when the command line tool is called. Change the lisp file which calls
`execute-program` and add a default function, like this:


```lisp
;;;; calculator.lisp -- A CLI calculator.
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
    (#:nrdl #:com.djhaskin.nrdl)
    (#:cliff #:com.djhaskin.cliff))
  (:export #:main))

(in-package #:com.djhaskin.calc)


(defparameter operators
    '(("+" . #'+)
      ("-" . #'-)
      ("*" . #'*)
      ("/" . #'/)))

(defun calc (options)
  (let* ((result (make-hash-table :test #'equal))

         (operands (cliff:ensure-option-exists :operands options))
         (operator (cliff:ensure-option-exists :operator options))
         (func (cdr (assoc "+" operators :test #'equal))))
    (setf (gethash :result result) (apply func operands))
    (setf (gethash :status result) :successful)))


(defun main ()
  (cliff:execute-program
    "calc"
    :default-function #'calc))
```

We have some changes here in `calculator.lisp` from our original listing.

First, we create a new function called `calc`. It takes one argument, `options`,
which will be that hash table of options we have been discussing building.

It calls `cliff:ensure-option-exists` on specific options that it expects to be
present in the hash table. This function ensures a particular key exists in the
hash table, and if it doesn't, it signals an error.

We then set it as the `:default-function` when we call `cliff:execute-program`.

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
```

We see that a `cl-usage-error` has been 



```
kill: SIGUSR1: invalid signal specification
kill: SIGUSR1: invalid signal specification






```


<do something>

Ope, an error happened. It got printed out for us.

<do something>

We did something but it didn't get printed.

<return something>

K, but we are going to be piping this to a different program.

<change output to JSON>

But also

<hide normal output and just print what I want>

Cool. Now let's suppose that we don't like hash tables much. We'll return one,
but we want to _work_ with alists.

<setup/teardown>

Let's make things easier on our user.

<CLI-aliases, ENV-var aliases>

Let's change our program to have subcommands.

<subcommands>

Here's our final calling function:

<polished function>

Let's write some unit tests for our program.

<show dep injection features>


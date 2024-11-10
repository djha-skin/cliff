# CLIFF Tutorial


This is a CLI framework/library for the _impatient_, so
let's make a CLI tool in Common Lisp _really fast_.

We'll make a CLI math calculator

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

CLIFF is a different kind of CLI framework. It assumes what we'll call an "open
world" configuration. It gathers options for the tool (in this case `calc`) from
configuration files, environment variables, and command-line flags. It merges
what it finds from various sources into one single, nested hash table.

Since we didn't tell it to do anything yet, it just prints the help page in
addition to what it finds. This part of the output shows what options have been
found:

```
The following options have been detected:
{
}
```

Looks like it didn't find anything. Let's help it find some options.

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
`CALC_NRDL_<thing>` environment variable. Thus, this:

```
TODO
export CALC_NRDL_OPERANDS=[1,2,3,4,5]
./calc --nrdl-divisors '{"one": 1,
```

Yields these options printed to the help page:




The user can specify any option in this way, even if the program doesn't use it.
This way, the command can take some options verbatim and pass it on, print it
out, return it, or change it in arbitrary ways. It also makes it easier on the
programmer.

Let's add something from a configuration file now. CLIFF looks in the present
working directory, as well as an OS-dependent, home-based location.

Let's make a file corresponding to the PWD option. Make this file in the PWD:

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

Now the help page shows these options:


```
The following options have been detected:
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
    floating-point true
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
}
```

Sweet. We have a bunch of options. Our app observes 12-factor goodness by
default, checking for config files, environment variables, and even harvesting
options for us from the command line, all without us having to write much.

Now let's do something with this information.

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


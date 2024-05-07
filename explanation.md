# CLIFF is a Command LIne Framework for Functions

What is CLIFF? I have a hard time explaining what it does because I've never
used anything like until I actually built it.

It's everything I want a command line tool to do but what I don't want to have
to write every time.

## The Options Pattern

Something I picked up from my Clojure days is something I call the "options
pattern". The idea is that instead of having keyword arguments to a function,
you pass in a hash table of options. This is nice because you can add or remove
items and then pass the same hash table down and down and down into different
functions, with functions only using the options they need. Many of the members
of the hash table don't apply to you, but you don't have to worry about it.

Here:

```common-lisp
(defun this (options)
  (let ((a (gethash :a options))
        (b (gethash :b options)))
        (setf (gethash :d options) (+ a b))
        (that options)))

(defun that (options)
  (let ((c (gethash :c options)))
    (format t "c is ~a" c))
    options)
```

Pretty cool. In our example, `this` doesn't care about the `:c` option, but does
about `:a` and `:b`, and adds them together to create option `:d`. It then passes
the options down to `that`, which deals with the `:c` option, and returns the
options as a result.

This pattern is very useful when making command line tools in lisp. The options
hash table is a natural place to store options given by command line arguments,
environment variables, and configuration files. CLIFF is a tool that makes using
this pattern easy.

CLIFF gathers options from the command line, environment variables, and
configuration files. Then, it runs a function that you give it, passing the
options in as a hash table. The function can then use the options as it sees fit.

CLIFF then expects that function to itself return a hash table. CLIFF then
optionally prints out the hash table in a hybrid human readable/machine readable
format. Finally, CLIFF exits with a status code based on the contents of the
returned hash table.

Let's see how it works.

## Hello World

I'mma make a script that we can play around with:

```
$ ros init fuzzy.ros
```


Here's a simple example of a CLIFF program:

```common-lisp
;;;; fuzz.lisp -- It's fuzzy!
;;;;
;;;; SPDX-FileCopyrightText: 2024 Daniel Jay Haskin
;;;; SPDX-License-Identifier: MIT
;;;;
(in-package #:cl-user)

(defpackage
  #:com.djhaskin.fuzz (:use #:cl)
  (:documentation
    "
    A fuzzy CLI!
    ")
  (:import-from #:com.djhaskin.cliff)
  (:local-nicknames
    (#:cl-i #:com.djhaskin.cl-i)
    )
  (:export #:main))

(in-package #:com.djhaskin.qp)

(defun fuzz (options)
  (let ((fuzziness (gethash :fuzziness options)))
    (format t "Fuzziness is ~a" fuzziness)
    (setf (gethash :fuzziness options) (1+ fuzziness))
    (setf (gethash :status options) :successful)
    options))

(defun main (argv)
  (cliff:execute-program
    "qp"
    (cliff:system-environment-variables)
    `((() . ,#'fuzz))
    :cli-arguments argv
    :helps
    '((() . "Converts between quoted printable and binary."))))
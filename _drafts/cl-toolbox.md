---
layout: post
title: The Common Lisper's Toolbox
permalink: /cl-toolbox
category: lisp
---

Talk to any Common Lisp programmer and there's a good chance they will tell talk
about the "toolbox" the language provides that lets you write programs any way
you want. What exactly is in this toolbox?

In this post, I will talk about some of the features in Common Lisp that are not
available in most non-Lisp languages that I am familiar with. I will restrict
myself to features that are provided out of the box by the language and not
cover libraries, etc.

* TOC
{:toc}

## Metaprogramming

### Macros

This is the most well-known of features unique to Lisp. It is in fact what makes
Common Lisp a Lisp. A lot has been written about macros, I am not going to cover
them in more detail here. Except share a few links:

* If you don't know anything about Lisp macros, read [The Nature of
Lisp][the-nature-of-lisp].
* If you are somewhat familiar with macros, but want to know more, go through
  the [relevant][pcl-chapter-7] [chapters][pcl-chapter-8] from *Practical Common
  Lisp*.
* For a more advanced treatment of macros, read Paul Graham's [On
  Lisp][on-lisp].

I've also written a bit about macros here -- using them to create non-blocking
constructs that look exactly like their blocking counterparts in [Promises and
Lisp][promises].

[the-nature-of-lisp]: https://www.defmacro.org/ramblings/lisp.html
[pcl-chapter-7]: http://www.gigamonkeys.com/book/macros-standard-control-constructs.html
[pcl-chapter-8]: http://www.gigamonkeys.com/book/macros-defining-your-own.html
[on-lisp]: http://www.paulgraham.com/onlisp.html
[promises]: /promises

### Reader Macros

Less well known than macros, but in some ways even more powerful, are reader
macros. Unlike macros, which work at compilation time, reader macros work
earlier in the cycle when raw characters are being tokenized (better known as
read time).

A well known reader macro is the quote character, which simply translates
`'form` to `(quote form)`.

```cl
'(1 2 3) => (quote (1 2 3))
```

Another well known reader macro is the backquote, which allows easy substitution
of values in a list structure. The backquote is in fact extensively used to
create macros.

```cl
`(,x) => (list x)
`(,x ,@y) => (list* x y)
```

The possibilities of what you can do with reader macros is
endless. [cl-interpol][] is a well-known CL library that brings string
interpolation to Common Lisp (something that's not available out of the
box). And [my post on reader macros][reader-macros] shows how you can read JSON
directly into Lisp arrays and dictionaries.

[cl-interpol]: https://edicl.github.io/cl-interpol/
[reader-macros]: /reader-macros

## Evaluation

### Dynamic variables

### Multiple return values

### Places (generalized assignments)

## Iteration

### LOOP

## Arithmetic

### Rationals

### Complex numbers

### Bignums

## Objects

### Generic functions and methods

### Multiple inheritance

### AROUND, AFTER, BEFORE methods

## Structures

## Symbols

## Condition System (i.e. error handling)

## Pathnames

## Printer

## Optimizations

### Speed, safety and ease of debugging

### Inline hints

### Allocation on the stack

### Compiler macros

Compiler macros allow you to take advantage of static information available at
compile time and tell the compiler about a more efficient way to perform the
given computation. Most of the time, you use compiler macros to transform a
function call into something more efficient.

To give a contrived example, let's use Pythagoras' themorem to compute the
hypotenuse of a right triangle:

```cl
(defun hypotenuse (a b)
  (sqrt (+ (expt a 2) (expt b 2))))
```

If, at any call site of `hypotenuse`, either of the arguments is a literal
integer, we can shift some computation to compile time via compiler macros.

```cl
(define-compiler-macro hypotenuse (&whole form a b)
  (cond ((and (integerp a) (integerp b))
         (sqrt (+ (expt a 2) (expt b 2))))
        ((integerp a)
         `(sqrt (+ ,(expt a 2) (expt ,b 2))))
        ((integerp b)
         `(sqrt (+ (expt ,a 2) ,(expt b 2))))
        (t form)))
```

In the above example, `FORM` is the original s-expression. If no optimizations
can be made, we simply return it. Otherwise we return an optimized form that
the compiler may choose to use instead.

In reality, for simple functions like `HYPOTENUSE` this is not required --
simply declaring it inline will work when using an advanced compiler like
SBCL. But for more complicated stuff, it can be very handy in times.

Take, for example, regex scanning. CL doesn't support regular expressions out of
the box so we use [CL-PPCRE][cl-ppcre]. This library makes good use of compiler
macros to transform regex strings (which due to their nature are nearly always
available at compile time) into highly optimized closures, thereby preventing
expensive parsing at run time.

[cl-ppcre]: https://edicl.github.io/cl-ppcre/

## Programming Environment

### *, /, +

### DESCRIBE

### APROPOS

### INSPECT

### DISASSEMBLE

### ROOM

### TRACE

### STEP

### TIME

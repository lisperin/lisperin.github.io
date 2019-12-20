---
layout: post
title: Optimizing array operations for multiple element types
permalink: /optimizing-array-ops
category: lisp
---

While working on [qbase64][], I stumbled over a peculiar problem: I wanted it to
work as fast as possible when optimized array types (`SIMPLE-ARRAY`,
`SIMLPE-BASE-STRING`, etc.) were passed to the encoding/decoding routines, but I
also wanted to support the more general types.

[qbase64]: https://github.com/chaitanyagupta/qbase64

For example, the core encoding routine in qbase64, `%ENCODE`, which looks
something like this (simplified):

```cl
(defun %encode (bytes string)
  (loop ;; over bytes and write to string
     ...))
```

goes through the `BYTES` array, taking groups of 3 octets each and writes the
encoded group of 4 characters into `STRING`.

If I declared its types like this:

```cl
(defun %encode (bytes string)
  (declare (type (simple-array (unsigned-byte 8)) bytes))
  (declare (type simple-base-string string))
  (declare (optimize speed))
  (loop ...))
```

SBCL would produce very fast code, but the function would no longer work for
either `ARRAY` or `STRING`:

And if I was to redefine the routine with more general types:

```cl
(defun %encode (bytes string)
  (declare (type array bytes))
  (declare (type string string))
  (declare (optimize speed))
  (loop ...))
```

the code produced would be significantly slower.

My experience with [generics][] is limited, but it seemed that generics could
solve this problem elegantly. However, Common Lisp doesn't have generics, but it
does support macros, so I came up with an ugly-but-gets-the-job-done hack.

[generics]: https://en.wikipedia.org/wiki/Generic_programming

I created a macro, [DEFUN/TD][], that would take all the different type
combinations I wanted to optimize and support upfront:

[DEFUN/TD]: https://github.com/chaitanyagupta/qbase64/blob/6374899aec189600e6b7b77c89009d0835154b93/utils.lisp#L49

```cl
(defun/td %encode (bytes string)
   (((bytes (simple-array (unsigned-byte 8))) (string simple-base-string))
    ((bytes (simple-array (unsigned-byte 8))) (string simple-string))
    ((bytes array)                            (string string)))
  (declare (optimize speed))
  (loop ...))
```

and generate code which would dispatch over the type combinations, then use
[`LOCALLY`][locally] to declare the types and splice the body in:

[locally]: http://www.lispworks.com/documentation/HyperSpec/Body/s_locall.htm

```cl
(defun %encode (bytes string)
  (cond
    ((and (typep bytes '(simple-array (unsigned-byte 8)))
          (typep string 'simple-base-string))
     (locally
       (declare (type bytes (simple-array (unsigned-byte 8))))
       (declare (type string simple-base-string))
       (declare (optimize speed))
       (loop ...)))
    ((and (typep bytes '(simple-array (unsigned-byte 8)))
          (typep string 'simple-string))
     (locally
       (declare (type bytes (simple-array (unsigned-byte 8))))
       (declare (type string simple-string))
       (declare (optimize speed))
       (loop ...)))
    ((and (typep bytes 'array)
          (typep string 'string))
     (locally
       (declare (type bytes array))
       (declare (type string string))
       (declare (optimize speed))
       (loop ...)))
    (t (error "Unsupported type combination"))))
```

The result is more generated code and an increase in the size of the Lisp image,
but now the loop is well optimized for each type combination given to
`DEFUN/TD`. The run-time dispatch might incur a slight penalty, but it is more
than offset by the gains made.

### Alternatives

This was a fairly interesting problem that I hadn't dealt with before,
nevertheless it looked like a fairly common one, so I asked on the cl-pro
list a couple of years ago how others solved this; [Mark Cox][] pointed me to a
few libraries:

[Mark Cox]: https://github.com/markcox80

* [template-function](https://github.com/markcox80/template-function)
* [specialization-store](https://github.com/markcox80/specialization-store)
* [inlined-generic-function](https://github.com/guicho271828/inlined-generic-function)
* [cl-parametric-types](https://github.com/cosmos72/cl-parametric-types)

All of these are quite interesting and attack more or less the same problem in
different ways.

Is there a trick or two that I've missed? Feel free to tell me.

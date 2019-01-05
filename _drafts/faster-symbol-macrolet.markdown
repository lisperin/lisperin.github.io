---
layout: post
title:  "A faster SYMBOL-MACROLET"
permalink: /faster-symbol-macrolet
---

## Introduction

[`SYMBOL-MACROLET`][sm-clhs], alongwith its cousin
[`DEFINE-SYMBOL-MACRO`][dsm-clhs], is a special operator in Common Lisp that
expands symbols into arbitrary forms. It comes in handy when you want to
abbreviate a complex form that is used multiple times inside a body, and simply
binding the form to a variable is not possible.

For example, a value that depends on the current state of a program:

```cl
(let* ((string "hello world")
       (start 0)
       (end (length string)))
  (symbol-macrolet ((len (- end start)))
    (print len)
    ;; make changes, modify bounds
    (incf start)
    (decf end)
    (print len)))
```

Symbol macros are also useful when the same form is used both as a getter and a
setter:

```cl
(defun swap-last (sequence new)
  "Swap the last element in SEQUENCE with NEW and return the previous value"
  (symbol-macrolet ((last (elt sequence (1- (length sequence)))))
    (prog1 last
      (setf last new))))
```

This is put to great use by `WITH-SLOTS` and `WITH-ACCESSORS`, which allow one
to write very concise code when dealing with structs or classes. Both of these
macros expand into `SYMBOL-MACROLET`.

```cl
(with-slots (x y z)
    f
  (if (evenp x)
      (setf y 'foo z 'bar)
      (setf y 'quux z 'baz)))

;; is equivalent to:

(if (evenp (slot-value f 'x))
    (setf (slot-value f 'y) 'foo  (slot-value f 'z) 'bar)
    (setf (slot-value f 'y) 'quux (slot-value f 'z) 'baz))
```

For reference, the expansion of the `WITH-SLOTS` form in the previous example is
equivalent to the following:

```cl
(LET ((#:G855 F))
  (SYMBOL-MACROLET ((X (SLOT-VALUE #:G855 'X))
                    (Y (SLOT-VALUE #:G855 'Y))
                    (Z (SLOT-VALUE #:G855 'Z)))
    (IF (EVENP X)
        (SETF Y 'FOO Z 'BAR)
        (SETF Y 'QUUX Z 'BAZ))))
```

## Performance

One problem with SYMBOL-MACROLET, especially when its used to abbreviate
accessor functions, is that it can slow things down if the getter takes a while
to return. Consider the following:




[sm-clhs]: http://www.lispworks.com/documentation/HyperSpec/Body/s_symbol.htm
[dsm-clhs]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defi_1.htm

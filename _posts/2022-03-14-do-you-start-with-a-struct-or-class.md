---
title: "Do you start with a struct or a class?"
permalink: /do-you-start-with-a-struct-or-a-class
category: lisp
layout: post
---

When creating a new compound data type in Common Lisp, do you make it a struct
or a class? Especially if you are still exploring things, and do not know how
it will evolve.

For some time after I learned CL, I'd always go for a struct. After all,
`defstruct` is so easy to work with -- you get so many useful accessors for
free!

```lisp
(defstruct foo
  a
  b)
```

This gives us the type `FOO`, the functions `MAKE-FOO`, `COPY-FOO` and `FOO-P`
and the slot accessing functions `FOO-A` and `FOO-B` out of the box.

Compare that to the equivalent `defclass` and the only thing you get is the
type. There's no copying function, the type needs to be passed around to
`MAKE-INSTANCE`, `TYPEP` and don't even get me started on the verbosity of
`SLOT-VALUE`. 

That said, at least this problem can be solved by using macros like `WITH-SLOTS`
or `defclass*` (readily available on Quicklisp).

However, there's still the issue of performance. Because there's no dynamic
dispatch, structs are usually faster than classes - plus their functions can be
inlined, and structs themselves can also be stack allocated.

What's not there to like?

The big problem with structs, especially when you are still exploring things, is
modifications. Change the above struct to the following:

```lisp
(defstruct foo
  x
  a
  b)
```

And SBCL will immediately complain with this:

```
WARNING: change in instance length of class FOO:
  current length: 2
  new length: 3

debugger invoked on a SIMPLE-ERROR in thread
#<THREAD "main thread" RUNNING {1004AC0203}>:
  attempt to redefine the STRUCTURE-OBJECT class FOO incompatibly with the
  current definition

Type HELP for debugger help, or (SB-EXT:EXIT) to exit from SBCL.

restarts (invokable by number or by possibly-abbreviated name):
  0: [CONTINUE           ] Use the new definition of FOO, invalidating
                           already-loaded code and instances.
  1: [RECKLESSLY-CONTINUE] Use the new definition of FOO as if it were
                           compatible, allowing old accessors to use new
                           instances and allowing new accessors to use old
                           instances.
  2: [ABORT              ] Exit debugger, returning to top level.
```

For your own sake, just abort (restart 2) or continue (restart 0). In no case
shall ye recklessly continue, because then you are just asking for trouble -- ok
maybe try it just for fun, but don't do this in production!

Classes, on the other hand, are [born to be
redefined](http://clhs.lisp.se/Body/f_upda_1.htm). Add a new slot, or remove an
existing one, your instances will keep working just fine.

And while classes may not be as performant as structs, their performance is good
enough most of the time, even more so when you are exploring
things. [Here's](http://lispm.de/docs/Publications/Common%20Lisp/CLOS%20and%20Efficiency/)
a good collection of articles on CLOS efficiency.

In conclusion, my opinion on this matter has done a 180-degree turn and today I
default to using a `defclass` when exploring new compound types.

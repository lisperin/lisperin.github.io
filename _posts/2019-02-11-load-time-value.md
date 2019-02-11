---
layout: post
title: Easier prepared queries in Postmodern with LOAD-TIME-VALUE
permalink: /load-time-value
category: lisp
---

The Common Lisp library [Postmodern][] defines a macro called `PREPARE` that
creates [prepared statements][] for a PostgreSQL connection. It takes a SQL
query with placeholders (`$1`, `$2`, etc.) as input and returns a function which
takes one argument for every placeholder and executes the query.

The first time I used it, I did something like this:

```cl
(defun run-query (id)
  (funcall (prepare "SELECT * FROM foo WHERE id = $1") id))
```

Soon after, I realized that running this function every time would generate a
new prepared statement instead of re-using the old one. Let's look at the macro
expansion:

```cl
(macroexpand-1 '(prepare "SELECT * FROM foo WHERE id = $1"))
==>
(LET ((POSTMODERN::STATEMENT-ID (POSTMODERN::NEXT-STATEMENT-ID))
      (QUERY "SELECT * FROM foo WHERE id = $1"))
  (LAMBDA (&REST POSTMODERN::PARAMS)
    (POSTMODERN::ENSURE-PREPARED *DATABASE* POSTMODERN::STATEMENT-ID QUERY)
    (POSTMODERN::ALL-ROWS
     (CL-POSTGRES:EXEC-PREPARED *DATABASE* POSTMODERN::STATEMENT-ID
                                POSTMODERN::PARAMS
                                'CL-POSTGRES:LIST-ROW-READER))))
T
```

`ENSURE-PREPARED` checks if a statement with the given statement-id
exists for the current connection. If yes, it will be re-used, else a new one is
created with the given query.

The problem is that the macro generates a new statement id every time it is
run. This was a bit surprising, but the fix was simple: capture the function
returned by `PREPARE` once, and use that instead.

```cl
(defparameter *prepared* (prepare "SELECT * FROM foo WHERE id = $1"))

(defun run-query (id)
  (funcall *prepared* id))
```

You can also use Postmodern's `DEFPREPARED` instead, which similarly defines a
new function at the top-level.

This works well, but now are using top-level forms instead of the
nicely encapsulated single form we used earlier.

To fix this, we can use [`LOAD-TIME-VALUE`][load-time-value].

```cl
(defun run-query (id)
  (funcall (load-time-value (prepare "SELECT * FROM foo WHERE id = $1")) id))
```

`LOAD-TIME-VALUE` is a special operator that

1. Evaluates the form in the null lexical environment
2. Delays evaluation of the form until load time
3. **If compiled**, it ensures that the form is evaluated only once

By wrapping `PREPARE` inside `LOAD-TIME-VALUE`, we get back our encapsulation
while ensuring that a new prepared statement is generated only once (per
connection), until the next time `RUN-QUERY` is recompiled.

## Convenience

To avoid the need to wrap `PREPARE` every time, we can create a converience
macro and use that instead:

```cl
(defmacro prepared-query (query &optional (format :rows))
  `(load-time-value (prepare ,query ,format)))

(defun run-query (id)
  (funcall (prepared-query "SELECT * FROM foo WHERE id = $1") id))
```

## Caveats

This only works for compiled code. As mentioned earlier, the form wrapped inside
`LOAD-TIME-VALUE` is evaluated once only if you compile it. If uncompiled, it is
evaluated every time so this solution will not work there.

Another thing to remember about `LOAD-TIME-VALUE` is that the form is evaluated
in the null lexical environment. So the form cannot use any lexically scoped
variables like in the example below:

```cl
(defun run-query (table id)
  (funcall (load-time-value
            (prepare (format nil "SELECT * FROM ~A WHERE id = $1" table)))
           id))
```

Evaluating this will signal that the variable `TABLE` is unbound.

[Postmodern]: http://marijnhaverbeke.nl/postmodern/
[prepared statements]: https://www.postgresql.org/docs/11/sql-prepare.html
[load-time-value]: http://clhs.lisp.se/Body/s_ld_tim.htm

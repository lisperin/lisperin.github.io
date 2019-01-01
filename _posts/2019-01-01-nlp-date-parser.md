---
layout: post
title: "Writing a natural language date parser"
permalink: /nlp-date-parser
---

In the [deftask](https://deftask.com) blog I described how it allow users to
search for tasks easily [by using natural language date
queries](https://blog.deftask.com/why-are-date-based-searches-so-hard/). It
accomplishes this by using a natural language date and time parser I wrote a
long time ago called [Chronicity](https://github.com/chaitanyagupta/chronicity).

But how exactly does Chronicity work? In this post, we'll dig into its innards
and get a sense of the steps involved in writing it.

If you want to hack into Chronicity, or write your own NLP date parser, this
might help.

*Note:* credit for Chronicity's architecture goes to the Ruby library
[Chronic](https://github.com/mojombo/chronic). It served both as an inspiration
and as the implementation reference.

Broadly, Chronicity follows these steps to parse date and time strings:

1. TOC
{:toc}

## Normalize text

We normalize the text before tokenizing it by doing the following:

1. Lower case the string
2. Convert numeric words (like "one", "ten", "third", etc.) to the corresponding
    numbers
3. Replace all the common synonyms of a word or phrase so that tokenizing
   becomes simpler.

All of this is accomplished by the [`PRE-NORMALIZE`][pre-normalize] function. To
convert numeric words to numbers the [`NUMERIZE`][numerize] function is
used. One caveat: do not immediately normalize the term "second" – it can either
mean the ordinal number or the unit of time. So we wait until after tokenization
(see [pre-process tokens](#pre-process-tokens)) to resolve this ambiguity.

```cl
CHRONICITY> (pre-normalize "tomorrow at seven")
"next day at 7"

CHRONICITY> (pre-normalize "20 days ago")
"20 days past"
```

[pre-normalize]: https://github.com/chaitanyagupta/chronicity/blob/v0.4.1/src/chronicity.lisp#L75
[numerize]: https://github.com/chaitanyagupta/chronicity/blob/v0.4.1/src/numerize.lisp#L67

## Tokenize

Next we assign a token to each word in the normalized text.

```cl
(defclass token ()
  ((word :initarg :word
         :reader token-word)
   (tags :initarg :tags
         :initform nil
         :accessor token-tags)))

(defun create-token (word &rest tags)
  (make-instance 'token
                 :word word
                 :tags tags))
```

As you can see, besides the word, a token also contains a list of tags. Each tag
indicates a possible way to interpret the given word or number. Take the phrase
"20 days ago". The number 20 can be interpreted in many ways:

* It might refer to the 20th day of the month 
* It might be the year 2020
* Or maybe just the number 20 (which is what is actually meant in the given
  phrase)
* It could also refer to the time 8 PM in 24-hour format (20:00 hours)

Remember, we are still in the tokenization phase so we don't know which
interpretation is correct. So we will assign all four tags to the token for this
number.

Each tag is a subclass of the `TAG` class, which is defined as follows.

```cl
(defclass tag ()
  ((type :initarg :type
         :reader tag-type)
   (now :initarg :now
        :accessor tag-now
        :initform nil)))

(defun create-tag (class type &key now)
  (make-instance class :type type :now now))
```

The slot `TYPE` is a misnomer – it actually indicates the designated value of
the token for this tag. For example, the `TYPE` for the year 2020 above will
be the integer 2020. For the time 8 PM it will be an object denoting the time.

The slot `NOW` has the current timestamp. It is used by some tag classes like
`REPEATER` for date-time computations (discussed later).

The various subclasses of `TAG` are:

* [`SEPARATOR`][separator] – Things like slash "/", dash "-", "in", "at", "on",
  etc.
* [`ORDINAL`][ordinal] – Numbers like 1st, 2nd, 3rd, etc.
* [`SCALAR`][scalar] – Simple numbers like 1, 5, 10, etc. It is further subclassed by
   `SCALAR-DAY` (1-31), `SCALAR-MONTH` (1-12) and `SCALAR-YEAR`. A token for any
   number will usually contain the SCALAR tag plus one or more of the subclassed
   tags as applicable.
* [`POINTER`][pointer] – Indicates whether we are looking forwards (hence,
   after, from) or backwards (ago, before). These words are normalized to future
   and past before they are tagged.
* [`GRABBER`][grabber] – The terms this, last or next (as in this month or last
  month).
* [`REPEATER`][repeater] – Most of the date and time terms are tagged using this
   class. This is described in more detail below.

[separator]: https://github.com/chaitanyagupta/chronicity/blob/v0.4.1/src/separator.lisp
[ordinal]: https://github.com/chaitanyagupta/chronicity/blob/v0.4.1/src/ordinal.lisp
[scalar]: https://github.com/chaitanyagupta/chronicity/blob/v0.4.1/src/scalar.lisp
[pointer]: https://github.com/chaitanyagupta/chronicity/blob/v0.4.1/src/pointer.lisp
[grabber]: https://github.com/chaitanyagupta/chronicity/blob/v0.4.1/src/grabber.lisp
[repeater]: https://github.com/chaitanyagupta/chronicity/blob/v0.4.1/src/repeater.lisp

There are a number of [subclasses][repeater-subclasses] of `REPEATER` to
indicate the numerous date and time terms. For example:

* Unit names like year, month, week, day, etc., use the subclasses
   `REPEATER-YEAR`, `REPEATER-MONTH`, `REPEATER-WEEK`, `REPEATER-DAY`.
* `REPEATER-MONTH-NAME` is used to indicate month names like jan or january.
* `REPEATER-DAY-NAME` indicates day names like monday.
* `REPEATER-TIME` is used to indicate time strings like 20:00.
* Parts of the day like AM, PM, morning, evening use the subclass
   `REPEATER-DAY-PORTION`.

[repeater-subclasses]: https://github.com/chaitanyagupta/chronicity/tree/v0.4.1/src/repeaters

In addition, all the `REPEATER` subclasses need to implement a few methods that
are needed for date-time computations.

* `R-NEXT` – Given a repeater and a pointer i.e. `:PAST` or `:FUTURE`, returns a
   time span in the immediate past or future relative to the `NOW` slot. For
   example, assume the date in `NOW` is 31st December 2018.
  * `(r-next repeater :past)` for a `REPEATER-MONTH` will return a time span
   starting 1st November 2018 and ending at 30th November.
  * `(r-next repeater :future)` will return a span for all of January 2019.
  * Similarly, for a `REPEATER-DAY` this would have returned 30th December for
   `:PAST` and 1st January for the `:FUTURE` pointer.
* `R-THIS` is similar to `R-NEXT` except it works in the current context. The
   width of the span also depends on whether direction of the pointer.
  * `(r-this repeater :past)` for a `REPEATER-DAY` will return a span from the
   start of day until now.
  * `(r-this repeater :future)` will return a span from now until the end of
   day.
  * `(r-this repeater :none)` will return the whole day today.
* `R-OFFSET` – Given a span, a pointer and an amount, returns a new span offset
   from the given span. The offset is roughly the amount mulitplied by the width
   of the repeater.

Now we can put the whole tokenization and tagging piece together:

```cl
(defun tokenize (text)
  (mapcar #'create-token
          (cl-ppcre:split #?r"\s+" text)))

(defun tokenize-and-tag (text)
  (let ((tokens (tokenize text)))
    (loop
       for type in (list 'repeater 'grabber 'pointer 'scalar 'ordinal 'separator)
       do (scan-tokens type tokens))
    tokens))
```

As you can see, computing the tags for each token is accomplished by the
`SCAN-TOKENS`. This is a generic function specialized on the class name of the
tag.

One of the methods implementing `SCAN-TOKENS` is shown below.

```cl
(defmethod scan-tokens ((tag (eql 'grabber)) tokens)
  (let ((scan-map '(("last" :last)
                    ("this" :this)
                    ("next" :next))))
    (dolist (token tokens tokens)
      (loop
         for (regex value) in scan-map
         when (cl-ppcre:scan regex (token-word token))
         do (tag (create-tag 'grabber value) token)))))

(defmethod tag (tag token)
  (push tag (token-tags token)))
```

Going back to our original example, for the text "20 days ago", these
are the tags set for each token (after normalization).

```
Token      Tags
-----      ----
20         [SCALAR-YEAR, SCALAR-DAY, SCALAR, REPEATER-TIME]
days       [REPEATER-DAY]
past       [POINTER]
```

## Pre-process tokens

We are almost ready to run pattern matching to figure out the input date, but
first, we need to resolve the ambiguity related to the term second  that we
faced during normalization. At that time, we did not convert it to the number 2 
since it could refer to either the unit of time or the number.

Now with tokenization done, we resolve this ambiguity with a simple hack: if the
term second is followed by a repeater (i.e. month, day, year, january, etc.), we
assume that it is the ordinal number 2nd and not the unit of time. See
[`PRE-PROCESS-TOKENS`][pre-process-tokens] for more details.

[pre-process-tokens]: https://github.com/chaitanyagupta/chronicity/blob/v0.4.1/src/chronicity.lisp#L109

## Pattern matching

The last piece of the puzzle is pattern matching. Armed with tokens and their
corresponding tags, we define several date and time patterns that we know of and
try to match them to their input tokens.

First we name a few pattern classes – each pattern we define belongs to one of
these classes.

* `DATE` – patterns that match an absolute date and time e.g. "1st January",
   "January 1 at 2 PM", etc.
* `ANCHOR` – patterns that typically involve a grabber e.g. "yesterday",
   "tuesday" "last week", etc.
* `ARROW` – patterns like "2 days from now", "3 weeks ago", etc.
* `NARROW` – patterns like "1st day this month", "3rd wednesday in 2007", etc.
* `TIME` – simple time patterns like "2 PM", "14:30", etc.

A pattern, at its simplest, is just a list of tag classes. A list of input
tokens successfully matches a pattern if, for every token, at least one of its
tags is an instance of the tag class mentioned at the corresponding position in
the pattern. For example, the text "20 days ago" had these tags:

```
Token      Tags
-----      ----
20         [SCALAR-YEAR, SCALAR-DAY, SCALAR, REPEATER-TIME]
days       [REPEATER-DAY]
past       [POINTER]
```

It will match any of these patterns:

```cl
(scalar repeater pointer)
(scalar repeater-day pointer)
((? scalar) repeater pointer)
```

The last example shows a pattern with an optional tag – `(? scalar)`. It will
match tokens with or without the scalar e.g. both "20 days ago" and "week ago"
will match.

Our pattern matching engine also allows us to match an entire pattern class. For
example,

```cl
(repeater-month-name scalar-day (? separator-at) (? p time))
```

`(? p time)` here means that any pattern that belongs to the `TIME` pattern
class can match. So all of "January 1 at 12:30", "January 1 at 2 PM" and
"January 1 at 6 in the evening" will match without us needing to duplicate all
the time patterns.

*Note:* There's one limitation -- a pattern class can only be specified at the
end of a pattern in Chronicity. So a pattern like `(repeater (p time) pointer)`
won't work. This will be fixed in the future.

Each pattern has a handler function that decides how to convert the matching
tokens to a date span.

A pattern and its handler function are defined using the
[`DEFINE-HANDLER`][define-handler] macro. It assigns one or more patterns to a
pattern class, and if either of these patterns match, the function body is
run. Its general form is:

```cl
(define-handler (pattern-class)
    (tokens-var)
    (pattern1 pattern2 ...)
  ... body ...
  )
```

An example handler is shown below.

```cl
(define-handler (date)
    (tokens)
    ((repeater-month-name scalar-year))
  (let* ((month-name (token-tag-type 'repeater-month-name (first tokens)))
         (month (month-index month-name))
         (year (token-tag-type 'scalar-year (second tokens)))
         (start (make-date year month)))
    (make-span start (datetime-incr start :month))))
```

Most handler functions will use make use of the the repeater methods `R-NEXT`,
`R-THIS` and `R-OFFSET` that we described above.

Chronicity implements this pattern matching logic in the
[`TOKENS-TO-SPAN`][tokens-to-span] function. All the patterns and their handler
functions are defined inside [handler-defs.lisp][]. Patterns defined earlier in
the file get precedence over those defined later. If you add, remove or modify a
handler, you should reload the whole file rather than just evaluating that
handler's definition.

[define-handler]: https://github.com/chaitanyagupta/chronicity/blob/v0.4.1/src/handlers.lisp#L29
[tokens-to-span]: https://github.com/chaitanyagupta/chronicity/blob/v0.4.1/src/handlers.lisp#L88
[handler-defs.lisp]: https://github.com/chaitanyagupta/chronicity/blob/v0.4.1/src/handler-defs.lisp

## Returning the result

Finally, we put everything together.

```cl
(defun parse (text &key (guess t))
  (let ((tokens (tokenize-and-tag (pre-normalize text))))
    (pre-process-tokens tokens)
    (values (guess-span (tokens-to-span tokens) guess) tokens)))
```

By default `PARSE` will return a timestamp instead of a time span. This depends
on the value passed to the `:GUESS` keyword -- see the `GUESS-SPAN` function to
see how it is interpreted. If you want to return a time span send `NIL` instead.

The second value that this function returns is the list of tokens alongwith all
its tags. This is useful for debugging Chronicity results in the REPL.

```cl
CHRONICITY> (parse "20 days ago")
@2018-12-12T12:01:53.758578+05:30
(#<TOKEN 20 [SCALAR-YEAR, SCALAR-DAY, SCALAR, REPEATER-TIME] {1007639243}>
 #<TOKEN days [REPEATER-DAY] {10076AF5D3}> #<TOKEN past [POINTER] {1007553443}>)

CHRONICITY> (parse "20 days ago" :guess nil)
#<SPAN 2018-12-12T00:00:00.000000+05:30..2018-12-13T00:00:00.000000+05:30>
(#<TOKEN 20 [SCALAR-YEAR, SCALAR-DAY, SCALAR, REPEATER-TIME] {1001B78BC3}>
 #<TOKEN days [REPEATER-DAY] {1001B78C03}> #<TOKEN past [POINTER] {1001B78C43}>)
```

The actual `PARSE` function has a few more bells and whistles than the one
defined here:
*  `:ENDIAN-PREFERENCE` to parse ambiguous dates as dd/mm (`:LITTLE`) or mm/dd
(`:MIDDLE`)
* `:AMBIGUOUS-TIME-RANGE` to specify whether a time like 5:00 is in the morning
  (AM) or evening (PM).
* `:CONTEXT` can be `:PAST`, `:FUTURE` or `:NONE`. This determines the time span
  returned for strings like "this day". See the definition of `R-THIS` above.

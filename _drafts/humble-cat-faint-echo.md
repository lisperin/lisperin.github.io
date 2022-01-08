---
layout: post
title: "The humble cat and the faint echo"
permalink: /humble-cat-faint-echo
category: unix
---

You can get pretty far in a Unix shell with `cat` and `echo`.

## cat

In its simplest form (i.e. without any arguments), `cat` reads from its standard
input and writes this to its standard output.

By itself, that's not very useful. But combine it with some facilities provided
by the Unix shell, and things become interesting.

To read a file, redirect its input using `<`:

```sh
# Who needs arguments when you can redirect?
cat <path/to/file
```

To create a file, redirect its output using `>`:

```sh
# Press Ctrl-d on a new line to finish file creation
cat >/path/to/new/file
```

If using modifier keys is not your thing, you can instead redirect input using
the heredoc:

```sh
cat <<EOF >/path/to/new/file
> type your document
> until you write "EOF"
> on a newline
> EOF
```

Appending to a file is easy too using `>>`:

```sh
# Whatever you type on stdin will get appended
# to the file and not be redirected
cat >>/path/to/file
```

Copying a file? No problem - just read from one file and write to the other:

```sh
cat </path/to/source >/path/to/destination
```

Since it works with stdin and stdout, `cat` can of course participate in pipes:

```sh
cat </path/to/file | grep pattern
cat </path/to/file | tr -d '\r'
```

That said, `cat` doesn't find much use in pipes since most commands either
explicitly take a file arg, and if they don't, one can directly use the
redirection operator with them.

```sh
grep pattern /path/to/file
tr -d '\r' </path/to/file
```

There's one good use though: if a command does some funny stuff by detecting the
terminal (like coloring or otherwise), and you don't like it, piping the output
through `cat` quickly fixes it.

```sh
# Get rid of jq's coloring and other oddities
jq . /path/to/file.json | cat
```

## echo

`echo` is even simpler

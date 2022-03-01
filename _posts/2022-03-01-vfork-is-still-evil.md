---
title: "vfork() is still evil"
permalink: /vfork-is-still-evil
category: posix
layout: post
---

Yesterday I read a
[post](https://gist.github.com/nicowilliams/a8a07b0fc75df05f684c23c18d7db234)
that rejects the conventional wisdom on `fork()` vs `vfork()` and asserts that
`fork()` is evil and `vfork()` is good.

The essence of that post is that `fork()` is slow and expensive, whereas
`vfork()` is fast and cheap. Therefore `vfork()` is good, and `fork()` is bad.

That's wrong.

`vfork()` is a pre-mature optimization, and a highly dangerous one at
that. Pre-mature optimization is the root of all evil. Therefore, `vfork()` is
still evil.

`vfork()` has a significant problem, and the post in question alludes to it:

> vfork() does have one downside: that the parent (specifically: the thread in
> the parent that calls vfork()) and child share a stack, necessitating that the
> parent (thread) be stopped until the child exec()s or _exit()s.

Unfortunately, it completely glosses over the real problem because the focus
here is on the parent process being blocked. The blocking behaviour is just a
symptom, the real problem here is that **the stack is shared** between the
parent and the child process.

More generally, the entire memory of the parent is shared with the child until
an `exec()` call is made or the child exits.

Here's what the Linux manual says about `vfork()`.

> (From POSIX.1) The vfork() function has the same effect as fork(2), except
> that the behavior is undefined if the process created by vfork() either
> modifies any data other than a variable of type pid_t used to store the return
> value from vfork(), or returns from the function in which vfork() was called,
> or calls any other function before successfully calling _exit(2) or one of the
> exec(3) family of functions.

And from the macOS/BSD system calls manual:

> Many problems can occur when replacing fork(2) with vfork().  For example, it
> does not work to return while running in the child's context from the pro-
> cedure that called vfork() since the eventual return from vfork() would then
> return to a no longer existent stack frame.  Also, changing process state
> which is partially implemented in user space such as signal handlers with
> libthr(3) will corrupt the parent's state.
>
> Be careful, also, to call _exit(2) rather than exit(3) if you cannot
> execve(2), since exit(3) will flush and close standard I/O channels, and
> thereby mess up the parent processes standard I/O data structures.  (Even with
> fork(2) it is wrong to call exit(3) since buffered data would then be flushed
> twice.)

You cannot blindly replace calls to `fork()` with `vfork()`.

`fork()` has multiple use cases, but `vfork()` has only one: when you want to
call the `exec()` family of functions after `vfork()`. That is, when you want to
launch a new process.

And be careful what you do in the child process before calling `exec()`. As
we've seen above, anything that modifies memory is unsafe. So is calling any
function that is not
[async-signal-safe](https://man7.org/linux/man-pages/man7/signal-safety.7.html).

An interesting consequence of all this is that while calling `dup2()` (to
redirect stdin/out) between `vfork()` and `exec()` is safe, if the call to
`dup2()` itself fails, there is no easy way to signal to the user what went
wrong. That is because all of stdio is NOT async-signal-safe.

All said and done -- just stick to `fork()`. Sure, `fork()` has its problems and
caveats, especially when you throw threads into the mix, but it is almost always
the better choice when compared to `vfork()`. Use `vfork()` only when you truly
need its performance benefits, and understand its caveats well.

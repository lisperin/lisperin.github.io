---
title: fork-exec for Python programmers
permalink: /fork-exec-python
category: python
layout: post
---

1. TOC
{:toc}

## fork

[fork()][os.fork] can magically make your program do things twice. Don't believe me? Let's
run this small program and see for ourselves. Create a file called `fork.py` and
save the following code in it.

[os.fork]: https://docs.python.org/3/library/os.html#os.fork

```python
import sys
import os
import time

sys.stdout.write('Ready to fork? (Press enter to continue) ')
sys.stdout.flush()

sys.stdin.readline()

os.fork()

print('I will print twice')

time.sleep(10)

print('I will also print twice')
```

Now run this program (make sure you use Python 3), press enter on the "Ready to
fork?" prompt and observe the output. Curiously, the print statements following
the `fork()` call do indeed print twice!

What's happening? To understand this, run the program again, but do not press
enter on the "Ready to fork?" prompt. Now open another terminal window, and
observe the output of the following `ps -af` command.

The output would look something like this:

```
UID          PID    PPID  C STIME TTY          TIME CMD
ubuntu     80568   80012  0 23:31 pts/1    00:00:00 python3 fork.py
ubuntu     80571   80547  0 23:31 pts/0    00:00:00 ps -af
```

Now, press enter, then quickly switch to the other terminal window and and run
`ps -af` again (before the 10 second sleep call runs out).

This time you will look like this:

```
UID          PID    PPID  C STIME TTY          TIME CMD
ubuntu     80568   80012  0 23:31 pts/1    00:00:00 python3 fork.py
ubuntu     80579   80568  0 23:33 pts/1    00:00:00 python3 fork.py
ubuntu     80580   80547  0 23:33 pts/0    00:00:00 ps -af
```

What's happening here? Are we really running our program twice?

Well yes we are!

To understand this better, let's first understand what `ps` does. `ps` just
lists the actively running processes on a system.

And what's a [process][]? A process is what an operating system creates when you
ask it to run a program. A process usually consists of the following things:

* A representation of the program's executable code in memory (the program in
  this case is `python3`).
* The processor state, i.e. the contents of all of its registers, including the
  [instruction pointer][].
* The [call stack][]. The processor state combined with the call stack will
  usually tell you what a program is doing at any point of time.
* The heap, which is where all Python objects and data structures are
  stored (see [memory management in Python][]).
* A list of external resources that may have been allocated to the process, for
  example, any open files or sockets.
* A process identifier, called the pid (see PID column in the output of the `ps`
  command)

[process]: https://en.wikipedia.org/wiki/Process_(computing)
[instruction pointer]: https://en.wikipedia.org/wiki/Program_counter
[call stack]: https://en.wikipedia.org/wiki/Call_stack
[memory management in Python]: https://realpython.com/python-memory-management/

When you call `fork()`, the OS makes an almost identical copy of the current
process, which is called the child process. And the process in which the
`fork()` call is made of course becomes the parent of this newly created child
process. In the output of the `ps` command observe the values of the PID and
PPID (i.e. parent PID) columns for both the python processes.

The child process, after creation, continues execution from the point at which
`fork()` returns. This is why you see duplicate output for both the print
statements in our program.

It is important to note that both the parent and the child process run in
parallel after the `fork()` call is made, even on systems with only a single
core, single processor CPU. This is possible due to [multitasking][].

You might also have noticed that even though the print calls are made twice, the
sleep lasts only for 10 seconds and not 20 seconds. This is a direct consequence
of the processes running in parallel.

[multitasking]: https://en.wikipedia.org/wiki/Computer_multitasking

**Exercise 1**: Inside a running Python process, you can get its pid using the
`os.getpid()` function. Modify the print statements above to also include pid
and observe the output.

**Exercise 2**: Remove the call to `time.sleep()` in the program above and
observe the output.

## Am I inside the parent or the child?

One problem with the code we've written till now is this - after `fork()`
returns, inside the respective processes, how do you identify which one is the
parent and which one is the child?

One possible solution is to do something like this:

```python
import sys
import os
import time

PARENT_PID = os.getpid()

sys.stdout.write('Ready to fork? (Press enter to continue) ')
sys.stdout.flush()

sys.stdin.readline()

os.fork()

PID_AFTER_FORK = os.getpid()

if PID_AFTER_FORK == PARENT_PID:
    print('Inside parent')
else:
    print('Inside child')
```

This should work, but fork provides an easier way: the return value of the
`fork()` call is `0` in the child process, and it is set to the pid of the child
in the parent process. That is, this should also work:

```python
import sys
import os
import time

sys.stdout.write('Ready to fork? (Press enter to continue) ')
sys.stdout.flush()

sys.stdin.readline()

PID_AFTER_FORK = os.fork()

if PID_AFTER_FORK > 0:
    print('Inside parent')
else:
    print('Inside child')
```

**Exercise 3**: After the fork, let the parent run to completion but put the
child to sleep. Observe the output of `ps -af`. What happens to the chlid's
parent PID after the parent exits?

**Exercise 4**: If the child process prints something after the parent process
exits, what happens to its output?

**Exercise 5**: Write a function, `launch_child`, that takes a function `fn` and
any number of positional and keyword arguments as params. This function should
create a child process, call `fn` inside the child process and pass it all the
positional and keyword arguments that were passed to it. After `fn` finishes
running, the child process should exit.

To test your `launch_child` function, use the following program:


```python
import sys
import os
import time

def launch_child(fn, *args, **kwargs):
    # Your implementation of launch_child here

def print_with_pid(*args, **kwargs):
    print(os.getpid(), *args, **kwargs)

sys.stdout.write('Ready to fork? (Press enter to continue) ')
sys.stdout.flush()

sys.stdin.readline()

PID_OF_CHILD = launch_child(print_with_pid, 'This prints inside child')
print_with_pid('child pid is', PID_OF_CHILD)
```

It is important that child process must exit immediately after `fn`
returns. Which means that the "child pid is ..." line MUST NOT print inside the
child process.

## Waiting for children to exit

Two things that might be important for a parent process - it might want to wait
till a child process completes, and it might want to know whether a child
process run successfully or not.

Success or failure of a process is usually indicated by a number which is called
its exit status. You can set the exit status of a Python process by calling
[sys.exit()][sys.exit]. Calling this function gracefully terminates your Python
process (by ensuring that the finally clauses of the try statement are run), and
sets the exit status to the value passed to it.

An exit status can be between 0 and 127. 0 means success, everything else
indicates failure.

A parent process can wait for a child process by using the
[os.waitpid()][os.waitpid] call. waitpid() takes a child pid as argument
alongwith an integer specifying options (usually set to 0). It returns a tuple
containing the child pid and exit status indication. The exit status indication
is a 16-bit number whose low byte is the [signal][] number that killed the
process, and whose high byte is the exit status (if the signal number is
zero). For now we will only worry about the exit status.

```python
import sys
import os
import time

sys.stdout.write('Ready to fork? (Press enter to continue) ')
sys.stdout.flush()

sys.stdin.readline()

PID_AFTER_FORK = os.fork()

if PID_AFTER_FORK > 0:
    print('Inside parent')
    status_encoded = os.waitpid(PID_AFTER_FORK, 0)[1]
    print('Inside parent, child exited with code', status_encoded >> 8)
else:
    print('Inside child')
    time.sleep(2)
    sys.exit(127)
```

[os.waitpid]: https://docs.python.org/3/library/os.html#os.waitpid
[sys.exit]: https://docs.python.org/3/library/sys.html#sys.exit
[signal]: https://en.wikipedia.org/wiki/Signal_(IPC)

**Exercise 6**: Write a program that creates multiple children, and then waits
for them. If any child exits, your program should print the pid of the child
that exited.

**Exercise 7**: Which process is the parent of the parent Python process? You
can figure this out by using the `ps` command.

**Exercise 8**: How can you check the exit status of the last program that was
run by a unix shell (e.g. bash).

**Exercise 9**: In bash, how do you run a series of commands one after another?
The only constraint is that a command should run only if the previous one
succeeded. That is, the pipeline should stop on first failure.

**Exercise 10**: Conversely, how do you run a pipeline of commands which should
stop on first success?

## Memory

When a child is forked, it gets an almost identical copy of all the memory
segments of the parent. However, it is a copy - once forked, any further
modifications made to any memory location by the parent process does not reflect
in the child, and vice versa. This can be tested with the simple program
below.

```python
import sys
import os
import time

X = 100
Y = dict(foo=123)

if os.fork() > 0:
    print('Inside parent')
    X = 200
    Y['foo'] = 456
    print('Inside parent, X:', X)
    print('Inside parent, Y:', Y)
    # wait for child to complete
    time.sleep(3)
else:
    print('Inside child')
    time.sleep(2)
    print('Inside child, X:', X)
    print('Inside child, Y:', Y)
```

## Files

Memory isolation between processes is fairly easy to grasp. What may not be so
easy to understand is how external resources like files work when a fork
happens.

**Exercise 11**: Consider the following program that writes to a file from two
processes:

```python
import sys
import os
import time


with open(sys.argv[1], 'w') as f:
    if os.fork() > 0:
        for i in range(10):
            print('writing from parent, chunk:', i)
            f.write('aaa\n')
            time.sleep(1)
    else:
        time.sleep(0.5)
        for i in range(10):
            print('writing from child, chunk:', i)
            f.write('bbb\n')
            time.sleep(1)
```

Notice that the same file handle, `f`, is open and available inside both the
child and the parent.

Without running it, can you say what this program will do? Keep in mind the fact
that you are dealing with buffered I/O.

* Now run it, what do you observe?
* If you move the initial sleep() from the chld to the parent, does
it change what gets written to the file?
* If you randomize the sleep timings inside the loop, does it change anything?
* What happens if you flush the output after every write?

**Exercise 12**: Consider the following program that reads a file linewise from
two processes:

```python
import sys
import os
import time


with open(sys.argv[1], 'r') as f:
    if os.fork() > 0:
        for line in f:
            print('reading from parent:', line, end='')
            time.sleep(1)
    else:
        time.sleep(0.5)
        for line in f:
            print('reading from child:', line, end='')
            time.sleep(1)
```

Again, keeping in mind that you are dealing with buffered I/O, what do you think
will happen when this program is run?

* Run this program with a small file as input - perhaps one with fewer than 10
  lines, or the file generated by the program in the previous exercise. Explain
  why the program behaves the way it does.
* Now run it against a large file. A good candidate would be the [words
  file][]. Again, explain why it behaves the way it does.

[words file]: https://en.wikipedia.org/wiki/Words_(Unix)

**Exercise 13**: If, instead of reading a file, we instead tried to read the
standard input linewise in both the parent and the child, what would happen?
Modify the program in the previous exercise to read from stdin instead and
explain the behaviour.

## Inter-process communication

There are many ways for two processes on the same system to communicate with one
another. One way to do it us to use [pipes][]. Pipes are most commonly used in
the shell to send ouptut of one command to another. For example,

```
ps -eaf | grep python | less
```

The following program uses a pipe to send a message from the child process to
the parent:

```python
import sys
import os
import time

read_fd, write_fd = os.pipe()

if os.fork() > 0:
    # Close the write fd in parent, since we don't need it here
    os.close(write_fd)
    print('In parent, waiting for child to write something')
    bytes_read = os.read(read_fd, 10)
    print('In parent, child wrote:', bytes_read)
    os.close(read_fd)
else:
    # Close the read fd in child, since we don't need it here
    os.close(read_fd)
    time.sleep(1)
    print('In child, writing something')
    os.write(write_fd, b'hello')
    os.close(write_fd)
```

Here's how this works: the function [pipe()][os.pipe] returns two file
descriptors - `read_fd` and `write_fd`. Any data written to `write_fd` can be
read on `read_fd`.

[File descriptors][], or "fds" in short, are positive integers that actually
power many operations on Unix - including files, sockets and pipes, among
others. In fact, the high level file API in Python is actually built on top of
file descriptors and the following system calls:

* [os.open()](https://docs.python.org/3.7/library/os.html#os.open) opens a file
  and returns the fd. As the fd is only an integer, the data structures that
  manage the state of the open file are not available to the user process -
  these are managed by the operating system itself.
* [os.close()](https://docs.python.org/3.7/library/os.html#os.close) cleans up
  any resources (data structures, etc.) allocated by the operating system for
  this file.
* [os.read()](https://docs.python.org/3.7/library/os.html#os.read) reads from a
  file. This is a raw unbuffered API that only returns bytes and not strings.
* [os.write()](https://docs.python.org/3.7/library/os.html#os.write) writes to a
  file. Again, this is a raw unbuffered API that only works with bytes.

The high level buffered API provided by Python is built on top of the raw
unbuffered API provided by `os.read()` and `os.write()`.

When a fork happens, any file descriptors open in the parent process remain open
in the child process. This is actually why files opened in a parent processs
remain open in a child, as we covered in the previous section.

Now back to pipes - in our case, the child process wants to send a message to
the parent process. So child will write to `write_fd` and
the parent reads from `read_fd`.

Also, we want to close the fds we don't need. As the parent process has no use
for `write_fd`, it closes this fd immediately after the fork. And as the child
process has no use for `read_fd`, it closes this fd as soon as it is created.

After everything is done, the other fd is also closed by both the processes.

Pipes is not the only way for two processes to communicate with each other. The
wikipedia page on
[IPC](https://en.wikipedia.org/wiki/Inter-process_communication) lists the
different approaches available.

**Exercise 14**: Write a program that launches multiple child processes. Provide
a unique writable fd to each child. Whenever any child writes to its writable
fd, the parent should print the byte string to console. You may need to use
[select()](https://docs.python.org/3.7/library/select.html#select.select) for
this.

**Exercise 15**: The function `map` takes at least two arguments - another
function and an iterable. It applies the given function to each element in the
iterable, and returns a new iterator with the result.

```python
>>> map(round, [1.4, 3.5, 7.8])
<map object at 0x10df15470>

>>> list(_)
[1, 4, 8]
```

Write a new function, `pmap` (parallelized map) that works similarly to
`map`. It should take a function and an iterable as an argument. The difference
is that it should apply the function to each element in a separate child
process. The parent should then assemble the results in a new list and return.

You will need to use the [pickle][] module to serialize object values between
parent and child processes - `pickle.dumps()` and `pickle.loads()` should be
sufficient.

[Os.pipe]: https://docs.python.org/3.7/library/os.html#os.pipe
[pipes]: https://en.wikipedia.org/wiki/Pipeline_(Unix)
[file descriptors]: https://en.wikipedia.org/wiki/File_descriptor
[pickle]: https://docs.python.org/3.7/library/pickle.html

## Exec

[exec](https://en.wikipedia.org/wiki/Exec_(system_call)) is another magical
piece of functionality in Unix systems. exec is how you run an executable in
unix. It causes the program that is currently being run by the calling process
to be replaced with a new program, with newly initialized stack, heap, and
(initialized and uninitialized) data segments.

In other words, the new executable is loaded into the current process, and will
have the same process id as the caller.

Let's see it in action:

```
import sys
import os

sys.stdout.write('''Provide program name and args to run like you would in a shell.

Examples:

ls
ls -al
ls -l file1 file2 file3

$ ''')
sys.stdout.flush()

program_and_arguments = sys.stdin.readline().rstrip().split()

program = program_and_arguments[0]
arguments = program_and_arguments[1:]

os.execlp(program, program, *arguments)

sys.stdout.write('I executed a program\n')
sys.stdout.flush()
```

The exec functionality here is provided by `os.execlp()`. Run the program above
and provide program name and args to run - what happens? Did you see the string
"I executed a program" in the output? If no, why not?

The Python interface to exec is provided by the `os` module, and is [documented
here](https://docs.python.org/3/library/os.html#os.execl). You will notice that
exec is not a single function but a family of functions. All these variants
provide the same functionality, differing only in one or more of the following:

1. How arguments are passed
2. How the executable is looked up i.e. whether to consult `PATH` or not.
3. Whether the environment is modified or not

The modifiers **e**, **l**, **p** and **v** appended to the name "exec" tell us
what combination of the above functionality is provided by a given variant. The
documentation explains this in greater detail.

One thing you might have noticed is that in the invocation of `execlp()` above
the program name was given twice. The first one tells execlp which program to
run. The second one actually becomes the first argument (arg0) to the
program. It is recommended that the first argument is always the name of the
program, but this is not enforced.

You can test this by compiling and running the following C program from our
program above, and passing a different arg0 rather than the program name (Python
does some funky stuff with `sys.argv[0]`, which is why we are using a C program
as our target here):

```c
#include<stdio.h>

int main(int argc, char **argv) {
  printf("No of arguments: %d\n", argc);
  for (int i = 0; i < argc; ++i) {
    printf("argv[%d]: %s\n", i, argv[i]);
  }
}
```

Since exec replaces the current process with a different program, how do we
launch another program yet retain our current process?  Simple, fork and then
exec. This is the classic Unix-y way of launching a new process, and is in fact
what your shell probably does. We will attempt to do the same in the exercise
that follows.

**Exercise 16**: Can you verify that the process running before and after `exec`
is the same i.e. the pid remains the same before and after the call to `exec`?

**Exercise 17**: Create a function, `launch_program(program_name, *args)` that
takes a program name and its arguments, if any. It should run the program in a
separate process, wait for the program to exit, and after it does exit, return
its exit status in the parent process.

**Exercise 18**: (Optional) Create a function, `pipeline(commands)`. `commands`
should be a list of commands. Each command is of the form `[program_name, arg0,
arg1, ...]` i.e. it names a program and its arguments. `pipeline()` should
launch each of these commands in parallel, and pipe the output of the first
command to the second, the second command to the third, and so on. That is, the
following,

```python
pipeline(["ls", "-al"], ["grep", "-F', ".py"], ["wc", "-l"])
```

should work the same as

```
ls -al | grep -F .py | wc -l
```

The function should wait for all the commands to exit, and return their exit
status codes in an array.

Besides using fork, exec, pipe and wait, you will need one more function to make
this work: [dup2](https://docs.python.org/3/library/os.html#os.dup2). `dup2` is
also pretty special - it allows you to duplicate a given fd to a target fd of
*your choice*. This means you can duplicate one of the pipe fds to stdin or
stdout as required. This setting up of pipes will probably need to be done
between the calls to fork and exec.

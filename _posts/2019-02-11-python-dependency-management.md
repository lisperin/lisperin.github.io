---
title: Dependency mangement for Python projects
permalink: /python-dependency-management
category: python
layout: post
---

I started working on Python recently and we need a dependency manager that gives
us [reproducible builds][], similar to [bundler][] or [npm][].

In a nutshell, we need to ensure that the same code is being run everywhere,
including the project source, its libraries and the version of Python on which
it is run.

Below are some quick notes on one way to achieve this.

[reproducible builds]: https://en.wikipedia.org/wiki/Reproducible_builds
[bundler]: https://bundler.io/
[npm]: https://www.npmjs.com/

1. TOC
{:toc}

## Summary

We will use [pipenv][] and [pyenv][] to get this done.

*pipenv* is a package manager that uses [pip][] and [virtualenv][] under the
hood. The project's direct dependencies are added to a `Pipfile`, and the
dependency graph is locked down in `Pipfile.lock`, which is generated
automatically and never touched by hand. The lock file is crucial for
reproducible builds, we will see how that is under project syncing.

*pyenv* makes it a breeze to install and manage multiple versions of Python. You
specify the desired Python version in your `Pipfile` and *pipenv* will use
*pyenv* to fetch and install the relevant Python version.

[pipenv]: https://pipenv.readthedocs.io/en/latest/
[pyenv]: https://github.com/pyenv/pyenv
[pip]: https://pypi.org/project/pip/
[virtualenv]: https://virtualenv.pypa.io/en/latest/

## Installation

### pyenv

1. See [pyenv installation instructions](https://github.com/pyenv/pyenv#installation)
2. While installing *pyenv* is pretty simple, however building a brand new
   Python (which is what *pyenv* does) may create problems, so make sure to go
   through pyenv's wiki entry on [common build
   problems](https://github.com/pyenv/pyenv/wiki/common-build-problems).
3. Make sure that you add `eval "$(pyenv init -)"` towards the **end** of your
   shell's init file (e.g. `~/.bash_profile`, `~/.profile` or `~/.bashrc`).

### pipenv

If you use Homebrew or Linuxbrew you can simply run

```
brew install pipenv
```

Otherwise you will need to make use of the Python and Pip that already ship with
your OS, or get it via *pyenv*. And then run something like:

```
pip install --user pipenv
```

Yeah, installing *pipenv* itself requires Python and pip. But this only needs to
be done once.

See [Installing Pipenv][] for more details.

[Installing Pipenv]: https://pipenv.readthedocs.io/en/latest/install/#installing-pipenv

## Usage

Make sure that pyenv and pipenv are installed as indicated in the previous section.

### Setting up a new project

1. Create a project directory e.g. `mkdir test`
2. `cd test`
3. Setup Python for your project: `pipenv install --python 3`. This will create
   a `Pipfile` and `Pipfile.lock` in the project directory.

   If you use this command, by default pipenv will try to pick the Python 3
   available on your system. If it doesn't find one, it will ask if you want it
   to fetch a Python from pyenv.

   If you want a more specific version of Python, use: `pipenv install --python
   3.7`.
4. Install the libraries that your project depends on using `pipenv install`.

   ```
   pipenv install django~=2.1.5
   pipenv install djangorestframework~=3.9.1
   ```

   You can skip specifying the version, but I won't recommend doing that. Note
   the use of the `~=` operator. It is the [compatible release operator][] and
   essentially means that a breaking version of the library won't be installed
   when you try to update it. More on this under updating dependencies.
5. Add `Pipfile` and `Pipfile.lock` to version control. Now you can share your
   project with the team.
   
[compatible release operator]: https://www.python.org/dev/peps/pep-0440/#compatible-release

### Syncing a project

Fetch the project from version control. Make sure that it contains both
`Pipfile` and `Pipfile.lock`. 

1. Go to the project's directory
2. Run `pipenv sync`

That's it. pipenv will install all your project's dependencies (including
Python, via pyenv) and allow you to start using them.

`pipenv sync` only looks at `Pipfile.lock`, installs the given dependencies
locally and ensures that the hashes match. This is exactly what we need to
ensure that the build is reproducible.

You should `pipenv sync` everytime the project's dependencies are updated.

### Running a project

There are two ways to run our project using the newly installed Python and
libraries:

The first is to invoke `pipenv shell`. This will drop you into a new shell with
`PATH` and `sys.path` setup so that you get the correct version of
everything. You can exit this shell at any time via `Ctrl-D` or `exit`.

The other way is to use `pipenv run <cmd>`. E.g. If you are, say, running
django, all you need to do is `pipenv run python manage.py runserver` and
everything should work as expected.

### Updating dependencies

How do you upgrade a library to a newer version?

One way is to simply run `pipenv update name-of-library`. If you used the
compatible release operator, which you should, this will update the library to
the newest version allowed by this operator.

For example, if you specified `django~=2.0.0` in your `Pipfile`, then `pipenv
update django` will update django to the highest version available under 2.0.x
but not to a newer version in the 2.1.x series.

And if you specified `django~=2.0`, then it will update django to the highest
version available under 2.x but will not go up to 3.x.

If you want to update django to a higher version than the one allowed by the
compatible release operator, you need to use the `install` subcommand i.e. do
something like `pipenv install django~=2.1.0`.

The other way to do this is to simply update the `Pipfile` by hand, and
subsequently run `pipenv install`. This will install the specified library
version and also update `Pipfile.lock`.

## Conclusion

Once you get past the installation hurdle, it seems easy and simple enough to
use *pipenv* (with help from *pyenv*) to manage a project's dependencies and get
reproducible builds.

For more on *pipenv*, you can go through:

* [Basic usage](https://pipenv.readthedocs.io/en/latest/basics/)
* [Advanced usage](https://pipenv.readthedocs.io/en/latest/advanced/)
* [Reference](https://pipenv.readthedocs.io/en/latest/#pipenv-usage)

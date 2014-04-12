autotools-project
=================

Interactively creating a skeleton for a C/C++ project using Autotools (GNU build system) like `cabal init` for Haskell projects.

## Prerequisites

`autoconf`, `automake`, and `libtool` must be installed before using in your system.

## Installation and Usage

```shell
cabal configure
cabal build
cabal install
mkdir -p ~/my-awesome-c-project && cd ~/my-awesome-c-project
autotools-project
./configure
make
./src/hello
make clean
```

You may have to modify `configure.ac` and `src/Makefile.am` in the new project.

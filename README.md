# Reticulated

## Note

I've decided to abandon this project, but I'm
leaving the source up for anyone who would want to inspect it or use it.
There are many better languages these days then Python. I don't recommend
trying to improve upon Python when you can use a language built from
scratch.

## Introduction

Reticulated is a highly optimized subset of the Python language written in Rust.

## Motivation

Python is a popular language for numerical computation and web services.
Python shines not in its performance, but in its elegant syntax and
extensive standard library. The libraries for numerical computation are 
usually written in C for speed, and CPython's global interpreter lock cripples 
its performance in web services which rely heavily on multi-threading.

Reticulated aims to overcome the performance limitations of CPython by making
different decisions, omitting features of CPython if necessary. Because it may
be necessary to drop support for features that would make optimization
difficult, Reticulated is not a complete implementation of the Python language.

### Use cases

* A language for highly concurrent web servers
* A tool for research and study of interpreted languages

### Goals

* Be faster than CPython in single-threaded performance
* Eliminate the global interpreter lock
* Document and write code that can be used as learning or reference material

### Non-goals

These are not part of the initial goal of the project, but may become goals
later as the project matures.

* Be faster than Pypy in single-threaded performance
* Become a drop-in replacement for CPython
* Support C bindings and libraries such as numpy

## Differences from CPython

This is a list of all differences from CPython as they are implemented.

* Type comments are not parsed
* Zeros are allowed before decimals. E.g. `0123`

## Design

Reticulated is intended to be modular enough that experimenting with different
designs is possible. E.g., a stack vs register virtual machine, or different
instruction sets. Currently no decision has been made for the design of the
compiler, bytecode, and interpreter. 

## Organization

The code is split up into rust modules that correlate roughly to the frontend
and backend of a compiler. This organization should make it relatively
straightforward to swap out parts of the pipeline to test.

### Frontend

* `lexer`
* WIP: `parser`
* TODO: `optimizer`

### Backend

* TODO: `compiler`
* TODO: `interpreter`
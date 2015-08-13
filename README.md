`cl-faster-input` A project for exploring and improving large files
input, parsing, and processing in the Common Lisp applications.

Standard `read-line` function is being blamed often for being too slow
and inefficient. Lisp offers a number of alternatives for reading and
processing text files and large logs.

This project will explore them all.

Check current benchmark results at [the project wiki.](https://bitbucket.org/vityok/cl-faster-input/wiki/Home)

How to reproduce:

1. clone this repository into the `quickllisp/local-projects`
   directory (or any other, but don't forget to symlink the `.asd`
   file!)

2. start your lisp

3. load the benchmarks: `(ql:quickload :cl-faster-input)`

4. run the benchmarks by executing one of:

```lisp
(fio:benchmark-count-lines)
(fio:benchmark-mmap)
(fio:benchmark-read-line
```

Any suggestions, corrections and ideas are welcome!
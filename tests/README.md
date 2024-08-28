# Test Suites

I have prepared extensive test suites for this library that make use of the **[Rove](https://github.com/fukamachi/rove)** library.

Every major function in this library is thoroughly tested, including all general functions and ciphers.

Note that these tests must be run on a Linux machine. (When I have previously attempted to run these tests on Windows in Portacle or the like, they would not run due to exhaustion of the heap.) If you are using a Windows computer, I recommend running these tests through Windows Subsystem for Linux (WSL).

## Running the tests

To prepare to run the tests, first run SBCL, and then enter the following line, making sure to provide the appropriate path for the asd file:

```common-lisp
(asdf:load-asd #p"provide/path/to/clyptic.asd")
```

### To run general tests

```common-lisp
(asdf:load-system "clyptic/tests/general")
(rove:run "clyptic/tests/general")
```

### To run cipher tests

```common-lisp
(asdf:load-system "clyptic/tests/ciphers")
(rove:run "clyptic/tests/ciphers")
```

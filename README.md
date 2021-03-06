Guile-spec
==========

Guile-spec is an RSpec/Jasmine-like BDD testing library for Guile Scheme.

Test Suites
-----------
Test suites encapsulate test cases, called specs. Test suites are
created with the `describe` form. `describe` forms can be nested.

```scheme
(describe "guile-spec README" ...)
```

Specs
-----
Specs contain expectations of the code under test. Specs are created
with the `it` form within a `describe` form.

```scheme
(describe "guile-spec README"
   (it "provides useful information") ...)
```

Expectations
------------
A spec must have all true expectations to pass. Any predicate can be
used as a matcher.

```scheme
(describe "guile-spec README"
   (it "provides useful information"
     (let ((information "useful"))
       (expect string=? (readme) information))))
```

Stubbing
--------
Stubs are used to better isolate the code under test. The `stub` form
inserts a procedure into a module that returns an arbitrary value,
replacing what was there before. The previous binding is restored
before leaving the form.

Syntax-wise, it resembles a `let` form, but with a module name before
the list of stubbed procedures.

```scheme
(stub (module name)
  ((procedure-name (return value)) ...) ...)
```

The `return` form is optional. `#f` is returned by default.

```scheme
(describe "guile-spec README"
  (let ((val "foo"))
    (stub (guile-spec README)
      ((stubbing-section (return val)))
      (it "has a section about stubbing"
        (expect string=? (stubbing-section) val)))))
```

Trivial Example
---------------
```scheme
(describe "#append"
  (it "concatenates 2 or more lists"
    (let ((a '(1))
          (b '(2))
          (c '(3)))
      (expect equal? (append a b c) '(1 2 3)))))
```

Things that are missing
-----------------------
* mocking
* spying
* good debug information
* error handling
* colored output

Usage
-----
```
$ guile-spec spec/my_spec.scm
```

Note: the result of calling `load` on a spec file *must* be a `<spec-suite>`.

Hacking
-------
There are tons of features missing to make this a real BDD
library. Patches welcome.

Contributors
------------
* davexunit
* mark_weaver

License
-------
LGPL v3

Guile-spec
==========

Guile-spec is an RSpec/Jasmine-like BDD testing library for Guile Scheme.

Test Suites
-----------
Test suites encapsulate test cases, called specs. Test suites are
created with the `describe` form. `describe` forms can be nested.

Specs
-----
Specs contain expectations of the code under test. Specs are created
with the `it` form within a `describe` form.

Expectations
------------
A spec must have all true expectations to pass. Any predicate can be
used as a matcher.

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
mocking, stubbing, spying, good debug information, colored output, etc.

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

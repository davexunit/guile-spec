GSpec
=====

GSpec is an RSpec/Jasmine-like testing framework for Guile Scheme.

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

Usage
-----
```
$ gspec spec/my_spec.scm
```

Hacking
-------
GSpec isn't very feature-rich currently. Patches are very welcome.
Please refer to the TODO org-mode file to see what needs to be done,
or improve/implement something that you feel is missing. :)

Contributors
------------
davexunit
mark_weaver

License
-------
LGPL v3

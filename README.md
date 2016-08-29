[![Build Status](https://travis-ci.org/phile314/tasty-silver.svg?branch=master)](https://travis-ci.org/phile314/tasty-silver)

Tasty Silver
============

This package provides support for «golden testing».

A golden test is an IO action that writes its result to a file.
To pass the test, this output file should be identical to the corresponding
«golden» file, which contains the correct result for the test.

Interactive Mode
----------------

If the test runner is called with the `-i` option, the diff of any failing golden test is shown
to the user. Based upon this diff, the user can choose to update the golden standard or to
fix the test case as necessary. Interactive mode requires that at least `git diff` and `less` is
available, or preferrably `wdiff` and `colordiff` for character-based diffs.

Examples
--------

For a non-trivial example see the [Agda tests](https://github.com/agda/agda/blob/master/test/Compiler/Tests.hs),
which is used for testing the Agda compiler.

Maintainers
-----------

[Philipp Hausmann](https://github.com/phile314) is the primary maintainer.

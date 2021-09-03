[![Hackage version](https://img.shields.io/hackage/v/tasty-silver.svg?label=Hackage)](http://hackage.haskell.org/package/tasty-silver)
[![tasty-silver on Stackage Nightly](https://stackage.org/package/tasty-silver/badge/nightly)](https://stackage.org/nightly/package/tasty-silver)
[![Stackage LTS version](https://www.stackage.org/package/tasty-silver/badge/lts?label=Stackage)](https://www.stackage.org/package/tasty-silver)
[![Haskell-CI](https://github.com/phile314/tasty-silver/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/phile314/tasty-silver/actions/workflows/haskell-ci.yml)
[![macOS](https://github.com/phile314/tasty-silver/actions/workflows/macOS.yml/badge.svg)](https://github.com/phile314/tasty-silver/actions/workflows/macOS.yml)
[![windows](https://github.com/phile314/tasty-silver/actions/workflows/windows.yml/badge.svg)](https://github.com/phile314/tasty-silver/actions/workflows/windows.yml)
[![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/tasty-silver/badge)](https://matrix.hackage.haskell.org/package/tasty-silver)

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
[Andreas Abel](https://github.com/andreasabel) is co-maintainer.

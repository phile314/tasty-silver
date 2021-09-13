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

If `-i` is supplied but no user input can be received (`hIsTerminalDevice stdin` gives `False`),
then answer "yes" is assumed on questions whether to update the golden value.
(So, with `-i < /dev/null` one can update all golden values.)

Portability
-----------

`tasty-silver` aims to work under Linux, macOS, and Windows.  In
particular, it should work in the [GitHub CI virtual
environments](https://github.com/actions/virtual-environments).

Known limitations:

- On `macOS`, GHC ≥ 7.10 is required, as GHC ≤ 7.8 produces code that
  is not compatible with the System Integrity Protection mechanism of
  Mac OS X.  In particular, you could see errors like:
  ```
  /usr/bin/less: getPermissions:fileAccess: permission denied (Operation not permitted)
  ```

- On Windows, the colored diff may not be available as it depends on
  the availability of `colordiff`, `less`, `sh`, and `wdiff`.

Examples
--------

For a non-trivial example see the [Agda tests](https://github.com/agda/agda/blob/master/test/Compiler/Tests.hs),
which is used for testing the Agda compiler.

Maintainers
-----------

[Philipp Hausmann](https://github.com/phile314) is the primary maintainer.
[Andreas Abel](https://github.com/andreasabel) is co-maintainer.

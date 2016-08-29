Changes
=======

Version 3.1.9
-------------

* Fix compilation with optparse-applicative 0.13.*.
* Provide character-level diff if wdiff and colordiff are available.

Version 3.1.8.1
---------------

* Fix compilation with GHC 8.

Version 3.1.8
-------------

* Make update function optional for test cases.

Version 3.1.7
-------------

* Add feature to disable certain tests, still showing them in the UI
  but not running them.
* Fix a concurrency issue in the interactive test runner.

Version 3.1.6
-------------

* Expose regex filter modules.
* Fix issue with regex filters when used together with withResource nodes.

Version 3.1.5
-------------

* Add experimental --regex-include option to select tests using a regex.
  This option is highly experimental and may change in later versions!
* The --regex-include/--regex-exclude option may be given multiple times now.
  The exclusion regexes are applied first, after that all inclusion regexes.

Version 3.1.4
-------------

* Add experimental --regex-exclude option to filter out tests using a regex.
  This option is highly experimental and may change in later versions!

Version 3.1.3
-------------

* Use package temporary instead of temporary-rc.
* Re-add command line options for test runner which were accidentally removed.

Version 3.1.2
-------------

* Add non-interactive mode to test runner, printing diffs/actual values directly to stdout.
  Useful for (travis) CI.

Version 3.1.1
-------------

* Report success instead of failure if new result is accepted in interactive mode.

Version 3.1
-----------

* Fixed & tested support for GHC 7.4.2 - 7.10.1
* Added missing lower bound for bytestring
* Removed upper bounds for most dependencies
* Enable travis CI builds

Version 3.0 - 3.0.2.2
-----------

* Refactored API
* Add interactive mode

Version 2.2.2.4
---------------

* Warn when some tests threw exceptions during `--accept`
* Properly handle exceptions; don't swallow Ctrl-C

Version 2.2.2.3
---------------

Restore compatibility with older compilers

Version 2.2.2.1
---------------

Relax `Cabal` dependency

Version 2.2.2
-------------

Add `findByExtension`

Version 2.2.1.2
---------------

Catch exceptions when accepting golden tests

Version 2.2.1.1
---------------

Switch to `temporary-rc`

Version 2.2.1
-------------

* Fix a bug where the result of the comparison function would reference yet
  unread data from a semiclosed file and the file gets closed, leading to a
  runtime exception
* Export `writeBinaryFile`
* Improve the docs
* Update to work with `tasty-0.8`

Version 2.2.0.2
---------------

Update to work with `tasty-0.7`

Version 2.2.0.1
---------------

Update to work with `tasty-0.5`

Version 2.2
-----------

Migrate to ingredients

Version 2.1
-----------

Add `goldenVsStringDiff`

Version 2.0.1
-------------

Update to work with `tasty-0.2`

Version 2.0
-----------

Initial release of `tasty-golden` (derived from `test-framework-golden-1.1.x`)

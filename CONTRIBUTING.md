# Contributing to Issie


* See the Issie `Info --> Bug Reports` for information about how to report a possible bug in an Issie issue
* There is an automatic test suite: `npm run test` runs 396 tests in about a hundred seconds. Run it before you make a PR, and add to it — see [Tests/README.md](Tests/README.md), which also says how to run a single group in seconds
* CI runs that suite on every push and a failure blocks. It does **not** run the `Issie.VerilogCompiler` group (skipped whenever `CI` is set) or `npm run typecheck`, so run those locally when your change touches what they cover. In your PR, say how much you have tested the change, or why you think it won't break anything
* If you are writing new code take to heart the extensive [Issie coding guidelines](https://github.com/tomcl/issie/wiki/1---Coding-guidelines-for-ISSIE)

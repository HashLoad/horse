# FPC callback compatibility regressions

Read this in [English](./README.md) or [Português (BR)](./README.pt-BR.md).

Run from the Horse root with Python 3, FPC and the matching RTL/FCL units:

```text
python3 tests/fpc-functionreferences/run.py --mode all
```

`all` builds the procedural suites, then builds the closure and exception
suites with `HORSE_FPC_FUNCTIONREFERENCES`. Use `--mode legacy` on FPC 3.2.2,
or `--mode funcref` for the opt-in branch on FPC 3.3.1. The runner rebuilds all
units (`-B`) into a separate temporary directory for each executable; it never
mixes `.ppu` files between modes. Compiler errors, process errors, timeouts and
heaptrc reports of unfreed blocks fail the run.

The closure suite checks both Tree and Radix: captured string/interface lifetime
after a factory returns, middleware before and after `Next`, lifecycle hook
order, core abbreviated callbacks, and separate instances with captured hooks
and abbreviated callbacks. It also checks resuming `Next` after nested route
dispatch, 404/405 finalizers and more than 64 abbreviated callbacks.
Calls are synchronous; the tests do not promise that
`Next` can be retained beyond the request. They do not open network sockets or
validate concurrent traffic.

The existing exception suite keeps its 34 expectations in both modes. The
legacy pass also runs header hashing, URL decoding and router lifetime tests;
those existing suites use procedural callback construction. A successful legacy
run does not establish closure support on that compiler.

For an isolated compiler/RTL installation, use `--compiler PATH` and repeat
`--compiler-arg=OPTION` for its configuration, for example `--compiler-arg=-n`
and a matching `--compiler-arg=-FuPATH_TO_UNITS/*`. Do not mix compiler versions'
units. `FPC` can also select the compiler executable.

For a negative control, keep these tests and supply an unpatched source tree:

```text
python3 tests/fpc-functionreferences/run.py --mode funcref --source-root PATH_TO_BASE/src
```

Against base `a0ac019`, the captured callbacks fail to compile. This is an
expected negative result, not a passing test.

Local validation is Linux x86_64 only. Delphi, FPC 3.2.0, other targets and
HTTP integration require separate validation; these portable regressions are
not a substitute for the full upstream test matrix.

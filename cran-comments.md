## Resubmission

This package was archived on CRAN on 2026-08-22 ("issues were not corrected
in time"). The archival was triggered by a single test failure on the
OpenBLAS-flavor check machine:

```
test-cglmm.R:192:3: Failure ('test-cglmm.R:192:3'): model output is class cglmm
Expected `cglmm(...)` to throw a warning.
```

All 13 regular CRAN check flavors passed; only the OpenBLAS special check
failed. The root cause was a flaky, session-order-dependent test: `lme4`
2.0-6 moved `findbars()`/`nobars()` into a new companion package,
`reformulas`, and calling the old `lme4::findbars()`/`lme4::nobars()`
re-exports emits a one-time-per-session deprecation warning. The failing
test relied on that incidental warning via `expect_warning()` with no
message pattern; because earlier tests in the same file already trigger the
warning once, it was no longer available by the time this test ran on that
particular check machine, causing the assertion to fail non-deterministically
depending on test/BLAS execution order.

This release fixes the root cause by:

* Migrating `GLMMcosinor` to call `reformulas::findbars()`/`nobars()`
  directly instead of the deprecated `lme4` re-exports (and removing the
  now-unused `lme4` `Imports` entry).
* Replacing the fragile `expect_warning()` assertion with `expect_no_error()`,
  since no genuine warning is produced by that model specification and the
  test was never verifying anything else.

Also included in this release:

* Two documentation typo fixes (`Opactiy` -> `Opacity`,
  `sugments` -> `segments`).
* Wrapped four slow examples (~6-9s CPU each, due to n = 10,000 simulated
  data + two-component `glmmTMB` fits) in `\donttest{}`.

## R CMD check results

0 errors | 0 warnings | 0 notes (via `R CMD check --as-cran`, R-devel,
Linux).

The "package was archived" / "new submission" incoming-feasibility note is
expected for a resubmission after archival and is not shown above.

Has been checked using GitHub Actions on:
- {os: macos-latest, r: 'release'}
- {os: windows-latest, r: 'release'}
- {os: ubuntu-latest, r: 'devel', http-user-agent: 'release'}
- {os: ubuntu-latest, r: 'release'}
- {os: ubuntu-latest, r: 'oldrel-1'}

The software has been peer reviewed through rOpenSci
<https://github.com/ropensci/software-review/issues/603>.

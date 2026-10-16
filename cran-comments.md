<!--
Draft. Before submitting, still to run and record here: win-builder (devel and
release), mac-builder. Replace this comment with their results.
-->

## Submission

This is a new submission.

## Test environments

- GitHub Actions, ubuntu-latest: R devel, R release, R oldrel-1
- GitHub Actions, windows-latest: R release
- GitHub Actions, macos-latest: R release, and R release in CRAN mode
  (`NOT_CRAN` unset) with only the hard dependencies installed (no Suggests
  beyond testthat, knitr and rmarkdown)
- Local (Ubuntu 24.04, R 4.6.0): `R CMD check --as-cran` on `radiatR_0.1.3.tar.gz`
  — 0 errors, 0 warnings, 1 NOTE (new submission)
- `urlchecker::url_check()`: all URLs correct
- win-builder (devel + release) and mac-builder: pending

## R CMD check results

0 errors | 0 warnings | 1 note

- This is a new submission.

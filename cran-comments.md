<!--
Draft. Before submitting, still to run and record here: win-builder (devel and
release), mac-builder, R-devel, a check with no Suggests installed, the PDF
manual and urlchecker. Replace this comment with their results.
-->

## Submission

This is a new submission.

## Test environments

- GitHub Actions, ubuntu-latest: R release, R oldrel-1
- GitHub Actions, windows-latest: R release
- GitHub Actions, macos-latest: R release

## R CMD check results

### Installed package size

The installed size is at or just above 5 MB on two of the three platforms:

- Windows: under 5 MB (check OK)
- Ubuntu: 5.0 MB (`R` 1.3 MB, `data` 1.7 MB)
- macOS: 5.9 MB (`R` 2.1 MB, `data` 1.7 MB)

`data` holds one example dataset, `cpunctatus`: the tracks of 235 trials, which
the examples, tests and vignettes use. It is stored with `xz` compression. The
raw files it was built from ship as a single 240 KB archive in `inst/extdata`.
`R` is the package's own code; its installed size differs by platform, the
source being the same.

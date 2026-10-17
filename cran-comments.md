## Submission

This is a new submission.

## Test environments

- GitHub Actions, ubuntu-latest: R devel, R release, R oldrel-1
- GitHub Actions, windows-latest: R release
- GitHub Actions, macos-latest: R release, and R release in CRAN mode
  (`NOT_CRAN` unset) with only the hard dependencies installed (no Suggests
  beyond testthat, knitr and rmarkdown)
- `urlchecker::url_check()`: all URLs correct
- win-builder, R release (4.6.1, Windows Server 2022): 0 errors, 0 warnings,
  1 NOTE (new submission; possibly misspelled words, see below)
- win-builder, R devel (2026-10-16 r90686, Windows Server 2022): 0 errors,
  0 warnings, 1 NOTE (same as R release)
- mac-builder was unavailable when this was submitted (three runs on
  2026-10-17 stopped at the dependency check because none of the CRAN
  dependencies were installed on the builder); macOS is covered by the
  GitHub Actions rows above

## R CMD check results

0 errors | 0 warnings | 1 note

- This is a new submission.
- Possibly misspelled words in DESCRIPTION: Benhamou, Buttarazzi, Fitak,
  Hermans, Johnsen, Landler, Malkemper, Pandolfo, Porzio, Rasson and Ruxton
  are author surnames in the cited references; "von Mises" is the
  distribution; "tortuosity" is a standard term for path winding; "bodypart"
  is the 'DeepLabCut' term for a tracked keypoint.

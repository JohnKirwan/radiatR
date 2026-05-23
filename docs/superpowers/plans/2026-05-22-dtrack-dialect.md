# dtrack Dialect Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Expose a `dtrack_read()` function for reading dtrack files into a `TrajSet`, register a `"dtrack"` dialect for pre-parsed data frames, document the file-format provenance, and update the loaders vignette to reference dtrack and the published paper.

**Architecture:** `dtrack_read()` is a thin wrapper: it reads the tab-separated headerless file itself (bypassing `.read_any()`, which mis-parses `.txt` as CSV), then calls `TrajSet_read()` with the cleaned data frame. A `"dtrack"` dialect is also registered for users who already have the data in a data frame. Four files are touched: `R/loaders.R`, `tests/testthat/test-loaders.R`, `vignettes/loaders.Rmd`, and `data-raw/P_lividus_example.R`.

**Tech Stack:** R, testthat (edition 3), Roxygen2, rmarkdown

---

## File map

| File | Change |
|------|--------|
| `R/loaders.R` | Add `dtrack_read()` exported function; `register_loader_dialect("dtrack", ...)`; `@details` in `import_tracks()` Roxygen |
| `tests/testthat/test-loaders.R` | Append tests for `dtrack_read()` and the `"dtrack"` dialect |
| `vignettes/loaders.Rmd` | Add "dtrack format" section; fix `format=` → `dialect=` in the json_tracks chunk |
| `data-raw/P_lividus_example.R` | Replace header comment with full provenance block |

---

## Task 1: Tests (write first, they must fail)

**Files:**
- Modify: `tests/testthat/test-loaders.R` (append)

- [ ] **Step 1: Append tests**

Add at the end of `tests/testthat/test-loaders.R`:

```r
test_that("dtrack_read() reads a headerless tab-sep file into a TrajSet", {
  tmp <- withr::local_tempfile(fileext = ".txt")
  writeLines(
    c("1\t100.5\t200.3\t1", "2\t101.0\t201.1\t1", "3\t102.2\t199.8\t1"),
    tmp
  )
  ts <- dtrack_read(tmp)
  expect_s4_class(ts, "TrajSet")
  expect_equal(nrow(ts@data), 3L)
  expect_true(all(c("x", "y") %in% names(ts@data)))
  expect_false("V4" %in% names(ts@data))
})

test_that("dtrack_read() errors on a file with fewer than 3 columns", {
  tmp <- withr::local_tempfile(fileext = ".txt")
  writeLines(c("1\t100.5", "2\t101.0"), tmp)
  expect_error(dtrack_read(tmp), "at least 3 columns")
})

test_that("dtrack dialect processes a pre-parsed data frame", {
  df <- data.frame(V1 = 1:3, V2 = c(10.1, 10.2, 10.3), V3 = c(20.1, 20.2, 20.3),
                   stringsAsFactors = FALSE)
  result <- get("dtrack", envir = radiatR:::.loader_registry)(df)
  expect_equal(names(result), c("frame", "x", "y", "id"))
  expect_equal(nrow(result), 3L)
  expect_equal(result$id[1], "1")
})
```

- [ ] **Step 2: Run tests — confirm all three fail**

```r
testthat::test_file("tests/testthat/test-loaders.R")
```

Expected: FAIL with "could not find function 'dtrack_read'" and "object '.loader_registry' not found".

---

## Task 2: Implement `dtrack_read()` and the `"dtrack"` dialect

**Files:**
- Modify: `R/loaders.R`

- [ ] **Step 1: Add `dtrack_read()` after the `import_tracks()` block (around line 95)**

Insert after the closing brace of `import_tracks()` and before `escape_specials`:

```r
#' Read a dtrack trajectory file into a TrajSet
#'
#' Reads a tab-separated, headerless file produced by dtrack
#' (\url{https://bitbucket.org/jochensmolka/dtrack}). The file is expected to
#' have at least three columns: frame number, x coordinate, y coordinate. A
#' fourth confidence/flag column (always \code{1} in practice) is silently
#' dropped.
#'
#' @param path Path to a dtrack \code{_point02.txt} trajectory file.
#' @param normalize_xy Logical; passed to [TrajSet_read()]. Default \code{FALSE}
#'   because dtrack files are in pixel space.
#' @param ... Additional arguments passed to [TrajSet_read()].
#' @return A \code{TrajSet}.
#' @seealso [import_tracks()] for discovering dtrack file pairs in a directory.
#' @export
dtrack_read <- function(path, normalize_xy = FALSE, ...) {
  stopifnot(is.character(path), length(path) == 1L, file.exists(path))
  df <- utils::read.delim(path, sep = "\t", header = FALSE, stringsAsFactors = FALSE)
  if (ncol(df) < 3L) stop("dtrack: expected at least 3 columns (frame, x, y)")
  df <- df[, 1:3, drop = FALSE]
  names(df) <- c("frame", "x", "y")
  df$id <- tools::file_path_sans_ext(basename(path))
  TrajSet_read(
    df,
    mapping = list(id = "id", time = "frame", x = "x", y = "y"),
    normalize_xy = normalize_xy,
    ...
  )
}
```

- [ ] **Step 2: Add `"dtrack"` dialect at the bottom of the dialect block (after `boris_xy`, before `# ---- built-in dialects: general`)**

```r
# dtrack (https://bitbucket.org/jochensmolka/dtrack)
# Expects a data frame with >=3 columns: frame, x, y[, confidence].
# The confidence column is dropped. Use dtrack_read() to read from a file path.
register_loader_dialect("dtrack", function(x) {
  df <- if (is.character(x) && file.exists(x)) {
    utils::read.delim(x, sep = "\t", header = FALSE, stringsAsFactors = FALSE)
  } else {
    x
  }
  stopifnot(is.data.frame(df))
  if (ncol(df) < 3L) stop("dtrack: expected at least 3 columns (frame, x, y)")
  df <- df[, 1:3, drop = FALSE]
  names(df) <- c("frame", "x", "y")
  df$id <- "1"
  df
})
```

- [ ] **Step 3: Run tests — confirm all three pass**

```r
testthat::test_file("tests/testthat/test-loaders.R")
```

Expected: all tests PASS.

- [ ] **Step 4: Run full test suite**

```r
devtools::test()
```

Expected: no new failures.

- [ ] **Step 5: Commit**

```bash
git checkout -b dtrack-dialect
git add R/loaders.R tests/testthat/test-loaders.R
git commit -m "feat: add dtrack_read() function and dtrack loader dialect"
```

---

## Task 3: Roxygen update for `import_tracks()`

**Files:**
- Modify: `R/loaders.R` lines 59–68

- [ ] **Step 1: Replace the `import_tracks()` Roxygen block**

Find:

```r
#' Import landmark coordinates from text files
#'
#' @param dir The directory in which to look for the landmark files. Defaults to the current working directory.
#' @param landmark_suffix The suffix of files containing landmark coordinates.
#' @param track_suffix The suffix of files containing track coordinates.
#' @return A dataframe of file names.
#' #examples
#' #import_tracks(dir=data)
#' @export
#' @importFrom tibble tibble
```

Replace with:

```r
#' Discover dtrack (or compatible) landmark/track file pairs in a directory
#'
#' Scans \code{dir} for paired files matching \code{landmark_suffix} and
#' \code{track_suffix} and returns a tibble of basenames and paths.
#'
#' @param dir Directory to scan. Defaults to the current working directory.
#' @param landmark_suffix Suffix identifying landmark files. Default
#'   \code{"_point01.txt"}.
#' @param track_suffix Suffix identifying trajectory files. Default
#'   \code{"_point02.txt"}.
#' @return A tibble with columns \code{basename}, \code{landmark}, and
#'   \code{track}.
#' @details
#'   The default suffixes match the export naming convention used by dtrack
#'   (\url{https://bitbucket.org/jochensmolka/dtrack}). In the bundled
#'   \emph{P. lividus} example data, \code{_point01} files contain two
#'   landmark rows per trial (arena centre and stimulus edge on the arena wall)
#'   and \code{_point02} files contain the per-frame animal trajectory. This
#'   two-file role split is specific to that experiment and is not a general
#'   dtrack convention. Use [dtrack_read()] to read an individual trajectory
#'   file.
#' @export
#' @importFrom tibble tibble
```

- [ ] **Step 2: Rebuild documentation**

```r
devtools::document()
```

Expected: `man/import_tracks.Rd` and `man/dtrack_read.Rd` created/updated, no warnings.

- [ ] **Step 3: Commit**

```bash
git add R/loaders.R man/import_tracks.Rd man/dtrack_read.Rd NAMESPACE
git commit -m "docs: add dtrack provenance note to import_tracks(); document dtrack_read()"
```

---

## Task 4: Loaders vignette update

**Files:**
- Modify: `vignettes/loaders.Rmd`

- [ ] **Step 1: Fix the `json_tracks` chunk — `format=` → `dialect=`**

Find:

```r
ts_json <- TrajSet_read(fake_json, format = "json_tracks")
```

Replace with:

```r
ts_json <- TrajSet_read(fake_json, dialect = "json_tracks")
```

(`format=` routes to the declarative format registry; `dialect=` routes to the function registry where `register_loader_dialect` stores entries.)

- [ ] **Step 2: Add "dtrack format" section**

After the closing ` ``` ` of the `get-all` chunk (the `ts <- get_all_object_pos(...)` block) and before `## Reading Tabular Trajectories`, insert:

```markdown
## dtrack format

The bundled example data was tracked with **dtrack**
(<https://bitbucket.org/jochensmolka/dtrack>), a desktop tracking tool for
video recordings of freely-moving animals. The data are from:

> Kirwan J.D., Li T., Ullrich-Lüter J., La Camera G., Nilsson D.-E.,
> Arnone M.I. (2024). *The sea urchin Paracentrotus lividus orients to visual
> stimuli.* bioRxiv. <https://doi.org/10.1101/2024.01.05.574409>

dtrack exports tab-separated text files with columns `frame`, `x`, `y`, and a
fourth confidence/flag column. Use `dtrack_read()` to load a single trajectory
file directly:

```{r dtrack-single}
track_file <- system.file(
  "extdata", "tracks", "G1D_0_obstacle",
  "WIN_20210201_11_24_19_Pro_point02.txt",
  package = "radiatR"
)
ts_raw <- dtrack_read(track_file)
head(ts_raw@data[, c("id", "frame", "x", "y")])
```

The `_point01` / `_point02` role split used in the bundled data — landmarks
in one file, trajectory in the other — is specific to this experiment and is
not a general dtrack convention.
```

- [ ] **Step 3: Verify the new chunk runs**

```r
pkgload::load_all(".", export_all = FALSE, quiet = TRUE)
track_file <- system.file(
  "extdata", "tracks", "G1D_0_obstacle",
  "WIN_20210201_11_24_19_Pro_point02.txt",
  package = "radiatR"
)
ts_raw <- dtrack_read(track_file)
head(ts_raw@data[, c("id", "frame", "x", "y")])
```

Expected: 6-row data frame with `id` = filename stem, numeric `frame`, `x`, `y`.

- [ ] **Step 4: Commit**

```bash
git add vignettes/loaders.Rmd
git commit -m "docs: add dtrack format section to loaders vignette; fix dialect= usage"
```

---

## Task 5: data-raw provenance comment

**Files:**
- Modify: `data-raw/P_lividus_example.R`

- [ ] **Step 1: Replace lines 1–9 (the header comment)**

Find and replace the entire comment block at the top:

```r
# Source: JohnKirwan/P_lividus_vision (GitHub)
# Five baseline trials (arc=0, marbles obstacle) from Paracentrotus lividus
# adult visual acuity experiment. Tracks are in pixel space from a webcam
# mounted above a circular arena (~2 m diameter).
#
# To refresh or expand the extract, run this script from the package root.
# It downloads from GitHub and writes into inst/extdata/tracks/.
#
# Full dataset: https://github.com/JohnKirwan/P_lividus_vision/tree/main/data/tracks
```

Replace with:

```r
# Data provenance
#
# Paper: Kirwan J.D., Li T., Ullrich-Lüter J., La Camera G., Nilsson D.-E.,
#        Arnone M.I. (2024). The sea urchin Paracentrotus lividus orients to
#        visual stimuli. bioRxiv. https://doi.org/10.1101/2024.01.05.574409
#
# Experiment: 50 P. lividus adults (Bay of Naples) placed individually at the
#   centre of a submerged cylindrical arena (~2 m diameter) and allowed to walk
#   freely toward a patterned wall stimulus. Five stimulus arc widths (0°, 15°,
#   30°, 45°, 60°, 150°) plus a uniform control were tested under diffuse,
#   downwelling light. Tracks recorded in pixel space from a webcam mounted
#   overhead.
#
# Tracking software: dtrack (Smolka J., Bitbucket)
#   https://bitbucket.org/jochensmolka/dtrack
#   Files: _point01.txt = 2 landmark rows per trial (arena centre + stimulus
#          edge on wall); _point02.txt = per-frame xy trajectory of the animal.
#   This two-file role split is specific to this experiment.
#
# Full dataset: https://github.com/JohnKirwan/P_lividus_vision
#
# To refresh or expand the extract, run this script from the package root.
# It downloads from GitHub and writes into inst/extdata/tracks/.
```

- [ ] **Step 2: Check the script parses cleanly**

```r
parse(file = "data-raw/P_lividus_example.R")
```

Expected: no parse errors.

- [ ] **Step 3: Commit**

```bash
git add data-raw/P_lividus_example.R
git commit -m "docs: add full provenance comment to P_lividus data-raw script"
```

---

## Task 6: Final checks and PR

- [ ] **Step 1: Run full test suite**

```r
devtools::test()
```

Expected: all tests pass.

- [ ] **Step 2: Run R CMD CHECK**

```r
devtools::check()
```

Expected: 0 errors, 0 warnings.

- [ ] **Step 3: Open PR**

```bash
git push -u origin dtrack-dialect
gh pr create \
  --title "feat: dtrack_read() function, dialect, and data provenance docs" \
  --body "$(cat <<'EOF'
## Summary
- Adds `dtrack_read(path)` exported function for reading dtrack tab-separated trajectory files into a TrajSet; bypasses the generic `.read_any()` loader which mis-parses headerless `.txt` files
- Registers a `"dtrack"` loader dialect for pre-parsed data frames
- Adds `@details` note to `import_tracks()` documenting the dtrack file-naming convention
- Updates loaders vignette with dtrack section citing Kirwan et al. (2024); fixes `format=` → `dialect=` bug in json_tracks example
- Updates `data-raw/P_lividus_example.R` with full provenance (paper DOI, tracking software, experimental description)

## Test plan
- [ ] `devtools::test()` passes with new dtrack_read and dialect tests
- [ ] `devtools::check()` shows 0 errors, 0 warnings
- [ ] Vignette `dtrack-single` chunk runs without error
EOF
)"
```

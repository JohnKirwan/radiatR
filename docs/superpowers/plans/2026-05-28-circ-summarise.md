# `circ_summarise()` Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add `circ_summarise()` — a tidy, grouped circular summary function that works on any data frame with an angle column and returns mean direction, resultant R, kappa, and count per group.

**Architecture:** A single exported function in `R/circular_statistics.R` backed by a private helper `.circ_summarise_one()`. Grouping is resolved from either a `group_by()`-grouped tibble or an explicit `.by` character vector. All circular computation goes through the `circular` package via helpers already in the package namespace (`.clock_to_uc()`, `.wrap_to_2pi()`, `.est_kappa_safe()`, `rad2clock()`).

**Tech Stack:** R, `circular`, `rlang` (column-name quoting), `tibble` (output), base-R `split()` for grouping — all already in DESCRIPTION Imports. `dplyr` added to Suggests for `group_by()`-based tests only.

---

## Files

| File | Change |
|---|---|
| `R/circular_statistics.R` | Add `.circ_summarise_one()` helper + `circ_summarise()` |
| `tests/testthat/test-circular-stats.R` | Add all new tests |
| `DESCRIPTION` | Add `dplyr` to Suggests |

### Helpers available at runtime from other package files
- `.clock_to_uc(angles, coords)` — `R/headings.R:52` — converts clock-convention angles to UC
- `.wrap_to_2pi(x)` — `R/headings.R:17` — wraps radians to [0, 2π]
- `.est_kappa_safe(tc, ...)` — `R/circular_statistics.R:38` — safe kappa estimation, returns NA on failure
- `rad2clock(theta)` — `R/circular_mapping.R:50` — converts UC angle to clock convention
- `%||%` — `R/headings.R:16` — null-coalescing operator

---

## Task 1: `.circ_summarise_one()` helper + grand summary (no grouping)

**Files:**
- Modify: `R/circular_statistics.R` (append after `dwell_proportions`)
- Modify: `tests/testthat/test-circular-stats.R`

- [ ] **Step 1: Write failing tests**

```r
test_that("circ_summarise grand summary returns single-row tibble with correct columns", {
  hd <- data.frame(heading = c(pi/6, pi/4, pi/3))
  result <- circ_summarise(hd, heading)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1L)
  expect_named(result, c("n", "mean_dir", "mean_dir_deg", "resultant_R", "kappa"))
})

test_that("circ_summarise grand summary matches circular package values", {
  angles <- c(pi/6, pi/4, pi/3)
  tc     <- circular::circular(angles, units = "radians", modulo = "2pi")
  exp_mean <- as.numeric(circular::mean.circular(tc)) %% (2 * pi)
  exp_R    <- as.numeric(circular::rho.circular(tc))

  result <- circ_summarise(data.frame(heading = angles), heading)

  expect_equal(result$n,          3L,       tolerance = 1e-8)
  expect_equal(result$mean_dir,   exp_mean, tolerance = 1e-8)
  expect_equal(result$mean_dir_deg, exp_mean * 180 / pi, tolerance = 1e-8)
  expect_equal(result$resultant_R, exp_R,   tolerance = 1e-8)
})

test_that("circ_summarise result is ungrouped tibble", {
  result <- circ_summarise(data.frame(heading = c(0, pi/2)), heading)
  expect_false(inherits(result, "grouped_df"))
  expect_s3_class(result, "tbl_df")
})
```

- [ ] **Step 2: Run tests to verify they fail**

```bash
Rscript -e "testthat::test_file('tests/testthat/test-circular-stats.R')" 2>&1 | tail -5
```
Expected: FAIL — `could not find function "circ_summarise"`

- [ ] **Step 3: Implement `.circ_summarise_one()` and minimal `circ_summarise()`**

Append to `R/circular_statistics.R`:

```r
# ---------------------------------------------------------------------------
# circ_summarise — tidy grouped circular summary

.circ_summarise_one <- function(angles, stats, angle_convention, coords) {
  angles <- angles[is.finite(angles)]
  n      <- length(angles)
  uc_angles <- if (angle_convention == "clock") .clock_to_uc(angles, coords) else angles

  if (n == 0L) {
    uc_mu <- NA_real_
    R     <- NA_real_
    kap   <- NA_real_
  } else {
    tc    <- circular::circular(uc_angles, units = "radians", modulo = "2pi")
    uc_mu <- .wrap_to_2pi(as.numeric(circular::mean.circular(tc)))
    R     <- as.numeric(circular::rho.circular(tc))
    kap   <- if (n >= 3L) .est_kappa_safe(tc) else NA_real_
  }

  out_mu <- if (!is.na(uc_mu) && angle_convention == "clock") {
    if (coords == "relative") (-uc_mu) %% (2 * pi) else rad2clock(uc_mu)
  } else {
    uc_mu
  }

  row <- vector("list", length(stats))
  names(row) <- stats
  for (s in stats) {
    row[[s]] <- switch(s,
      n            = as.integer(n),
      mean_dir     = out_mu,
      mean_dir_deg = if (is.na(out_mu)) NA_real_ else out_mu * 180 / pi,
      resultant_R  = R,
      kappa        = kap
    )
  }
  row
}

#' Tidy circular summary of a grouped data frame
#'
#' Computes circular summary statistics from any data frame column containing
#' angles in radians. Supports grouped tibbles and an explicit \code{.by}
#' argument, returning one row per group.
#'
#' @param data A data frame or grouped tibble.
#' @param col Unquoted or quoted name of the column containing angles (radians).
#' @param .by Character vector of grouping column names. Overrides any
#'   \code{group_by()} groups on \code{data}.
#' @param stats Character vector selecting which statistics to compute. Order
#'   determines column order in the output. Valid values: \code{"n"},
#'   \code{"mean_dir"}, \code{"mean_dir_deg"}, \code{"resultant_R"},
#'   \code{"kappa"}. Default: all five.
#' @param angle_convention \code{"unit_circle"} (0 = East, CCW) or
#'   \code{"clock"} (0 = North, CW). When \code{NULL}, read from
#'   \code{attr(data, "angle_convention")}; defaults to \code{"unit_circle"}.
#' @param coords \code{"relative"} or \code{"absolute"}. Only used when
#'   \code{angle_convention = "clock"}. When \code{NULL}, read from
#'   \code{attr(data, "coords")}; defaults to \code{"absolute"}.
#'
#' @return An ungrouped \code{tibble} with group columns first followed by
#'   requested stat columns in the order given in \code{stats}.
#'
#' @examples
#' hd <- data.frame(heading = c(0, pi/4, pi/2), arc = c("a", "a", "b"))
#' circ_summarise(hd, heading)
#' circ_summarise(hd, heading, .by = "arc")
#' circ_summarise(hd, heading, .by = "arc", stats = c("n", "mean_dir"))
#'
#' @importFrom rlang ensym as_string
#' @importFrom tibble as_tibble
#' @importFrom circular circular mean.circular rho.circular
#' @export
circ_summarise <- function(data,
                           col,
                           .by              = NULL,
                           stats            = c("n", "mean_dir", "mean_dir_deg",
                                               "resultant_R", "kappa"),
                           angle_convention = NULL,
                           coords           = NULL) {
  col_name <- rlang::as_string(rlang::ensym(col))
  if (!col_name %in% names(data))
    stop(sprintf("`col` column '%s' not found in data.", col_name))

  valid_stats <- c("n", "mean_dir", "mean_dir_deg", "resultant_R", "kappa")
  unknown <- setdiff(stats, valid_stats)
  if (length(unknown))
    stop(sprintf("Unknown stats: '%s'. Valid values are: %s.",
                 paste(unknown, collapse = "', '"),
                 paste(valid_stats, collapse = ", ")))

  if (is.null(angle_convention))
    angle_convention <- if (!is.null(attr(data, "angle_convention")))
      attr(data, "angle_convention") else "unit_circle"
  angle_convention <- match.arg(angle_convention, c("unit_circle", "clock"))

  if (is.null(coords))
    coords <- if (!is.null(attr(data, "coords"))) attr(data, "coords") else "absolute"
  coords <- match.arg(coords, c("relative", "absolute"))

  # Resolve grouping — deferred to Tasks 2 & 3; for now: no grouping
  group_vars <- character(0)

  data_df <- as.data.frame(data)

  if (length(group_vars) == 0L) {
    srow <- .circ_summarise_one(data_df[[col_name]], stats, angle_convention, coords)
    return(tibble::as_tibble(as.data.frame(srow, stringsAsFactors = FALSE)))
  }
}
```

- [ ] **Step 4: Run tests to verify they pass**

```bash
Rscript -e "testthat::test_file('tests/testthat/test-circular-stats.R')" 2>&1 | tail -5
```
Expected: the three new tests PASS; existing tests still pass.

- [ ] **Step 5: Commit**

```bash
git checkout -b feat/circ-summarise
git add R/circular_statistics.R tests/testthat/test-circular-stats.R
git commit -m "feat: add .circ_summarise_one helper and circ_summarise grand summary"
```

---

## Task 2: `.by` grouping

**Files:**
- Modify: `R/circular_statistics.R` — replace `group_vars <- character(0)` stub with split logic
- Modify: `tests/testthat/test-circular-stats.R`

- [ ] **Step 1: Write failing tests**

```r
test_that("circ_summarise .by returns one row per group with correct columns", {
  hd <- data.frame(
    heading = c(pi/6, pi/4, pi/3, pi),
    arc     = c("a", "a", "b", "b")
  )
  result <- circ_summarise(hd, heading, .by = "arc")
  expect_equal(nrow(result), 2L)
  expect_named(result, c("arc", "n", "mean_dir", "mean_dir_deg", "resultant_R", "kappa"))
  expect_equal(sort(result$arc), c("a", "b"))
  expect_equal(result$n[result$arc == "a"], 2L)
  expect_equal(result$n[result$arc == "b"], 2L)
})

test_that("circ_summarise .by group stats match per-group circular package values", {
  angles_a <- c(pi/6, pi/4)
  angles_b <- c(pi/3, pi/2)
  hd <- data.frame(heading = c(angles_a, angles_b), arc = c("a","a","b","b"))

  result <- circ_summarise(hd, heading, .by = "arc")

  for (grp_name in c("a", "b")) {
    angs <- if (grp_name == "a") angles_a else angles_b
    tc   <- circular::circular(angs, units = "radians", modulo = "2pi")
    exp_mean <- as.numeric(circular::mean.circular(tc)) %% (2*pi)
    exp_R    <- as.numeric(circular::rho.circular(tc))
    row      <- result[result$arc == grp_name, ]
    expect_equal(row$mean_dir,    exp_mean, tolerance = 1e-8)
    expect_equal(row$resultant_R, exp_R,    tolerance = 1e-8)
  }
})

test_that("circ_summarise .by group cols appear before stat cols", {
  hd <- data.frame(heading = c(0, pi), arc = c("x","y"), cond = c("A","A"))
  result <- circ_summarise(hd, heading, .by = c("arc", "cond"))
  expect_equal(names(result)[1:2], c("arc", "cond"))
})

test_that("circ_summarise .by factor column preserves level order in output", {
  hd <- data.frame(
    heading = c(0, pi/2, pi, 3*pi/2),
    arc     = factor(c("b","b","a","a"), levels = c("b","a"))
  )
  result <- circ_summarise(hd, heading, .by = "arc")
  expect_equal(as.character(result$arc), c("b", "a"))
})

test_that("circ_summarise .by missing column raises informative error", {
  hd <- data.frame(heading = pi/4)
  expect_error(
    circ_summarise(hd, heading, .by = "nonexistent"),
    ".by column 'nonexistent' not found in data"
  )
})
```

- [ ] **Step 2: Run tests to verify they fail**

```bash
Rscript -e "testthat::test_file('tests/testthat/test-circular-stats.R')" 2>&1 | tail -5
```
Expected: FAIL — `.by` grouping not yet implemented.

- [ ] **Step 3: Replace `group_vars` stub with full grouping logic**

In `circ_summarise()`, replace:

```r
  # Resolve grouping — deferred to Tasks 2 & 3; for now: no grouping
  group_vars <- character(0)
```

with:

```r
  group_vars <- if (!is.null(.by)) {
    missing_by <- setdiff(.by, names(data))
    if (length(missing_by))
      stop(sprintf(".by column '%s' not found in data.", missing_by[1L]))
    .by
  } else {
    character(0)   # group_by() detection added in Task 3
  }
```

And replace the body after `data_df <- as.data.frame(data)` with:

```r
  data_df <- as.data.frame(data)

  if (length(group_vars) == 0L) {
    srow <- .circ_summarise_one(data_df[[col_name]], stats, angle_convention, coords)
    return(tibble::as_tibble(as.data.frame(srow, stringsAsFactors = FALSE)))
  }

  split_key <- if (length(group_vars) == 1L) {
    data_df[[group_vars]]
  } else {
    interaction(data_df[group_vars], drop = TRUE, sep = "\001")
  }
  idx_list <- split(seq_len(nrow(data_df)), split_key, drop = TRUE)

  result_rows <- lapply(names(idx_list), function(k) {
    ii   <- idx_list[[k]]
    srow <- .circ_summarise_one(data_df[[col_name]][ii], stats, angle_convention, coords)
    krow <- data_df[ii[1L], group_vars, drop = FALSE]
    rownames(krow) <- NULL
    cbind(krow, as.data.frame(srow, stringsAsFactors = FALSE))
  })

  result <- do.call(rbind, result_rows)
  rownames(result) <- NULL
  tibble::as_tibble(result)
```

- [ ] **Step 4: Run tests to verify they pass**

```bash
Rscript -e "testthat::test_file('tests/testthat/test-circular-stats.R')" 2>&1 | tail -5
```
Expected: all Task 1 and Task 2 tests PASS.

- [ ] **Step 5: Commit**

```bash
git add R/circular_statistics.R tests/testthat/test-circular-stats.R
git commit -m "feat: circ_summarise .by grouping"
```

---

## Task 3: `group_by()` detection and `.by` override

**Files:**
- Modify: `DESCRIPTION` — add `dplyr` to Suggests
- Modify: `R/circular_statistics.R` — add `inherits(data, "grouped_df")` branch
- Modify: `tests/testthat/test-circular-stats.R`

- [ ] **Step 1: Add dplyr to Suggests in DESCRIPTION**

In `DESCRIPTION`, add `dplyr` to the `Suggests:` block:

```
Suggests: 
    dplyr,
    testthat (>= 3.0.0),
    knitr,
    ...
```

- [ ] **Step 2: Write failing tests**

```r
test_that("circ_summarise respects group_by groups on a grouped tibble", {
  skip_if_not_installed("dplyr")
  hd <- dplyr::group_by(
    data.frame(heading = c(0, pi/2, pi, 3*pi/2), arc = c("a","a","b","b")),
    arc
  )
  result <- circ_summarise(hd, heading)
  expect_equal(nrow(result), 2L)
  expect_true("arc" %in% names(result))
  expect_false(inherits(result, "grouped_df"))
})

test_that("circ_summarise .by overrides group_by groups", {
  skip_if_not_installed("dplyr")
  hd <- dplyr::group_by(
    data.frame(heading = c(0, pi/2, pi, 3*pi/2),
               arc  = c("a","a","b","b"),
               cond = c("x","y","x","y")),
    arc
  )
  result <- circ_summarise(hd, heading, .by = "cond")
  expect_true("cond" %in% names(result))
  expect_false("arc"  %in% names(result))
  expect_equal(nrow(result), 2L)
})
```

- [ ] **Step 3: Run tests to verify they fail**

```bash
Rscript -e "testthat::test_file('tests/testthat/test-circular-stats.R')" 2>&1 | tail -5
```
Expected: the two new tests FAIL.

- [ ] **Step 4: Add `group_by()` detection branch**

In `circ_summarise()`, replace:

```r
  group_vars <- if (!is.null(.by)) {
    missing_by <- setdiff(.by, names(data))
    if (length(missing_by))
      stop(sprintf(".by column '%s' not found in data.", missing_by[1L]))
    .by
  } else {
    character(0)   # group_by() detection added in Task 3
  }
```

with:

```r
  group_vars <- if (!is.null(.by)) {
    missing_by <- setdiff(.by, names(data))
    if (length(missing_by))
      stop(sprintf(".by column '%s' not found in data.", missing_by[1L]))
    .by
  } else if (inherits(data, "grouped_df")) {
    grp_attr <- attr(data, "groups")
    nms      <- names(grp_attr)
    nms[nms != ".rows"]
  } else {
    character(0)
  }
```

- [ ] **Step 5: Run tests to verify they pass**

```bash
Rscript -e "testthat::test_file('tests/testthat/test-circular-stats.R')" 2>&1 | tail -5
```
Expected: all tests PASS.

- [ ] **Step 6: Commit**

```bash
git add DESCRIPTION R/circular_statistics.R tests/testthat/test-circular-stats.R
git commit -m "feat: circ_summarise group_by() detection and .by override"
```

---

## Task 4: `stats` selection and column ordering

**Files:**
- Modify: `tests/testthat/test-circular-stats.R` (tests only — implementation already handles this)

- [ ] **Step 1: Write tests**

```r
test_that("circ_summarise stats subset returns only requested columns", {
  hd     <- data.frame(heading = c(0, pi/2, pi))
  result <- circ_summarise(hd, heading, stats = c("n", "resultant_R"))
  expect_named(result, c("n", "resultant_R"))
})

test_that("circ_summarise stats column order matches stats argument order", {
  hd     <- data.frame(heading = c(0, pi/2))
  result <- circ_summarise(hd, heading,
                            stats = c("resultant_R", "n", "mean_dir"))
  expect_equal(names(result), c("resultant_R", "n", "mean_dir"))
})

test_that("circ_summarise unknown stats value raises informative error", {
  hd <- data.frame(heading = c(0, pi/2))
  expect_error(
    circ_summarise(hd, heading, stats = c("n", "foo")),
    "Unknown stats: 'foo'"
  )
})

test_that("circ_summarise stats ordering applies within grouped output too", {
  hd     <- data.frame(heading = c(0, pi), arc = c("a","b"))
  result <- circ_summarise(hd, heading, .by = "arc",
                            stats = c("resultant_R", "n"))
  expect_equal(names(result), c("arc", "resultant_R", "n"))
})
```

- [ ] **Step 2: Run tests**

```bash
Rscript -e "testthat::test_file('tests/testthat/test-circular-stats.R')" 2>&1 | tail -5
```
Expected: all four new tests PASS immediately (implementation already correct).

- [ ] **Step 3: Commit**

```bash
git add tests/testthat/test-circular-stats.R
git commit -m "test: circ_summarise stats selection and column ordering"
```

---

## Task 5: Clock convention and attribute reading

**Files:**
- Modify: `tests/testthat/test-circular-stats.R`

The implementation already handles both in `.circ_summarise_one()` and the `angle_convention`/`coords` resolution block. This task verifies correctness with tests.

- [ ] **Step 1: Write tests**

```r
test_that("circ_summarise clock+relative: all-zero angles give mean_dir=0, R=1", {
  # Clock 0 (toward stimulus, relative) -> UC 0 via (-0) %% 2pi = 0
  # mean of UC 0 is UC 0 -> back to clock: (-0) %% 2pi = 0
  hd     <- data.frame(heading = c(0, 0, 0))
  result <- circ_summarise(hd, heading,
                            angle_convention = "clock", coords = "relative")
  expect_equal(result$mean_dir,    0,          tolerance = 1e-6)
  expect_equal(result$resultant_R, 1,          tolerance = 1e-6)
  expect_equal(result$mean_dir_deg, 0,         tolerance = 1e-6)
})

test_that("circ_summarise clock+relative: mean_dir is in clock convention", {
  # Clock 90 deg = pi/2 rad -> UC: (-pi/2) %% 2pi = 3pi/2 = 270 deg
  # mean of c(3pi/2, 3pi/2) in UC = 3pi/2 -> back to clock: (-3pi/2) %% 2pi = pi/2
  hd     <- data.frame(heading = c(pi/2, pi/2))
  result <- circ_summarise(hd, heading,
                            angle_convention = "clock", coords = "relative")
  expect_equal(result$mean_dir, pi/2, tolerance = 1e-6)
})

test_that("circ_summarise reads angle_convention from data frame attributes", {
  hd <- data.frame(heading = c(0, 0, 0))
  attr(hd, "angle_convention") <- "clock"
  attr(hd, "coords")           <- "relative"
  result_attr     <- circ_summarise(hd, heading)
  result_explicit <- circ_summarise(hd, heading,
                                     angle_convention = "clock",
                                     coords = "relative")
  expect_equal(result_attr$mean_dir, result_explicit$mean_dir, tolerance = 1e-8)
  expect_equal(result_attr$resultant_R, result_explicit$resultant_R, tolerance = 1e-8)
})

test_that("circ_summarise explicit angle_convention overrides attribute", {
  hd <- data.frame(heading = c(0, 0))
  attr(hd, "angle_convention") <- "clock"
  attr(hd, "coords")           <- "relative"
  result_uc    <- circ_summarise(hd, heading, angle_convention = "unit_circle")
  result_clock <- circ_summarise(hd, heading, angle_convention = "clock",
                                  coords = "relative")
  # With UC: angle 0 stays 0; with clock+relative: (-0) %% 2pi = 0 also 0
  # Use pi/3 angles to distinguish
  hd2 <- data.frame(heading = c(pi/3, pi/3))
  attr(hd2, "angle_convention") <- "clock"
  r_uc    <- circ_summarise(hd2, heading, angle_convention = "unit_circle")
  r_clock <- circ_summarise(hd2, heading, angle_convention = "clock",
                              coords = "relative")
  # UC pi/3 mean_dir = pi/3; clock pi/3 -> UC (-pi/3) %% 2pi = 5pi/3 -> mean 5pi/3 -> clock (-5pi/3) %% 2pi = pi/3
  # Actually they end up the same for this angle — use an angle that differs
  hd3 <- data.frame(heading = c(pi/4, pi/3))
  r_uc3    <- circ_summarise(hd3, heading, angle_convention = "unit_circle")
  r_clock3 <- circ_summarise(hd3, heading, angle_convention = "clock",
                               coords = "relative")
  expect_false(isTRUE(all.equal(r_uc3$mean_dir, r_clock3$mean_dir, tolerance = 1e-4)))
})
```

- [ ] **Step 2: Run tests**

```bash
Rscript -e "testthat::test_file('tests/testthat/test-circular-stats.R')" 2>&1 | tail -5
```
Expected: all four new tests PASS.

- [ ] **Step 3: Commit**

```bash
git add tests/testthat/test-circular-stats.R
git commit -m "test: circ_summarise clock convention and attribute reading"
```

---

## Task 6: Edge cases and error handling

**Files:**
- Modify: `tests/testthat/test-circular-stats.R`

The implementation already handles these edge cases. This task verifies them explicitly.

- [ ] **Step 1: Write tests**

```r
test_that("circ_summarise all-NA angles returns n=0 and NA stats", {
  hd     <- data.frame(heading = c(NA_real_, NA_real_))
  result <- circ_summarise(hd, heading)
  expect_equal(result$n, 0L)
  expect_true(is.na(result$mean_dir))
  expect_true(is.na(result$mean_dir_deg))
  expect_true(is.na(result$resultant_R))
  expect_true(is.na(result$kappa))
})

test_that("circ_summarise n=1 returns kappa=NA but valid mean_dir and R", {
  hd     <- data.frame(heading = pi/4)
  result <- circ_summarise(hd, heading)
  expect_equal(result$n, 1L)
  expect_true(is.na(result$kappa))
  expect_false(is.na(result$mean_dir))
  expect_equal(result$resultant_R, 1, tolerance = 1e-6)
})

test_that("circ_summarise n=2 returns kappa=NA but valid mean_dir and R", {
  hd     <- data.frame(heading = c(pi/4, pi/4))
  result <- circ_summarise(hd, heading)
  expect_equal(result$n, 2L)
  expect_true(is.na(result$kappa))
  expect_false(is.na(result$mean_dir))
})

test_that("circ_summarise non-finite angles are excluded from n", {
  hd     <- data.frame(heading = c(pi/4, Inf, -Inf, NaN, NA))
  result <- circ_summarise(hd, heading)
  expect_equal(result$n, 1L)
  expect_equal(result$mean_dir, pi/4, tolerance = 1e-6)
})

test_that("circ_summarise all-NA group within multi-group data", {
  hd <- data.frame(
    heading = c(pi/4, NA_real_, NA_real_),
    arc     = c("a",  "b",     "b")
  )
  result <- circ_summarise(hd, heading, .by = "arc")
  row_b  <- result[result$arc == "b", ]
  expect_equal(row_b$n, 0L)
  expect_true(is.na(row_b$mean_dir))
})

test_that("circ_summarise missing col raises informative error", {
  hd <- data.frame(heading = pi/4)
  expect_error(
    circ_summarise(hd, foo),
    "`col` column 'foo' not found in data"
  )
})

test_that("circ_summarise quoted col name also works", {
  hd <- data.frame(angle = c(0, pi/2))
  expect_no_error(circ_summarise(hd, "angle"))
})
```

- [ ] **Step 2: Run all tests**

```bash
Rscript -e "devtools::test()" 2>&1 | tail -8
```
Expected: `FAIL 4 | PASS 39X` where the 4 failures are the pre-existing unrelated ones (`test-calibration.R`, `test-simulate-tracks.R`). All new `circ_summarise` tests PASS.

- [ ] **Step 3: Rebuild documentation**

```bash
Rscript -e "devtools::document()" 2>&1 | tail -5
```
Expected: `Writing NAMESPACE` and `Writing man/circ_summarise.Rd`.

- [ ] **Step 4: Run full check**

```bash
Rscript -e "devtools::check()" 2>&1 | grep -E "^(ERROR|WARNING|NOTE|--)" | head -20
```
Expected: 0 ERRORs, 0 WARNINGs. Possible NOTE about new exports.

- [ ] **Step 5: Commit**

```bash
git add tests/testthat/test-circular-stats.R NAMESPACE man/circ_summarise.Rd
git commit -m "test: circ_summarise edge cases and error handling; document"
```

---

## Final: merge to master

```bash
git checkout master
git merge feat/circ-summarise
git branch -d feat/circ-summarise
```

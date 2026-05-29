# Headings Frame Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add `headings_frame()` + `stack_headings()` for loading and preprocessing direct heading data, `add_stacked_headings()` as a ggplot2 layer, and convert `radiate()` to an S3 generic so `radiate(hf)` produces a headings plot and `radiate(ts)` continues to produce trajectory plots unchanged.

**Architecture:** `headings_frame()` and `stack_headings()` live in a new `R/headings_frame.R`; `add_stacked_headings()` and `radiate.headings_frame()` are appended to `R/circular_plotting.R`. The S3 refactor of `radiate()` is a three-line change — one generic definition plus renaming the existing body — so no existing behaviour changes. All plotting layers return ggplot2 `LayerInstance` objects and compose with `+` as usual.

**Tech Stack:** R, ggplot2, rlang, testthat 3

---

## File structure

| Action | Path |
|---|---|
| Create | `R/headings_frame.R` |
| Modify | `R/circular_plotting.R` — append `add_stacked_headings()` + `radiate.headings_frame()`; edit `radiate()` to S3 generic |
| Create | `tests/testthat/test-headings-frame.R` |
| Modify | `DESCRIPTION` — add `'headings_frame.R'` to Collate |

---

## Task 1: `headings_frame()` constructor

**Files:**
- Create: `R/headings_frame.R`
- Create: `tests/testthat/test-headings-frame.R`

- [ ] **Step 1: Create the test file with failing tests**

```r
# tests/testthat/test-headings-frame.R
test_that("headings_frame returns correct classes and attributes", {
  df <- data.frame(bearing = c(0, pi/4, pi/2))
  hf <- headings_frame(df, col = "bearing", units = "radians")
  expect_s3_class(hf, "headings_frame")
  expect_s3_class(hf, "data.frame")
  expect_equal(attr(hf, "heading_col"),      "bearing")
  expect_equal(attr(hf, "angle_convention"), "unit_circle")
  expect_equal(attr(hf, "coords"),           "absolute")
})

test_that("headings_frame converts degrees to radians in place", {
  df <- data.frame(deg = c(0, 90, 180, 270))
  hf <- headings_frame(df, col = "deg", units = "degrees")
  expect_equal(hf$deg, c(0, pi/2, pi, 3*pi/2), tolerance = 1e-10)
})

test_that("headings_frame accepts non-default angle_convention and coords", {
  df <- data.frame(h = c(0, pi/4))
  hf <- headings_frame(df, col = "h", units = "radians",
                       angle_convention = "clock", coords = "relative")
  expect_equal(attr(hf, "angle_convention"), "clock")
  expect_equal(attr(hf, "coords"),           "relative")
})

test_that("headings_frame errors informatively when col not found", {
  df <- data.frame(x = 1)
  expect_error(headings_frame(df, col = "bearing", units = "radians"),
               "column 'bearing' not found")
})

test_that("headings_frame errors informatively when units missing", {
  df <- data.frame(heading = 0)
  expect_error(headings_frame(df, col = "heading"),
               "'units' must be specified")
})

test_that("headings_frame preserves all original columns", {
  df <- data.frame(id = "A", trial = 1L, heading = pi/4)
  hf <- headings_frame(df, col = "heading", units = "radians")
  expect_true(all(c("id", "trial", "heading") %in% names(hf)))
})
```

- [ ] **Step 2: Run tests to confirm they fail**

```bash
Rscript -e "testthat::test_file('tests/testthat/test-headings-frame.R')"
```

Expected: errors like `could not find function "headings_frame"`

- [ ] **Step 3: Create `R/headings_frame.R` with the constructor**

```r
# R/headings_frame.R

#' Construct a headings frame from a data frame of angles
#'
#' Validates the angle column, optionally converts degrees to radians, and
#' marks the data frame with class \code{headings_frame} and attributes that
#' downstream functions (\code{\link{stack_headings}},
#' \code{\link{add_stacked_headings}}, \code{\link{radiate}}) use as
#' defaults.
#'
#' @param data A data frame containing the angle column.
#' @param col Unquoted or quoted name of the angle column.
#' @param units Units of the angle column: \code{"radians"} or
#'   \code{"degrees"}. No default — must be specified. Values are converted
#'   to radians in place when \code{"degrees"}.
#' @param angle_convention \code{"unit_circle"} (0 = East, CCW, default) or
#'   \code{"clock"} (0 = North, CW).
#' @param coords \code{"absolute"} (default) or \code{"relative"}.
#'
#' @return A \code{data.frame} with additional class \code{"headings_frame"}
#'   and attributes \code{heading_col}, \code{angle_convention},
#'   \code{coords}.
#'
#' @seealso \code{\link{stack_headings}}, \code{\link{add_stacked_headings}},
#'   \code{\link{radiate}}
#' @export
headings_frame <- function(data,
                           col,
                           units,
                           angle_convention = "unit_circle",
                           coords           = "absolute") {
  col_name <- if (is.character(col)) col[1L] else
                rlang::as_string(rlang::ensym(col))
  if (!col_name %in% names(data))
    stop(sprintf("column '%s' not found in data.", col_name))
  if (missing(units))
    stop("'units' must be specified: use \"radians\" or \"degrees\".\n",
         "  Hint: most behavioral data is recorded in degrees.")
  units            <- match.arg(units, c("radians", "degrees"))
  angle_convention <- match.arg(angle_convention, c("unit_circle", "clock"))
  coords           <- match.arg(coords, c("absolute", "relative"))

  .check_angle_units(data[[col_name]], units, col_name)
  if (units == "degrees")
    data[[col_name]] <- data[[col_name]] * pi / 180

  attr(data, "angle_convention") <- angle_convention
  attr(data, "coords")           <- coords
  attr(data, "heading_col")      <- col_name
  class(data) <- c("headings_frame", "data.frame")
  data
}
```

- [ ] **Step 4: Run tests to confirm they pass**

```bash
Rscript -e "testthat::test_file('tests/testthat/test-headings-frame.R')"
```

Expected: 6 tests pass.

- [ ] **Step 5: Commit**

```bash
git add R/headings_frame.R tests/testthat/test-headings-frame.R
git commit -m "feat: add headings_frame() constructor"
```

---

## Task 2: `stack_headings()` preprocessing

**Files:**
- Modify: `R/headings_frame.R` (append)
- Modify: `tests/testthat/test-headings-frame.R` (append)

- [ ] **Step 1: Append failing tests**

Append to `tests/testthat/test-headings-frame.R`:

```r
# ---- stack_headings -----------------------------------------------------------

test_that("stack_headings always adds stack_r and stack_n columns", {
  df <- data.frame(heading = c(0, pi/4, pi/2))
  hf <- headings_frame(df, col = "heading", units = "radians")
  out <- stack_headings(hf)
  expect_true(all(c("stack_r", "stack_n") %in% names(out)))
  expect_equal(nrow(out), 3L)  # row count unchanged
})

test_that("stack_headings inward: outermost rank=1 at base_r, inner ranks decrease", {
  df <- data.frame(heading = c(0, 0, 0))
  hf <- headings_frame(df, col = "heading", units = "radians")
  out <- stack_headings(hf, step = 0.025, direction = "inward", base_r = 1)
  expect_equal(max(out$stack_r), 1, tolerance = 1e-10)
  expect_equal(sort(out$stack_r, decreasing = TRUE),
               c(1, 0.975, 0.950), tolerance = 1e-10)
})

test_that("stack_headings outward: outermost rank=1 at base_r, inner ranks increase", {
  df <- data.frame(heading = c(0, 0))
  hf <- headings_frame(df, col = "heading", units = "radians")
  out <- stack_headings(hf, step = 0.025, direction = "outward", base_r = 1)
  expect_equal(min(out$stack_r), 1, tolerance = 1e-10)
  expect_equal(sort(out$stack_r), c(1, 1.025), tolerance = 1e-10)
})

test_that("stack_headings unique angles: every observation gets stack_r == base_r", {
  df <- data.frame(heading = c(0, pi/4, pi/2))
  hf <- headings_frame(df, col = "heading", units = "radians")
  out <- stack_headings(hf, base_r = 1)
  expect_true(all(out$stack_r == 1))
  expect_true(all(out$stack_n == 1L))
})

test_that("stack_headings stack_n counts coincident angles correctly", {
  df <- data.frame(heading = c(0, 0, 0, pi/2, pi/2))
  hf <- headings_frame(df, col = "heading", units = "radians")
  out <- stack_headings(hf)
  expect_equal(out$stack_n[out$heading == 0],    rep(3L, 3))
  expect_equal(out$stack_n[out$heading == pi/2], rep(2L, 2))
})

test_that("stack_headings shade=TRUE adds shade_n column equal to stack_n", {
  df <- data.frame(heading = c(0, 0, pi/4))
  hf <- headings_frame(df, col = "heading", units = "radians")
  out <- stack_headings(hf, shade = TRUE)
  expect_true("shade_n" %in% names(out))
  expect_equal(out$shade_n, out$stack_n)
})

test_that("stack_headings shade=FALSE does not add shade_n", {
  df <- data.frame(heading = c(0, 0))
  hf <- headings_frame(df, col = "heading", units = "radians")
  out <- stack_headings(hf, shade = FALSE)
  expect_false("shade_n" %in% names(out))
})

test_that("stack_headings shape=TRUE: n=1 group gets shape_code=1", {
  df <- data.frame(heading = c(0, pi/4))
  hf <- headings_frame(df, col = "heading", units = "radians")
  out <- stack_headings(hf, shape = TRUE)
  expect_true("shape_code" %in% names(out))
  expect_true(all(out$shape_code == 1L))
})

test_that("stack_headings shape=TRUE: n=3 group gets codes 1, 2, 3", {
  df <- data.frame(heading = c(0, 0, 0))
  hf <- headings_frame(df, col = "heading", units = "radians")
  out <- stack_headings(hf, shape = TRUE)
  expect_equal(sort(out$shape_code), c(1L, 2L, 3L))
})

test_that("stack_headings shape=FALSE does not add shape_code", {
  df <- data.frame(heading = c(0, 0))
  hf <- headings_frame(df, col = "heading", units = "radians")
  out <- stack_headings(hf, shape = FALSE)
  expect_false("shape_code" %in% names(out))
})

test_that("stack_headings tol groups near-coincident angles", {
  # 0 and 0.01 are within tol=0.05; pi/2 is separate
  df <- data.frame(heading = c(0, 0.01, pi/2))
  hf <- headings_frame(df, col = "heading", units = "radians")
  out <- stack_headings(hf, tol = 0.05)
  expect_equal(out$stack_n[out$heading == 0],    2L)
  expect_equal(out$stack_n[out$heading == 0.01], 2L)
  expect_equal(out$stack_n[out$heading == pi/2], 1L)
})

test_that("stack_headings col defaults to heading_col attribute", {
  df <- data.frame(bearing = c(0, 0))
  hf <- headings_frame(df, col = "bearing", units = "radians")
  out <- stack_headings(hf)   # no col arg — should find "bearing" via attribute
  expect_equal(max(out$stack_n), 2L)
})

test_that("stack_headings errors when step <= 0", {
  df <- data.frame(heading = 0)
  hf <- headings_frame(df, col = "heading", units = "radians")
  expect_error(stack_headings(hf, step = 0),  "'step' must be positive")
  expect_error(stack_headings(hf, step = -1), "'step' must be positive")
})

test_that("stack_headings errors when tol < 0", {
  df <- data.frame(heading = 0)
  hf <- headings_frame(df, col = "heading", units = "radians")
  expect_error(stack_headings(hf, tol = -0.1), "'tol' must be NULL or non-negative")
})
```

- [ ] **Step 2: Run tests to confirm they fail**

```bash
Rscript -e "testthat::test_file('tests/testthat/test-headings-frame.R')"
```

Expected: `stack_headings` tests fail with "could not find function".

- [ ] **Step 3: Append `stack_headings()` to `R/headings_frame.R`**

```r
#' Add stacking columns to a headings data frame
#'
#' Computes radial positions for stacked dot plots on circular plots.
#' Observations at the same angle (or within \code{tol} radians of each other)
#' are assigned successive radial positions, preventing overplotting of
#' coincident or binned headings.
#'
#' @param data A data frame with an angle column in radians.
#' @param col Name of the angle column. Defaults to the \code{heading_col}
#'   attribute when \code{data} is a \code{headings_frame}.
#' @param step Radial offset per stack level as a fraction of \code{base_r}.
#'   Default \code{0.025} matches \code{circular}'s \code{sep} default.
#' @param tol Grouping tolerance in radians. \code{NULL} (default) = exact
#'   equality, correct for binned data. \code{tol > 0} groups angles within
#'   \code{tol} radians for continuous headings.
#' @param direction \code{"inward"} (default, stacks toward centre) or
#'   \code{"outward"} (away from perimeter, matches \code{circular} default).
#' @param base_r Radius of the reference circle in data units. Default 1.
#' @param shade If \code{TRUE}, add a \code{shade_n} column (alias of
#'   \code{stack_n}) for use as an alpha aesthetic.
#' @param shape If \code{TRUE}, add a \code{shape_code} integer column:
#'   1 = hollow (outermost / singleton), 2 = filled (middle), 3 = filled with
#'   ring (innermost in a stack of 3+).
#'
#' @return \code{data} augmented with \code{stack_r} and \code{stack_n}
#'   columns (always), plus \code{shade_n} and/or \code{shape_code} when
#'   requested. Row count is unchanged.
#'
#' @seealso \code{\link{headings_frame}}, \code{\link{add_stacked_headings}}
#' @export
stack_headings <- function(data,
                           col       = NULL,
                           step      = 0.025,
                           tol       = NULL,
                           direction = "inward",
                           base_r    = 1,
                           shade     = FALSE,
                           shape     = FALSE) {
  if (is.null(col))
    col <- if (!is.null(attr(data, "heading_col"))) attr(data, "heading_col")
           else "heading"
  if (!col %in% names(data))
    stop(sprintf("column '%s' not found in data.", col))
  if (step <= 0)
    stop("'step' must be positive.")
  if (!is.null(tol) && tol < 0)
    stop("'tol' must be NULL or non-negative.")
  direction <- match.arg(direction, c("inward", "outward"))

  angles <- data[[col]]
  n      <- length(angles)

  # --- Assign group IDs -------------------------------------------------------
  if (is.null(tol)) {
    grp <- match(angles, unique(angles))
  } else {
    ord <- order(angles, na.last = TRUE)
    grp <- integer(n)
    centres <- numeric(0)
    g <- 0L
    for (i in ord) {
      a <- angles[i]
      if (is.na(a)) { grp[i] <- NA_integer_; next }
      if (length(centres)) {
        nearest <- which.min(abs(centres - a))
        if (abs(centres[nearest] - a) <= tol) { grp[i] <- nearest; next }
      }
      g <- g + 1L
      centres[g] <- a
      grp[i] <- g
    }
  }

  # --- Rank within each group (sorted angle order for ties) -------------------
  rank_in_grp <- integer(n)
  seen_g      <- if (any(!is.na(grp))) integer(max(grp, na.rm = TRUE)) else integer(0)
  for (i in order(angles, na.last = TRUE)) {
    g <- grp[i]
    if (!is.na(g)) {
      seen_g[g]      <- seen_g[g] + 1L
      rank_in_grp[i] <- seen_g[g]
    }
  }

  grp_tbl   <- tabulate(grp)
  grp_count <- ifelse(is.na(grp), NA_integer_, grp_tbl[grp])

  # --- Compute stack_r --------------------------------------------------------
  offset <- (rank_in_grp - 1L) * step
  data$stack_r <- if (direction == "inward") base_r - offset else base_r + offset
  data$stack_n <- as.integer(grp_count)

  if (shade) data$shade_n <- data$stack_n

  if (shape) {
    data$shape_code <- as.integer(mapply(
      function(rk, tot) {
        if (is.na(rk) || is.na(tot)) return(NA_integer_)
        if (tot == 1L || rk == 1L) return(1L)   # singleton or outermost
        if (rk == tot)             return(3L)   # innermost
        2L                                       # middle
      },
      rank_in_grp, data$stack_n,
      SIMPLIFY = TRUE, USE.NAMES = FALSE
    ))
  }

  data
}
```

- [ ] **Step 4: Run all tests in the file**

```bash
Rscript -e "testthat::test_file('tests/testthat/test-headings-frame.R')"
```

Expected: all tests pass (6 from Task 1 + 14 from Task 2).

- [ ] **Step 5: Commit**

```bash
git add R/headings_frame.R tests/testthat/test-headings-frame.R
git commit -m "feat: add stack_headings() preprocessing"
```

---

## Task 3: `add_stacked_headings()` ggplot2 layer

**Files:**
- Modify: `R/circular_plotting.R` (append before the `# ---- themes` section at line ~1235)
- Modify: `tests/testthat/test-headings-frame.R` (append)

- [ ] **Step 1: Append failing tests**

Append to `tests/testthat/test-headings-frame.R`:

```r
# ---- add_stacked_headings ----------------------------------------------------

test_that("add_stacked_headings returns a ggplot2 layer", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(heading = c(0, pi/4, pi/2))
  hf <- headings_frame(df, col = "heading", units = "radians")
  layer <- add_stacked_headings(hf)
  expect_s3_class(layer, "LayerInstance")
})

test_that("add_stacked_headings auto-computes stack_r when absent", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(heading = c(0, 0, pi/4))
  hf <- headings_frame(df, col = "heading", units = "radians")
  expect_no_error(add_stacked_headings(hf))
})

test_that("add_stacked_headings uses pre-computed stack_r without recomputing", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(heading = c(0, pi/4))
  hf <- headings_frame(df, col = "heading", units = "radians")
  pre <- stack_headings(hf)
  pre$stack_r <- c(0.99, 0.99)  # custom values
  layer <- add_stacked_headings(pre)
  # The layer data must retain our custom stack_r
  expect_equal(layer$data$stack_r, c(0.99, 0.99))
})

test_that("add_stacked_headings col defaults to heading_col attribute", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(bearing = c(0, pi/4))
  hf <- headings_frame(df, col = "bearing", units = "radians")
  expect_no_error(add_stacked_headings(hf))  # finds "bearing" via attribute
})

test_that("add_stacked_headings errors when col not found", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(x = 1)
  expect_error(add_stacked_headings(df, col = "bearing"),
               "column 'bearing' not found")
})
```

- [ ] **Step 2: Run tests to confirm they fail**

```bash
Rscript -e "testthat::test_file('tests/testthat/test-headings-frame.R')"
```

Expected: the 5 new tests fail with "could not find function".

- [ ] **Step 3: Append `add_stacked_headings()` to `R/circular_plotting.R`**

Find the `# ---- heading overlay layers` section (around line 1106) and append this function after `add_heading_vectors()` (around line 1233):

```r
#' Add stacked heading dots as a ggplot2 layer
#'
#' Places one point per observation at its heading angle, stacking coincident
#' angles radially to avoid overplotting. If \code{stack_r} is already a
#' column in \code{data} (from a prior call to \code{\link{stack_headings}}),
#' it is used as-is; otherwise stacking is computed internally.
#'
#' @param data A data frame with an angle column in radians, typically a
#'   \code{\link{headings_frame}}.
#' @param col Name of the angle column. Defaults to the \code{heading_col}
#'   attribute when \code{data} is a \code{headings_frame}.
#' @param step,tol,direction,base_r Passed to \code{\link{stack_headings}}
#'   when \code{stack_r} is absent. See that function for details.
#' @param shade If \code{TRUE}, map \code{stack_n} to the alpha aesthetic
#'   (scaled 0.2–1 across the observed range). Overrides the fixed
#'   \code{alpha} argument.
#' @param shape If \code{TRUE}, map \code{shape_code} to ggplot2 shape
#'   integers (1 = hollow circle, 16 = filled, 21 = filled with ring).
#' @param colour Fixed point colour (ignored when \code{colour_col} is set).
#' @param colour_col Optional column name to map to the colour aesthetic.
#' @param size Point size passed to \code{geom_point()}.
#' @param alpha Fixed alpha. Ignored when \code{shade = TRUE}.
#' @param ... Additional arguments passed to \code{ggplot2::geom_point()}.
#'
#' @return A \code{geom_point()} layer.
#'
#' @seealso \code{\link{headings_frame}}, \code{\link{stack_headings}},
#'   \code{\link{radiate}}, \code{\link{add_heading_points}}
#' @importFrom ggplot2 geom_point aes
#' @importFrom rlang .data sym
#' @export
add_stacked_headings <- function(data,
                                 col        = NULL,
                                 step       = 0.025,
                                 tol        = NULL,
                                 direction  = "inward",
                                 base_r     = 1,
                                 shade      = FALSE,
                                 shape      = FALSE,
                                 colour     = "black",
                                 colour_col = NULL,
                                 size       = 2,
                                 alpha      = 1,
                                 ...) {
  if (is.null(col))
    col <- if (!is.null(attr(data, "heading_col"))) attr(data, "heading_col")
           else "heading"
  if (!col %in% names(data))
    stop(sprintf("column '%s' not found in data.", col))

  if (!"stack_r" %in% names(data))
    data <- stack_headings(data, col = col, step = step, tol = tol,
                           direction = direction, base_r = base_r,
                           shade = shade, shape = shape)

  if (identical(attr(data, "display_convention"), "clock")) {
    disp <- .to_clock_display(data$stack_r * cos(data[[col]]),
                               data$stack_r * sin(data[[col]]))
    data[[".x_stk"]] <- disp$x
    data[[".y_stk"]] <- disp$y
  } else {
    data[[".x_stk"]] <- data$stack_r * cos(data[[col]])
    data[[".y_stk"]] <- data$stack_r * sin(data[[col]])
  }

  mapping <- ggplot2::aes(x = .data[[".x_stk"]], y = .data[[".y_stk"]])

  use_fixed_colour <- is.null(colour_col) || !colour_col %in% names(data)
  if (!use_fixed_colour) mapping[["colour"]] <- rlang::sym(colour_col)

  use_fixed_shape <- TRUE
  if ("shape_code" %in% names(data)) {
    pch_map <- c(`1` = 1L, `2` = 16L, `3` = 21L)
    data[[".shape_pch"]] <- pch_map[as.character(data$shape_code)]
    mapping[["shape"]] <- rlang::sym(".shape_pch")
    use_fixed_shape <- FALSE
  }

  use_fixed_alpha <- TRUE
  if (shade && "stack_n" %in% names(data)) {
    mx <- max(data$stack_n, na.rm = TRUE)
    if (mx > 0) {
      data[[".alpha_v"]] <- 0.2 + 0.8 * data$stack_n / mx
      mapping[["alpha"]] <- rlang::sym(".alpha_v")
      use_fixed_alpha <- FALSE
    }
  }

  args <- list(data = data, mapping = mapping, size = size,
               inherit.aes = FALSE, ...)
  if (use_fixed_colour) args$colour <- colour
  if (use_fixed_alpha)  args$alpha  <- alpha
  if (use_fixed_shape)  args$shape  <- 16L

  do.call(ggplot2::geom_point, args)
}
```

- [ ] **Step 4: Run all tests in the file**

```bash
Rscript -e "testthat::test_file('tests/testthat/test-headings-frame.R')"
```

Expected: all tests pass.

- [ ] **Step 5: Commit**

```bash
git add R/circular_plotting.R tests/testthat/test-headings-frame.R
git commit -m "feat: add add_stacked_headings() ggplot2 layer"
```

---

## Task 4: Refactor `radiate()` to S3 generic

**Files:**
- Modify: `R/circular_plotting.R` — lines around 1569
- Modify: `tests/testthat/test-headings-frame.R` (append backward-compat test)

This is a three-line change. The existing function body becomes `radiate.default`; `radiate.TrajSet` forwards to it; `radiate` becomes a one-line generic.

- [ ] **Step 1: Append backward-compatibility tests**

Append to `tests/testthat/test-headings-frame.R`:

```r
# ---- radiate() backward compatibility ----------------------------------------

test_that("radiate() still works on a TrajSet after S3 refactor", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(
    id      = rep("A", 4L),
    time    = 1:4,
    x       = c(0.5, 0.7, 0.6, 0.8),
    y       = c(0.5, 0.6, 0.7, 0.5)
  )
  ts <- TrajSet(df, id = "id", time = "time", x = "x", y = "y")
  p  <- radiate(ts)
  expect_s3_class(p, "ggplot")
})

test_that("radiate() still works on a plain data frame after S3 refactor", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(
    trial_id = rep("T1", 3L),
    frame    = 1:3,
    rel_x    = c(0.5, 0.6, 0.7),
    rel_y    = c(0.5, 0.4, 0.3)
  )
  p <- radiate(df, x_col = "rel_x", y_col = "rel_y", group_col = "trial_id")
  expect_s3_class(p, "ggplot")
})
```

- [ ] **Step 2: Run tests — they should pass before the refactor**

```bash
Rscript -e "testthat::test_file('tests/testthat/test-headings-frame.R')"
```

Expected: the 2 new backward-compat tests pass (current `radiate()` handles these fine).

- [ ] **Step 3: Apply the S3 refactor**

Open `R/circular_plotting.R`. Find the line (around 1569 in the original, adjust line number after earlier insertions from Task 3):

```r
radiate <- function(
  data,
```

Replace those two lines with:

```r
radiate <- function(data, ...) UseMethod("radiate")

radiate.TrajSet <- function(data, ...) radiate.default(data, ...)

#' @rdname radiate
radiate.default <- function(
  data,
```

That is the complete change. The rest of the function body is untouched.

Also add `#' @export` before the generic and `#' @rdname radiate` before `radiate.TrajSet` and `radiate.default` in the Roxygen block. The existing `@export` on `radiate` covers the generic; the methods inherit it via `@rdname`.

- [ ] **Step 4: Run backward-compat tests to confirm they still pass**

```bash
Rscript -e "testthat::test_file('tests/testthat/test-headings-frame.R')"
```

Expected: all tests pass including the 2 backward-compat ones.

- [ ] **Step 5: Run the full test suite to confirm nothing else broke**

```bash
Rscript -e "devtools::test()" 2>&1 | tail -5
```

Expected: same pass/fail counts as before this task (pre-existing failures unchanged).

- [ ] **Step 6: Commit**

```bash
git add R/circular_plotting.R tests/testthat/test-headings-frame.R
git commit -m "refactor: convert radiate() to S3 generic"
```

---

## Task 5: `radiate.headings_frame()` method

**Files:**
- Modify: `R/circular_plotting.R` (append after `radiate.default`)
- Modify: `tests/testthat/test-headings-frame.R` (append)

- [ ] **Step 1: Append failing tests**

Append to `tests/testthat/test-headings-frame.R`:

```r
# ---- radiate.headings_frame --------------------------------------------------

test_that("radiate(headings_frame) returns a ggplot object", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(heading = c(0, pi/4, pi/2, pi, 3*pi/2))
  hf <- headings_frame(df, col = "heading", units = "radians")
  p  <- radiate(hf)
  expect_s3_class(p, "ggplot")
})

test_that("radiate(headings_frame) with panel_by produces facets", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(heading = c(0, pi/4, pi/2, pi),
                   grp     = c("a", "a", "b", "b"))
  hf <- headings_frame(df, col = "heading", units = "radians")
  p  <- radiate(hf, panel_by = "grp")
  expect_false(inherits(p$facet, "FacetNull"))
})

test_that("radiate(headings_frame) forwards stacking params to add_stacked_headings", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(heading = c(0, 0, pi/4))
  hf <- headings_frame(df, col = "heading", units = "radians")
  # direction="outward" — should not error
  expect_no_error(radiate(hf, direction = "outward"))
})

test_that("radiate(headings_frame) panel_by errors on missing column", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(heading = c(0, pi/4))
  hf <- headings_frame(df, col = "heading", units = "radians")
  expect_error(radiate(hf, panel_by = "nonexistent"),
               "panel_by column")
})
```

- [ ] **Step 2: Run tests to confirm they fail**

```bash
Rscript -e "testthat::test_file('tests/testthat/test-headings-frame.R')"
```

Expected: the 4 new tests fail with "could not find function `radiate.headings_frame`".

- [ ] **Step 3: Append `radiate.headings_frame()` to `R/circular_plotting.R`** (after `radiate.default`)

```r
#' @rdname radiate
#' @param col Name of the angle column in \code{data}. Defaults to the
#'   \code{heading_col} attribute when \code{data} is a
#'   \code{\link{headings_frame}}.
#' @param step,tol,direction,base_r,shade,shape Passed to
#'   \code{\link{add_stacked_headings}}. See that function for details.
#' @export
radiate.headings_frame <- function(
  data,
  col       = NULL,
  step      = 0.025,
  tol       = NULL,
  direction = "inward",
  base_r    = 1,
  shade     = FALSE,
  shape     = FALSE,
  panel_by  = NULL,
  ncol      = NULL,
  ticks     = TRUE,
  degrees   = TRUE,
  title     = NULL,
  style     = c("classic", "minimal"),
  ...) {

  style <- match.arg(style)

  if (style == "minimal") {
    g <- ggplot2::ggplot() + ggplot2::coord_fixed() + spartan_theme()
  } else {
    g <- ggplot2::ggplot() + ggplot2::coord_fixed() + sparse_theme()
  }

  g <- g +
    add_quadrant_lines() +
    add_circ(circle_color = if (style == "minimal") "grey60" else "black",
             circle_size  = if (style == "minimal") 1        else 1.5)

  if (ticks)   g <- g + add_ticks()
  if (degrees) g <- g + degree_labs()

  g <- g + add_stacked_headings(
    data, col = col, step = step, tol = tol, direction = direction,
    base_r = base_r, shade = shade, shape = shape, ...
  )

  if (!is.null(panel_by)) {
    if (!is.character(panel_by))
      stop("`panel_by` must be a character string.")
    missing_pby <- setdiff(panel_by, names(data))
    if (length(missing_pby))
      stop("panel_by column(s) not found in data: ",
           paste(missing_pby, collapse = ", "))
    g <- g + ggplot2::facet_wrap(
      stats::as.formula(paste("~", paste(panel_by, collapse = "+"))),
      ncol = ncol
    )
  }

  if (!is.null(title))
    g <- g + ggplot2::ggtitle(as.character(title)) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 14, hjust = 0.5))

  g
}
```

- [ ] **Step 4: Run all tests in the file**

```bash
Rscript -e "testthat::test_file('tests/testthat/test-headings-frame.R')"
```

Expected: all tests pass.

- [ ] **Step 5: Commit**

```bash
git add R/circular_plotting.R tests/testthat/test-headings-frame.R
git commit -m "feat: add radiate.headings_frame() S3 method"
```

---

## Task 6: Register, document, and verify

**Files:**
- Modify: `DESCRIPTION` — add `'headings_frame.R'` to Collate
- Generated: `NAMESPACE`, `man/headings_frame.Rd`, `man/stack_headings.Rd`,
  `man/add_stacked_headings.Rd`

- [ ] **Step 1: Add `headings_frame.R` to Collate in `DESCRIPTION`**

In `DESCRIPTION`, the Collate block currently ends with:

```
    'headings.R'
    'loaders.R'
```

Change it to:

```
    'headings.R'
    'headings_frame.R'
    'loaders.R'
```

(`headings_frame.R` must come after `headings.R` because it calls
`stack_headings()` which uses `%||%` defined in `headings.R`.)

- [ ] **Step 2: Regenerate docs and NAMESPACE**

```bash
Rscript -e "devtools::document()"
```

Expected output includes lines like:
```
Writing 'headings_frame.Rd'
Writing 'stack_headings.Rd'
Writing 'add_stacked_headings.Rd'
Writing 'NAMESPACE'
```

Check that `NAMESPACE` now contains:
```
export(headings_frame)
export(stack_headings)
export(add_stacked_headings)
```

```bash
grep "headings_frame\|stack_headings\|add_stacked_headings" NAMESPACE
```

- [ ] **Step 3: Run the full test suite**

```bash
Rscript -e "devtools::test()" 2>&1 | tail -5
```

Expected: same pre-existing failures as before this feature branch; new tests all pass.

- [ ] **Step 4: Run R CMD CHECK**

```bash
Rscript -e "devtools::check()" 2>&1 | grep -E "^(ERROR|WARNING|NOTE|FAIL)"
```

Expected: no new ERRORs or WARNINGs introduced by this feature.

- [ ] **Step 5: Commit**

```bash
git add DESCRIPTION NAMESPACE man/headings_frame.Rd man/stack_headings.Rd \
        man/add_stacked_headings.Rd
git commit -m "feat: register headings_frame.R; regenerate docs and NAMESPACE"
```

---

## Self-review notes

**Spec coverage check:**

| Spec requirement | Task |
|---|---|
| `headings_frame()` constructor with units, attributes, range check | Task 1 |
| `stack_headings()` with stack_r/stack_n/shade_n/shape_code | Task 2 |
| `stack_headings()` tol grouping | Task 2 |
| `stack_headings()` direction inward/outward | Task 2 |
| `stack_headings()` col defaults from heading_col attribute | Task 2 |
| `add_stacked_headings()` auto-stacks when stack_r absent | Task 3 |
| `add_stacked_headings()` uses pre-computed stack_r | Task 3 |
| `add_stacked_headings()` shape_code → ggplot2 pch mapping | Task 3 |
| `add_stacked_headings()` shade → alpha mapping | Task 3 |
| `radiate()` S3 generic — backward compat for TrajSet + plain df | Task 4 |
| `radiate.headings_frame()` method | Task 5 |
| `radiate.headings_frame()` panel_by faceting | Task 5 |
| DESCRIPTION Collate + NAMESPACE + Rd files | Task 6 |

**Type consistency:** `col` is always a length-1 character. `stack_r` is numeric. `stack_n` is integer. `shape_code` is integer. All consistent across Tasks 2, 3, 5.

**`%||%` dependency note:** `headings_frame.R` uses `%||%` via the inline `if (!is.null(...)) ... else ...` pattern — no dependency on `%||%` from `headings.R`. `stack_headings()` also avoids it. No cross-file symbol dependency.

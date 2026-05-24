# Coordinate Frame and Angular Convention Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add `coords` and `angle_convention` parameters to `derive_headings()` so callers can explicitly request stimulus-relative headings and clock-convention output (0° = North, clockwise), with auto-detection flowing downstream via `attr()`.

**Architecture:** Heading rules continue to compute in unit-circle space internally. Two post-processing steps are layered on top: (1) which Cartesian columns to use (`absolute` = `cols$x/y`; `relative` = `cols$rel_x/rel_y`), and (2) how to express the resulting angle (`unit_circle` = unchanged; `clock` = apply convention conversion). The output data frame carries `angle_convention` and `coords` attributes so downstream helpers (`circ_summary_headings`, `circ_mean_segments`) can auto-detect without requiring repeated parameter threading.

**Tech Stack:** R, S4 (methods), ggplot2, circular package, testthat, devtools/roxygen2

---

## File map

| File | What changes |
|------|-------------|
| `R/TrajSet.R` | Validator + constructor + `show()` for `rel_x`/`rel_y` in `@cols` |
| `R/headings.R` | `derive_headings()` coords+convention params; conversion helpers; `circ_summary_headings()` convention param+attr pass-through; `circ_mean_segments()` auto-detect |
| `R/circular_statistics.R` | `circ_summary()` TrajSet method: `angle_convention` param |
| `R/data.R` | Update `@format` docs to mention `rel_x`/`rel_y` col pointers |
| `data/plividus.rda` | Rebuilt with `rel_x`/`rel_y` registered in `@cols` |
| `vignettes/radiatR.Rmd` | Per-condition arrows use `coords="relative", angle_convention="clock"` |
| `tests/testthat/test-headings.R` | New tests for coords, angle_convention, attr tagging |
| `tests/testthat/test-circular-plotting.R` | New tests for `circ_mean_segments` clock auto-detect |
| `tests/testthat/test-circular-stats.R` | New tests for `circ_summary` convention param |

---

## Task 1: TrajSet — rel_x/rel_y in @cols

**Files:**
- Modify: `R/TrajSet.R`
- Test: `tests/testthat/test-headings.R`

### Background

`TrajSet@cols` is a plain list; adding `rel_x`/`rel_y` fields means updating the validator (to check columns exist when non-NULL), the `TrajSet()` constructor (to accept the two new args), and `show()` (to print them).

The validator block that checks `cl$x`/`cl$y` starts at line 174. The `new("TrajSet", ...)` call is at line 320. The `show()` method is at line 350.

- [ ] **Step 1: Write the failing test**

Add to `tests/testthat/test-headings.R`:

```r
test_that("TrajSet accepts rel_x/rel_y col pointers", {
  df <- data.frame(id = "A", time = 1:3,
                   x = c(0.1, 0.2, 0.3), y = c(0.0, 0.1, 0.2),
                   rx = c(-0.1, -0.2, -0.3), ry = c(0.0, -0.1, -0.2))
  ts <- TrajSet(df, id = "id", time = "time", x = "x", y = "y",
                rel_x = "rx", rel_y = "ry", normalize_xy = FALSE)
  expect_equal(ts@cols$rel_x, "rx")
  expect_equal(ts@cols$rel_y, "ry")
})

test_that("TrajSet validator rejects rel_x without rel_y", {
  df <- data.frame(id = "A", time = 1:3,
                   x = c(0.1, 0.2, 0.3), y = c(0.0, 0.1, 0.2),
                   rx = c(-0.1, -0.2, -0.3))
  expect_error(
    TrajSet(df, id = "id", time = "time", x = "x", y = "y",
            rel_x = "rx", normalize_xy = FALSE),
    "rel_y"
  )
})

test_that("TrajSet validator rejects rel_x pointing to absent column", {
  df <- data.frame(id = "A", time = 1:3,
                   x = c(0.1, 0.2, 0.3), y = c(0.0, 0.1, 0.2))
  expect_error(
    TrajSet(df, id = "id", time = "time", x = "x", y = "y",
            rel_x = "no_such", rel_y = "y", normalize_xy = FALSE),
    "rel_x"
  )
})
```

- [ ] **Step 2: Run tests to verify they fail**

```r
testthat::test_file("tests/testthat/test-headings.R")
```

Expected: FAIL — `TrajSet` does not accept `rel_x`/`rel_y` arguments.

- [ ] **Step 3: Update TrajSet() constructor signature and body**

In `R/TrajSet.R`, update the `TrajSet` function signature (line 212):

```r
TrajSet <- function(df,
                    id = "id", time = "time",
                    angle = NULL,
                    x = NULL, y = NULL,
                    rel_x = NULL, rel_y = NULL,
                    angle_unit = c("radians","degrees"),
                    weight = NULL,
                    normalize_xy = TRUE,
                    meta = list(),
                    transform_history = NULL) {
```

After the `if (!have_angle && !have_xy)` check (around line 231), add:

```r
  # Validate rel_x/rel_y pairing
  have_rel <- !is.null(rel_x) || !is.null(rel_y)
  if (have_rel) {
    if (is.null(rel_x) || is.null(rel_y))
      stop("Both rel_x and rel_y must be supplied if either is provided.")
    if (!rel_x %in% names(df))
      stop("rel_x column '", rel_x, "' not found in df.")
    if (!rel_y %in% names(df))
      stop("rel_y column '", rel_y, "' not found in df.")
  }
```

Update the `new("TrajSet", ...)` call (line 320) to include the new fields:

```r
  new("TrajSet",
      data = d,
      cols = list(id = id, time = time, angle = angle_col,
                  x = if (!is.null(x)) x else NULL,
                  y = if (!is.null(y)) y else NULL,
                  rel_x = if (have_rel) rel_x else NULL,
                  rel_y = if (have_rel) rel_y else NULL,
                  raw_x = raw_cols$x,
                  raw_y = raw_cols$y,
                  rho = rho_col,
                  weight = weight),
      angle_unit = angle_unit,
      meta = meta)
```

- [ ] **Step 4: Update show() to print rel_x/rel_y when present**

In `R/TrajSet.R`, update `show()` (line 350):

```r
setMethod("show", "TrajSet", function(object) {
  id <- object@cols$id; tm <- object@cols$time; th <- object@cols$angle
  xy <- if (!is.null(object@cols$x)) paste0(", x='", object@cols$x, "', y='", object@cols$y, "'") else ""
  rel_xy <- if (!is.null(object@cols$rel_x)) paste0(", rel_x='", object@cols$rel_x, "', rel_y='", object@cols$rel_y, "'") else ""
  raw_xy <- if (!is.null(object@cols$raw_x)) paste0(", raw_x='", object@cols$raw_x, "', raw_y='", object@cols$raw_y, "'") else ""
  cat(sprintf("TrajSet: %d trajectories, %d observations\n", length(ids(object)), nrow(object@data)))
  cat(sprintf("Columns: id='%s', time='%s', angle='%s' (radians)%s%s%s\n", id, tm, th, xy, rel_xy, raw_xy))
  hist <- transform_history(object)
  if (nrow(hist)) {
    steps <- unique(hist$step[order(hist$order, hist$step)])
    cat("Transform steps:", paste(steps, collapse = " -> "), "\n")
  }
  print(utils::head(object@data, 6))
})
```

- [ ] **Step 5: Run tests — expect pass**

```r
testthat::test_file("tests/testthat/test-headings.R")
```

Expected: the three new tests PASS; all existing heading tests still PASS.

- [ ] **Step 6: Commit**

```bash
git add R/TrajSet.R tests/testthat/test-headings.R
git commit -m "feat: add rel_x/rel_y pointer fields to TrajSet@cols"
```

---

## Task 2: derive_headings() — coords parameter

**Files:**
- Modify: `R/headings.R`
- Test: `tests/testthat/test-headings.R`

### Background

`derive_headings()` TrajSet method resolves column names at line 410:
```r
id <- x@cols$id; tc <- x@cols$time; xc <- x@cols$x; yc <- x@cols$y
```
When `coords = "relative"`, use `x@cols$rel_x` / `x@cols$rel_y` instead.

- [ ] **Step 1: Write the failing tests**

Add to `tests/testthat/test-headings.R`:

```r
# Helper: TrajSet with both abs and rel coord columns
make_ts_with_rel <- function() {
  # Absolute heading: East (0°). Relative heading: North (pi/2 in UC), because
  # rel coords point North (stim at East, animal heads East -> rel = 0, but
  # here we explicitly set rel_x/rel_y to point North for simplicity).
  df <- data.frame(
    id = "A", time = 1:10,
    x  = seq(0, 0.8, length.out = 10),  # heading East in absolute
    y  = rep(0, 10),
    rx = rep(0, 10),                     # heading North in relative
    ry = seq(0, 0.8, length.out = 10)
  )
  TrajSet(df, id = "id", time = "time", x = "x", y = "y",
          rel_x = "rx", rel_y = "ry", normalize_xy = FALSE)
}

test_that("derive_headings coords='absolute' uses x/y columns", {
  ts <- make_ts_with_rel()
  hd <- derive_headings(ts, rule = "crossing", circ0 = 0.2, circ1 = 0.4,
                        coords = "absolute", angle_convention = "unit_circle")
  expect_equal(hd$heading, 0, tolerance = 1e-6)  # East
})

test_that("derive_headings coords='relative' uses rel_x/rel_y columns", {
  ts <- make_ts_with_rel()
  hd <- derive_headings(ts, rule = "crossing", circ0 = 0.2, circ1 = 0.4,
                        coords = "relative", angle_convention = "unit_circle")
  expect_equal(hd$heading, pi / 2, tolerance = 1e-6)  # North
})

test_that("derive_headings errors when coords='relative' but rel_x/rel_y not registered", {
  df <- data.frame(id = "A", time = 1:10,
                   x = seq(0, 0.8, length.out = 10), y = rep(0, 10))
  ts <- TrajSet(df, id = "id", time = "time", x = "x", y = "y",
                normalize_xy = FALSE)
  expect_error(
    derive_headings(ts, rule = "crossing", circ0 = 0.2, circ1 = 0.4,
                    coords = "relative"),
    "rel_x"
  )
})
```

- [ ] **Step 2: Run tests to verify they fail**

```r
testthat::test_file("tests/testthat/test-headings.R")
```

Expected: FAIL — `derive_headings` does not accept `coords` or `angle_convention`.

- [ ] **Step 3: Update derive_headings() generic signature**

In `R/headings.R`, update the `setGeneric("derive_headings", ...)` call (line 95):

```r
setGeneric(
  "derive_headings",
  function(
    x,
    rule = c("crossing","distal","straight","origin_mean","net","velocity_mean","window_net","goal_bias","pca_axis","ransac_straight","maxspeed_window","vm_fit","exit","entry","ring_tangent"),
    ...,
    coords = c("absolute", "relative"),
    angle_convention = c("clock", "unit_circle")
  ) standardGeneric("derive_headings")
)
```

- [ ] **Step 4: Update derive_headings() TrajSet method signature and column resolution**

In `R/headings.R`, update `setMethod("derive_headings", "TrajSet", ...)` (line 408):

```r
setMethod("derive_headings", "TrajSet", function(
    x,
    rule = c("crossing","distal","straight","origin_mean","net","velocity_mean",
             "window_net","goal_bias","pca_axis","ransac_straight","maxspeed_window",
             "vm_fit","exit","entry","ring_tangent"),
    ...,
    first_only = FALSE,
    carry = NULL,
    coords = c("absolute", "relative"),
    angle_convention = c("clock", "unit_circle")) {

  coords           <- match.arg(coords)
  angle_convention <- match.arg(angle_convention)

  id <- x@cols$id; tc <- x@cols$time

  if (coords == "relative") {
    if (is.null(x@cols$rel_x) || is.null(x@cols$rel_y))
      stop("coords='relative' requires rel_x and rel_y registered in TrajSet@cols.")
    xc <- x@cols$rel_x
    yc <- x@cols$rel_y
  } else {
    xc <- x@cols$x
    yc <- x@cols$y
  }

  if (is.null(xc) || is.null(yc))
    stop("derive_headings: TrajSet needs x/y columns for these rules.")

  # ... rest of the method body unchanged (d, sp, builtins, dispatch, etc.)
```

The rest of the method body (the `d <- x@data; sp <- split(...)` through `rownames(res) <- NULL; if (!is.null(carry)) ...`) stays the same.

At the very end, before `res`, add placeholder for angle conversion (implemented in Task 3).

- [ ] **Step 5: Run tests — expect pass**

```r
testthat::test_file("tests/testthat/test-headings.R")
```

Expected: the three new tests PASS; all existing tests still PASS.

- [ ] **Step 6: Commit**

```bash
git add R/headings.R tests/testthat/test-headings.R
git commit -m "feat: add coords param to derive_headings() to select abs/rel columns"
```

---

## Task 3: derive_headings() — angle_convention parameter and attr tagging

**Files:**
- Modify: `R/headings.R`
- Test: `tests/testthat/test-headings.R`

### Background

After the dispatch block computes `res` with `heading` in `[0, 2π)` unit-circle, apply the convention conversion, then tag the data frame with attributes.

Conversions (both map `[0, 2π)` UC to `[0, 2π)` clock):
- Absolute clock: `(pi/2 - uc) %% (2*pi)`
- Relative clock: `(-uc) %% (2*pi)`

- [ ] **Step 1: Write the failing tests**

Add to `tests/testthat/test-headings.R`:

```r
test_that("derive_headings clock convention converts absolute heading correctly", {
  # Straight East trajectory => UC heading 0. Absolute clock: (pi/2 - 0) = pi/2 = 90°
  df <- data.frame(id = "A", time = 1:10,
                   x = seq(0, 0.8, length.out = 10), y = rep(0, 10))
  ts <- TrajSet(df, id = "id", time = "time", x = "x", y = "y",
                normalize_xy = FALSE)
  hd <- derive_headings(ts, rule = "crossing", circ0 = 0.2, circ1 = 0.4,
                        angle_convention = "clock")
  # East (UC=0) → absolute clock = pi/2 (= 90° East of North)
  expect_equal(hd$heading, pi / 2, tolerance = 1e-6)
})

test_that("derive_headings clock convention converts relative heading correctly", {
  # rel_x/rel_y trajectory heading North (UC = pi/2).
  # Relative clock: (-pi/2) %% 2pi = 3pi/2? No: stimulus at UC=0 (East).
  # When rel_x/rel_y head North (UC pi/2): relative clock = (-pi/2) %% 2pi = 3pi/2.
  # Alternatively, a trajectory heading toward stimulus (rel UC=0) gives clock 0.
  df <- data.frame(
    id = "A", time = 1:10,
    x = rep(0, 10), y = seq(0, 0.8, length.out = 10),  # abs North
    rx = seq(0, 0.8, length.out = 10), ry = rep(0, 10) # rel East = toward stimulus
  )
  ts <- TrajSet(df, id = "id", time = "time", x = "x", y = "y",
                rel_x = "rx", rel_y = "ry", normalize_xy = FALSE)
  hd <- derive_headings(ts, rule = "crossing", circ0 = 0.2, circ1 = 0.4,
                        coords = "relative", angle_convention = "clock")
  # rel UC heading = 0 (East = toward stimulus). Relative clock = (-0) %% 2pi = 0.
  expect_equal(hd$heading, 0, tolerance = 1e-6)
})

test_that("derive_headings attaches angle_convention and coords attrs", {
  df <- data.frame(id = "A", time = 1:10,
                   x = seq(0, 0.8, length.out = 10), y = rep(0, 10))
  ts <- TrajSet(df, id = "id", time = "time", x = "x", y = "y",
                normalize_xy = FALSE)
  hd <- derive_headings(ts, rule = "crossing", circ0 = 0.2, circ1 = 0.4,
                        angle_convention = "clock")
  expect_equal(attr(hd, "angle_convention"), "clock")
  expect_equal(attr(hd, "coords"), "absolute")
})

test_that("derive_headings with angle_convention='unit_circle' matches raw atan2", {
  theta <- pi / 4
  n <- 20
  r <- seq(0, 0.8, length.out = n)
  df <- data.frame(id = "A", time = seq_len(n),
                   x = r * cos(theta), y = r * sin(theta))
  ts <- TrajSet(df, id = "id", time = "time", x = "x", y = "y",
                normalize_xy = FALSE)
  hd_uc <- derive_headings(ts, rule = "crossing", circ0 = 0.2, circ1 = 0.4,
                            angle_convention = "unit_circle")
  expect_equal(hd_uc$heading, theta %% (2 * pi), tolerance = 1e-10)
})
```

- [ ] **Step 2: Run tests to verify they fail**

```r
testthat::test_file("tests/testthat/test-headings.R")
```

Expected: FAIL — no conversion applied and no attrs set.

- [ ] **Step 3: Add conversion helpers in R/headings.R**

Near the top of `R/headings.R`, after the existing utility definitions, add:

```r
.uc_to_clock <- function(uc_heading, coords) {
  if (coords == "relative") {
    (-uc_heading) %% (2 * pi)
  } else {
    (pi / 2 - uc_heading) %% (2 * pi)
  }
}

.clock_to_uc <- function(clock_heading, coords) {
  if (coords == "relative") {
    (-clock_heading) %% (2 * pi)
  } else {
    wrap_to_2pi(rad_unclock(clock_heading))
  }
}
```

- [ ] **Step 4: Apply conversion at the end of the TrajSet method**

At the end of `setMethod("derive_headings", "TrajSet", ...)`, just before the `rownames(res) <- NULL` line, add:

```r
  if (angle_convention == "clock") {
    res$heading <- .uc_to_clock(res$heading, coords)
  }

  rownames(res) <- NULL
  if (!is.null(carry)) res <- .carry_nearest(res, d, idc = id, tc = tc, cols = carry)

  attr(res, "angle_convention") <- angle_convention
  attr(res, "coords")           <- coords
  res
```

- [ ] **Step 5: Fix existing tests broken by the new "clock" default**

The new default `angle_convention = "clock"` changes the output of `derive_headings()` for two existing tests. In `tests/testthat/test-headings.R`, add `angle_convention = "unit_circle"` to the two affected `derive_headings()` calls:

**Line ~63** (crossing rule recovers correct heading for N trajectories):
```r
hd  <- derive_headings(ts, rule = "crossing", circ0 = 0.2, circ1 = 0.4,
                       angle_convention = "unit_circle")
```

**Line ~157** (custom rule heading):
```r
res <- derive_headings(ts, rule = "zero_heading", angle_convention = "unit_circle")
```

- [ ] **Step 6: Run tests — expect pass**

```r
testthat::test_file("tests/testthat/test-headings.R")
```

Expected: all new and existing tests PASS.

- [ ] **Step 7: Commit**

```bash
git add R/headings.R tests/testthat/test-headings.R
git commit -m "feat: add angle_convention param to derive_headings() with attr tagging"
```

---

## Task 4: circ_summary_headings() — angle_convention param and attr pass-through

**Files:**
- Modify: `R/headings.R`, `R/circular_statistics.R`
- Test: `tests/testthat/test-headings.R`, `tests/testthat/test-circular-stats.R`

### Background

`circ_summary_headings()` calls `derive_headings()` and then computes circular mean. When headings are in clock convention, they must be reverse-converted to unit-circle before `mean.circular`, then the output `mean_dir` is re-converted to clock. The `circ_summary()` TrajSet method in `circular_statistics.R` always reads unit-circle step angles from `@cols$angle`; adding `angle_convention` just converts the output `mean_dir`.

- [ ] **Step 1: Write the failing tests**

Add to `tests/testthat/test-headings.R`:

```r
test_that("circ_summary_headings respects angle_convention='clock' via attr", {
  # Four trajectories all heading North (UC pi/2); clock should be 0 for absolute.
  angles <- rep(pi / 2, 4)
  ts <- make_multi_crossing_ts(angles)
  summ <- circ_summary_headings(ts, rule = "crossing",
                                circ0 = 0.2, circ1 = 0.4,
                                group_by = NULL,
                                angle_convention = "clock")
  # UC pi/2 -> abs clock (pi/2 - pi/2) %% 2pi = 0
  expect_equal(summ$mean_dir, 0, tolerance = 1e-6)
  expect_equal(attr(summ, "angle_convention"), "clock")
})

test_that("circ_summary_headings auto-detects convention from attr", {
  # Build a headings df tagged as clock manually
  angles <- rep(pi / 2, 4)
  ts <- make_multi_crossing_ts(angles)
  hd <- derive_headings(ts, rule = "crossing", circ0 = 0.2, circ1 = 0.4,
                        angle_convention = "clock")
  # mean_dir from heading data frame already tagged clock
  # circ_summary_headings takes a TrajSet, not headings — so test
  # circ_summary_headings with explicit param instead:
  summ_clock <- circ_summary_headings(ts, rule = "crossing",
                                      circ0 = 0.2, circ1 = 0.4,
                                      group_by = NULL,
                                      angle_convention = "clock")
  summ_uc    <- circ_summary_headings(ts, rule = "crossing",
                                      circ0 = 0.2, circ1 = 0.4,
                                      group_by = NULL,
                                      angle_convention = "unit_circle")
  # UC mean should be pi/2; clock mean should be 0
  expect_equal(summ_uc$mean_dir,    pi / 2, tolerance = 1e-6)
  expect_equal(summ_clock$mean_dir, 0,      tolerance = 1e-6)
})
```

Add to `tests/testthat/test-circular-stats.R`:

```r
test_that("circ_summary angle_convention='clock' converts mean_dir output", {
  library(radiatR)
  # Single-trajectory TrajSet with all angles at pi/2 (North in UC)
  df <- data.frame(id = "A", time = 1:4,
                   x = c(0, 0, 0, 0), y = c(0.2, 0.4, 0.6, 0.8))
  ts <- TrajSet(df, id = "id", time = "time", x = "x", y = "y",
                normalize_xy = FALSE)
  # The angle col stores atan2(y,x) of each point: pi/2 for all
  summ_clock <- circ_summary(ts, angle_convention = "clock")
  # Absolute clock: (pi/2 - pi/2) %% 2pi = 0
  expect_equal(summ_clock$mean_dir, 0, tolerance = 1e-6)
  # Default (unit_circle) unchanged
  summ_uc <- circ_summary(ts)
  expect_equal(summ_uc$mean_dir, pi / 2, tolerance = 1e-6)
})
```

- [ ] **Step 2: Run tests to verify they fail**

```r
testthat::test_file("tests/testthat/test-headings.R")
testthat::test_file("tests/testthat/test-circular-stats.R")
```

Expected: FAIL.

- [ ] **Step 3: Update circ_summary_headings() in R/headings.R**

Replace the current `circ_summary_headings` signature and the summary computation at lines 545–598:

```r
circ_summary_headings <- function(x, rule = c("crossing","distal","straight","origin_mean","net","velocity_mean"),
                                  group_by = "id", ...,
                                  angle_convention = c("clock", "unit_circle")) {
  rule             <- match.arg(rule)
  angle_convention <- match.arg(angle_convention)

  # derive_headings returns headings in the requested convention (+ coords attr)
  hd <- derive_headings(x, rule = rule, ..., angle_convention = angle_convention)
  coords <- attr(hd, "coords") %||% "absolute"

  if (nrow(hd) == 0L || all(is.na(hd$heading))) {
    return(data.frame(mean_dir = NA_real_, resultant_R = NA_real_, kappa = NA_real_, n = 0L))
  }

  # Convert back to UC for mean computation
  uc_heading <- if (angle_convention == "clock") {
    .clock_to_uc(hd$heading, coords)
  } else {
    hd$heading
  }
  hd$.uc_heading <- uc_heading

  # group keys (unchanged logic)
  if (is.null(group_by)) {
    hd$..grp <- factor("all")
  } else {
    if (!all(group_by %in% names(hd))) {
      if (!identical(group_by, "id"))
        stop("group_by contains unknown column(s) in derived heading table")
    }
    if (length(group_by) == 1L && group_by == "id") {
      hd$..grp <- hd$id
    } else {
      key <- interaction(hd[, group_by, drop = FALSE], drop = TRUE, lex.order = TRUE)
      hd$..grp <- key
    }
  }

  sp <- split(hd$.uc_heading, hd$..grp)
  res <- lapply(names(sp), function(g) {
    th <- sp[[g]]
    th <- th[is.finite(th)]
    if (!length(th)) return(data.frame(mean_dir = NA_real_, resultant_R = NA_real_, kappa = NA_real_, n = 0L))
    tc  <- .as_circ(th)
    mu  <- circular::mean.circular(tc, na.rm = TRUE)
    R   <- circular::rho.circular(tc,  na.rm = TRUE)
    kap <- .est_kappa_safe(tc, fallback = .kappa_from_Rbar(as.numeric(R)))
    uc_mu <- .wrap_to_2pi(as.numeric(mu))
    out_mu <- if (angle_convention == "clock") .uc_to_clock(uc_mu, coords) else uc_mu
    data.frame(.grp = g, mean_dir = out_mu, resultant_R = as.numeric(R),
               kappa = as.numeric(kap), n = length(th))
  })
  res <- do.call(rbind, res)

  # restore grouping column names (unchanged logic)
  if (is.null(group_by)) {
    res$group <- "all"; res$.grp <- NULL
  } else if (length(group_by) == 1L && group_by == "id") {
    names(res)[names(res) == ".grp"] <- "id"
  } else {
    parts <- do.call(rbind, strsplit(as.character(res$.grp), split = "\\."))
    parts <- as.data.frame(parts, stringsAsFactors = FALSE)
    names(parts) <- group_by
    res <- cbind(parts, res[, setdiff(names(res), ".grp"), drop = FALSE])
  }
  rownames(res) <- NULL
  attr(res, "angle_convention") <- angle_convention
  attr(res, "coords")           <- coords
  res
}
```

- [ ] **Step 4: Update circ_summary() TrajSet method in R/circular_statistics.R**

Update the `setGeneric("circ_summary", ...)` (line 31) and `setMethod("circ_summary", "TrajSet", ...)` (line 47):

```r
setGeneric("circ_summary", function(x, w = NULL, by = c("id","global"),
                                    angle_convention = c("unit_circle","clock"))
  standardGeneric("circ_summary"))

setMethod("circ_summary", "TrajSet", function(x, w = NULL, by = c("id","global"),
                                              angle_convention = c("unit_circle","clock")) {
  by               <- match.arg(by)
  angle_convention <- match.arg(angle_convention)
  id <- x@cols$id; tm <- x@cols$time; th <- x@cols$angle
  wcol <- if (is.null(w)) x@cols$weight else w
  d <- x@data
  split_idx <- if (by == "id") split(seq_len(nrow(d)), d[[id]]) else list(all = seq_len(nrow(d)))

  rows <- lapply(names(split_idx), function(k) {
    ii <- split_idx[[k]]
    theta <- d[[th]][ii]
    wts <- if (!is.null(wcol) && wcol %in% names(d)) d[[wcol]][ii] else NULL

    valid <- !is.na(theta)
    if (!is.null(wts)) valid <- valid & !is.na(wts)
    theta_valid <- theta[valid]
    wts_valid <- if (!is.null(wts)) wts[valid] else NULL

    tc <- circular::circular(theta_valid, units = "radians", modulo = "2pi")
    mu <- if (length(theta_valid)) {
      circular::mean.circular(tc, na.rm = TRUE, weights = wts_valid)
    } else {
      NA_real_
    }

    if (length(theta_valid)) {
      if (is.null(wts_valid)) {
        mean_cos <- mean(cos(theta_valid)); mean_sin <- mean(sin(theta_valid))
      } else {
        w_norm <- wts_valid / sum(wts_valid)
        mean_cos <- sum(w_norm * cos(theta_valid)); mean_sin <- sum(w_norm * sin(theta_valid))
      }
      R <- sqrt(mean_cos^2 + mean_sin^2)
    } else {
      R <- NA_real_
    }

    kap <- .est_kappa_safe(tc, fallback = NA_real_, w = wts_valid)
    uc_mu  <- .wrap_to_2pi(as.numeric(mu))
    out_mu <- if (angle_convention == "clock") (pi/2 - uc_mu) %% (2*pi) else uc_mu
    data.frame(
      id          = if (by == "id") k else "global",
      n           = sum(!is.na(theta)),
      t_start     = d[[tm]][ii[1]],
      t_end       = d[[tm]][ii[length(ii)]],
      mean_dir    = out_mu,
      resultant_R = as.numeric(R),
      kappa       = as.numeric(kap),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
})
```

- [ ] **Step 5: Run tests — expect pass**

```r
testthat::test_file("tests/testthat/test-headings.R")
testthat::test_file("tests/testthat/test-circular-stats.R")
```

Expected: all new and existing tests PASS.

- [ ] **Step 6: Commit**

```bash
git add R/headings.R R/circular_statistics.R tests/testthat/test-headings.R tests/testthat/test-circular-stats.R
git commit -m "feat: add angle_convention param to circ_summary_headings() and circ_summary()"
```

---

## Task 5: circ_mean_segments() — auto-detect convention from attr

**Files:**
- Modify: `R/headings.R`
- Test: `tests/testthat/test-circular-plotting.R`

### Background

`circ_mean_segments()` currently computes:
```r
xend <- x0 + scale * stats_df$resultant_R * cos(stats_df$mean_dir)
yend <- y0 + scale * stats_df$resultant_R * sin(stats_df$mean_dir)
```

When `attr(stats_df, "angle_convention") == "clock"`, `mean_dir` is in clock space. To draw correctly on the unit-circle Cartesian plot, convert back via `rad_unclock(mean_dir)` (available from `circular_mapping.R` namespace).

- [ ] **Step 1: Write the failing tests**

Add to `tests/testthat/test-circular-plotting.R`:

```r
test_that("circ_mean_segments with clock attr draws arrow pointing North for mean_dir=0", {
  # mean_dir = 0 in clock convention = North = (0, 1) in Cartesian
  stats_df <- data.frame(mean_dir = 0, resultant_R = 1)
  attr(stats_df, "angle_convention") <- "clock"
  seg <- circ_mean_segments(stats_df)
  expect_equal(seg$xend, 0, tolerance = 1e-10)
  expect_equal(seg$yend, 1, tolerance = 1e-10)
})

test_that("circ_mean_segments with clock attr draws arrow pointing East for mean_dir=pi/2", {
  # mean_dir = pi/2 in absolute clock = East = (1, 0) in Cartesian
  stats_df <- data.frame(mean_dir = pi / 2, resultant_R = 1)
  attr(stats_df, "angle_convention") <- "clock"
  seg <- circ_mean_segments(stats_df)
  expect_equal(seg$xend, 1, tolerance = 1e-10)
  expect_equal(seg$yend, 0, tolerance = 1e-10)
})

test_that("circ_mean_segments without clock attr is unchanged", {
  stats_df <- data.frame(mean_dir = pi / 3, resultant_R = 0.7)
  seg <- circ_mean_segments(stats_df)
  expect_equal(seg$xend, 0.7 * cos(pi / 3), tolerance = 1e-10)
  expect_equal(seg$yend, 0.7 * sin(pi / 3), tolerance = 1e-10)
})
```

- [ ] **Step 2: Run tests to verify they fail**

```r
testthat::test_file("tests/testthat/test-circular-plotting.R")
```

Expected: the two new clock tests FAIL (wrong xend/yend); the unchanged test PASSES.

- [ ] **Step 3: Update circ_mean_segments() in R/headings.R**

Replace the current `circ_mean_segments` body (lines 608–613):

```r
circ_mean_segments <- function(stats_df, x0 = 0, y0 = 0, scale = 1) {
  stopifnot(all(c("mean_dir","resultant_R") %in% names(stats_df)))
  convention <- attr(stats_df, "angle_convention") %||% "unit_circle"
  uc_dir <- if (convention == "clock") {
    rad_unclock(stats_df$mean_dir)
  } else {
    stats_df$mean_dir
  }
  xend <- x0 + scale * stats_df$resultant_R * cos(uc_dir)
  yend <- y0 + scale * stats_df$resultant_R * sin(uc_dir)
  cbind(stats_df, x = x0, y = y0, xend = xend, yend = yend)
}
```

- [ ] **Step 4: Run tests — expect pass**

```r
testthat::test_file("tests/testthat/test-circular-plotting.R")
```

Expected: all new and existing tests PASS.

- [ ] **Step 5: Run full test suite**

```r
devtools::test()
```

Expected: all tests PASS.

- [ ] **Step 6: Commit**

```bash
git add R/headings.R tests/testthat/test-circular-plotting.R
git commit -m "feat: circ_mean_segments auto-detects clock convention via attr"
```

---

## Task 6: plividus dataset — register rel_x/rel_y in @cols

**Files:**
- Modify: `data/plividus.rda`, `R/data.R`

### Background

The `plividus` TrajSet already has `rel_x` and `rel_y` columns in `@data` (confirmed by inspection). The `@cols` list just doesn't include them. We rebuild the .rda by loading the existing object, patching `@cols`, and saving.

- [ ] **Step 1: Run the rebuild script**

From an R session in the package root:

```r
load("data/plividus.rda")

# Verify rel_x/rel_y exist in the data
stopifnot("rel_x" %in% names(plividus@data))
stopifnot("rel_y" %in% names(plividus@data))

# Patch @cols
plividus@cols$rel_x <- "rel_x"
plividus@cols$rel_y <- "rel_y"

# Validate
methods::validObject(plividus)

# Save
save(plividus, file = "data/plividus.rda", compress = "xz")
cat("Done. cols:", paste(names(plividus@cols), collapse=", "), "\n")
```

Expected output:
```
Done. cols: id, time, angle, x, y, rel_x, rel_y, raw_x, raw_y, rho, weight
```

- [ ] **Step 2: Verify the saved object**

```r
load("data/plividus.rda")
stopifnot(plividus@cols$rel_x == "rel_x")
stopifnot(plividus@cols$rel_y == "rel_y")
cat("rel_x registered:", plividus@cols$rel_x, "\n")
```

Expected:
```
rel_x registered: rel_x
```

- [ ] **Step 3: Update R/data.R documentation**

In `R/data.R`, update the `@format` section to document the `rel_x`/`rel_y` col pointers in `@cols`:

After the existing `\item{arc}{...}` entry, add:

```
#'   }
#'   \describe{
#'     \item{cols$rel_x, cols$rel_y}{Registered pointers to the \code{rel_x}
#'       and \code{rel_y} columns, enabling \code{derive_headings(..., coords = "relative")}.}
```

Or more simply, update the `@format` prose at the top to note the col pointers:

```r
#' @format A [`TrajSet`] object with 134 375 rows. Data columns:
#'   \describe{
#'     \item{trial_id}{Character. Unique trial identifier.}
#'     \item{frame}{Numeric. Video frame number.}
#'     \item{trans_x, trans_y}{Numeric. Unit-circle Cartesian coordinates.}
#'     \item{abs_theta}{Numeric. Absolute bearing (radians, −π to π).}
#'     \item{rel_theta}{Numeric. Bearing relative to stimulus direction (radians, −π to π).}
#'     \item{rel_x, rel_y}{Numeric. Stimulus-relative unit-circle Cartesian coordinates.
#'       Registered in \code{@@cols} so \code{derive_headings(..., coords = "relative")} works directly.}
#'     \item{arc}{Ordered factor. Stimulus arc angle in degrees.}
#'   }
```

- [ ] **Step 4: Regenerate docs and verify**

```r
devtools::document()
```

Expected: no errors; `man/plividus.Rd` updated.

- [ ] **Step 5: Run full test suite**

```r
devtools::test()
```

Expected: all tests PASS.

- [ ] **Step 6: Commit**

```bash
git add data/plividus.rda R/data.R man/plividus.Rd
git commit -m "feat: register rel_x/rel_y in plividus@cols for relative heading extraction"
```

---

## Task 7: Vignette — per-condition arrows with relative + clock convention

**Files:**
- Modify: `vignettes/radiatR.Rmd`

### Background

The current vignette computes per-condition mean arrows on absolute headings. Switch to `coords = "relative", angle_convention = "clock"` so the arrows show stimulus-relative directionality (stimulus at top = 0°).

The relevant chunk is `arc-panels` (around line 132). The grand-mean chunk (`headings-overlay`, around line 96) can also be updated to use relative+clock.

- [ ] **Step 1: Update the headings chunk**

Find the `{r headings}` chunk (around line 80) and update the `derive_headings` call:

```r
hd <- derive_headings(plividus, rule = "crossing",
                      circ0 = 0.2, circ1 = 0.4,
                      coords = "relative",
                      angle_convention = "clock",
                      return_coords = TRUE)
```

- [ ] **Step 2: Update the headings-overlay chunk**

The `summ_all` computation (around line 97) uses raw `hd$heading`. Since headings are now in clock convention, use `circ_mean_segments` directly (it auto-detects via attr from `circ_summary_headings`). Replace the manual `atan2/sqrt` block with:

```r
summ_all <- circ_summary_headings(plividus, rule = "crossing",
                                  circ0 = 0.2, circ1 = 0.4,
                                  group_by = NULL,
                                  coords = "relative",
                                  angle_convention = "clock")
seg_all <- circ_mean_segments(summ_all)
```

- [ ] **Step 3: Update the arc-panels chunk**

The `hd` data frame from Step 1 already has the `arc` column carried through (since `arc` exists in `plividus@data`; confirm by adding `carry = "arc"` to the `derive_headings` call in Step 1). Replace the manual `arc_summ` computation (around line 133) with:

```r
arc_summ <- do.call(rbind, lapply(split(hd, hd$arc), function(sub) {
  # Reverse relative clock → UC so we can compute a proper circular mean.
  # Relative clock: clock = (-uc) %% 2pi, so uc = (-clock) %% 2pi.
  h_uc <- (-sub$heading) %% (2 * pi)
  uc_mean <- atan2(mean(sin(h_uc), na.rm = TRUE),
                   mean(cos(h_uc), na.rm = TRUE)) %% (2 * pi)
  data.frame(
    arc         = sub$arc[1],
    mean_dir    = (-uc_mean) %% (2 * pi),  # back to relative clock
    resultant_R = sqrt(mean(cos(h_uc), na.rm = TRUE)^2 +
                       mean(sin(h_uc), na.rm = TRUE)^2)
  )
}))
attr(arc_summ, "angle_convention") <- "clock"
attr(arc_summ, "coords")           <- "relative"
segs <- circ_mean_segments(arc_summ)
```

- [ ] **Step 4: Update the prose**

Update the sentence after the `arc-panels` chunk that mentions heading values in degrees, since the convention has changed. Change any reference to "absolute heading" to "stimulus-relative heading (0° = toward stimulus)".

- [ ] **Step 5: Build the vignette and check for errors**

```r
devtools::build_vignettes()
```

Expected: no errors or warnings.

- [ ] **Step 6: Commit**

```bash
git add vignettes/radiatR.Rmd
git commit -m "docs: update vignette to use relative coords and clock convention for mean arrows"
```

---

## Task 8: Final documentation and R CMD CHECK

**Files:**
- Modify: `R/headings.R` (Roxygen tags)

- [ ] **Step 1: Update Roxygen @param for derive_headings()**

In `R/headings.R`, update the `#' @param` block for `derive_headings` (before `setGeneric`):

```r
#' @param coords Character. Which Cartesian columns to use: `"absolute"` (default,
#'   uses `x`/`y` from `TrajSet@cols`) or `"relative"` (uses `rel_x`/`rel_y`;
#'   errors if not registered).
#' @param angle_convention Character. Output angle convention: `"clock"` (default;
#'   0 = North/top, clockwise) or `"unit_circle"` (0 = East, counterclockwise).
#'   The output data frame carries `attr(result, "angle_convention")` and
#'   `attr(result, "coords")` for downstream auto-detection.
```

- [ ] **Step 2: Update Roxygen @param for circ_summary_headings()**

Add to the existing Roxygen block for `circ_summary_headings`:

```r
#' @param angle_convention Character. Output convention for `mean_dir`:
#'   `"clock"` (0 = North, clockwise) or `"unit_circle"` (0 = East,
#'   counterclockwise). Auto-detected from the headings `attr` when not supplied.
#'   Default `"unit_circle"` for backward compatibility.
```

- [ ] **Step 3: Rebuild documentation**

```r
devtools::document()
```

Expected: no errors.

- [ ] **Step 4: Run full R CMD CHECK**

```r
devtools::check()
```

Expected: 0 errors, 0 warnings. Notes about `rad_unclock` being internal are acceptable.

- [ ] **Step 5: Push branch and open PR**

```bash
git push -u origin feat/coord-convention
```

Then open a PR with summary:
- Adds `rel_x`/`rel_y` pointer fields to `TrajSet@cols`
- Adds `coords` + `angle_convention` params to `derive_headings()`, defaulting to `"absolute"` + `"clock"` (new default)
- Adds `angle_convention` to `circ_summary_headings()` and `circ_summary()`
- `circ_mean_segments()` auto-detects clock convention via attr
- Rebuilds `plividus` with `rel_x`/`rel_y` registered
- Updates vignette to use relative + clock for per-condition arrows

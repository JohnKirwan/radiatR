# Clock Display Convention Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make `radiate()` and heading overlay layers display stimulus direction at the top (12 o'clock / North) instead of East, by propagating a "clock" display convention through TrajSet metadata and heading data frame attributes.

**Architecture:** A 90° CCW rotation helper `.to_clock_display(x, y) = (-y, x)` is applied in each rendering function when it detects `display_convention = "clock"`. The convention travels through two channels: `ts@meta` (for `radiate()` column selection and rotation) and `attr(hd, "display_convention")` (for heading overlay layers). `derive_headings()` sets the attribute when `coords = "relative"`; `compute_*()` functions propagate it. `get_tracked_object_pos()` sets the three meta keys so `radiate(plividus)` automatically uses relative coordinates and applies the rotation.

**Tech Stack:** R, S4 (TrajSet), ggplot2, testthat, devtools

> **Line number note:** All line numbers in this plan reflect the state of each file *before any task runs*. Tasks 1–6 each add lines to `R/circular_plotting.R`, so by Task 7 the line numbers will have shifted by ~35 lines. Use the exact code strings shown in each **Background** block to locate the right place — do not rely on line numbers alone.

---

## File Map

| File | Change |
|------|--------|
| `R/circular_plotting.R` | Add `.to_clock_display()` helper; update `compute_circ_interval()`, `add_circ_interval()`, `compute_circ_mean()`, `add_circ_mean()`, `add_heading_points()`, `add_heading_vectors()`, `radiate()` |
| `R/headings.R` | `derive_headings()` TrajSet method: set `display_convention` attr when `coords = "relative"` |
| `R/circular_trials.R` | `get_tracked_object_pos()`: set three `@meta` keys |
| `tests/testthat/test-circular-plotting.R` | New tests for helper, rotated layers, `radiate()` integration |
| `tests/testthat/test-headings.R` | New tests for `derive_headings()` attribute |
| `tests/testthat/test-circular-trials.R` | New test for `get_tracked_object_pos()` meta keys |

---

### Task 1: `.to_clock_display()` helper and `derive_headings()` attribute

This task adds the rotation primitive and wires up the first propagation channel: `derive_headings()` stamps the `display_convention` attribute on heading data frames when `coords = "relative"`.

**Files:**
- Modify: `R/circular_plotting.R` — insert before the `# ---- circular interval arc ----` section header (line 642)
- Modify: `R/headings.R:564-566` — TrajSet `derive_headings` method, end of body
- Test: `tests/testthat/test-circular-plotting.R` — append at end of file (after line 931)
- Test: `tests/testthat/test-headings.R` — append at end of file (after line 318)

**Background:** `R/circular_plotting.R` line 641 is a blank line between a closing `}` and the `# ---- circular interval arc ----` comment at line 642. Insert the new section there. `R/headings.R` lines 564–566 read:
```r
  attr(res, "angle_convention") <- angle_convention
  attr(res, "coords")           <- coords
  res
})
```
Insert the new attribute line between line 565 and 566 (between the `coords` attr assignment and the bare `res`).

- [ ] **Step 1: Write failing tests for `.to_clock_display()`**

Append to `tests/testthat/test-circular-plotting.R` (after the closing `})` of the last test at line 931):

```r
# ---- clock display convention: .to_clock_display helper ---------------------

test_that(".to_clock_display maps (1,0) to (0,1)", {
  r <- .to_clock_display(1, 0)
  expect_equal(r$x, 0)
  expect_equal(r$y, 1)
})

test_that(".to_clock_display maps (0,1) to (-1,0)", {
  r <- .to_clock_display(0, 1)
  expect_equal(r$x, -1)
  expect_equal(r$y,  0)
})

test_that(".to_clock_display maps (-1,0) to (0,-1)", {
  r <- .to_clock_display(-1, 0)
  expect_equal(r$x,  0)
  expect_equal(r$y, -1)
})

test_that(".to_clock_display maps (0,-1) to (1,0)", {
  r <- .to_clock_display(0, -1)
  expect_equal(r$x, 1)
  expect_equal(r$y, 0)
})
```

- [ ] **Step 2: Write failing tests for `derive_headings()` attribute**

Append to `tests/testthat/test-headings.R` (after line 318, the end of the file):

```r
# ---- display_convention attribute --------------------------------------------

test_that("derive_headings sets display_convention='clock' when coords='relative'", {
  ts <- make_ts_with_rel()
  hd <- derive_headings(ts, rule = "crossing", circ0 = 0.2, circ1 = 0.4,
                        coords = "relative", angle_convention = "unit_circle")
  expect_equal(attr(hd, "display_convention"), "clock")
})

test_that("derive_headings does not set display_convention when coords='absolute'", {
  ts <- make_ts_with_rel()
  hd <- derive_headings(ts, rule = "crossing", circ0 = 0.2, circ1 = 0.4,
                        coords = "absolute", angle_convention = "unit_circle")
  expect_null(attr(hd, "display_convention"))
})
```

`make_ts_with_rel()` is defined at line 201 of `test-headings.R` and builds a TrajSet with both `x/y` (absolute) and `rx/ry` (relative) columns registered.

- [ ] **Step 3: Run tests to verify they fail**

```bash
Rscript -e "devtools::test(filter = 'circular-plotting|headings')" 2>&1 | grep -E "FAIL|ERROR|\.to_clock_display|display_convention" | head -20
```

Expected: errors about `.to_clock_display` not found and attribute tests failing.

- [ ] **Step 4: Add `.to_clock_display()` to `R/circular_plotting.R`**

Insert the following block immediately before the `# ---- circular interval arc ----` line (currently line 642). The blank line at 641 becomes part of the new block:

```r
# ---- display helpers ---------------------------------------------------------

.to_clock_display <- function(x, y) list(x = -y, y = x)

# ---- circular interval arc ---------------------------------------------------
```

(Remove the original blank line 641 and the original `# ---- circular interval arc ----` line 642; they are replaced by the above four lines.)

- [ ] **Step 5: Add `display_convention` attribute in `derive_headings()` TrajSet method**

In `R/headings.R`, the TrajSet method body ends with (around lines 564–566):
```r
  attr(res, "angle_convention") <- angle_convention
  attr(res, "coords")           <- coords
  res
```

Insert one line between the `coords` assignment and the bare `res`:

```r
  attr(res, "angle_convention") <- angle_convention
  attr(res, "coords")           <- coords
  if (coords == "relative") attr(res, "display_convention") <- "clock"
  res
```

- [ ] **Step 6: Run tests to verify they pass**

```bash
Rscript -e "devtools::test(filter = 'circular-plotting|headings')" 2>&1 | tail -20
```

Expected: all tests pass. Confirm the four `.to_clock_display` tests and two `display_convention` tests show `[ PASS ]`.

- [ ] **Step 7: Commit**

```bash
git add R/circular_plotting.R R/headings.R \
        tests/testthat/test-circular-plotting.R tests/testthat/test-headings.R
git commit -m "feat: add .to_clock_display helper; derive_headings stamps display_convention attr"
```

---

### Task 2: Attribute propagation in `compute_circ_mean()` and `compute_circ_interval()`

Both compute functions must pass the `display_convention` attribute from their `headings_df` input to their output data frame, so the downstream `add_*` functions can read it.

**Files:**
- Modify: `R/circular_plotting.R:731–736` — `compute_circ_interval()` return block
- Modify: `R/circular_plotting.R:943–947` — `compute_circ_mean()` return block
- Test: `tests/testthat/test-circular-plotting.R` — add within the `compute_circ_interval` and `compute_circ_mean` sections

**Background:** Both functions end with the same pattern:
```r
  out <- do.call(rbind, out_list)
  if (use_colour && ...) out[[colour_col]] <- factor(...)
  out
}
```
Add `attr(out, "display_convention") <- attr(headings_df, "display_convention")` before the final `out`.

- [ ] **Step 1: Write failing tests**

In `tests/testthat/test-circular-plotting.R`, locate the `# ---- compute_circ_interval ---` section (which starts around line 612). Add two tests immediately before the `# ---- add_circ_interval ----` section header that follows it:

```r
test_that("compute_circ_interval propagates display_convention='clock'", {
  hd <- data.frame(heading = c(0, pi / 4, pi / 2, pi))
  attr(hd, "display_convention") <- "clock"
  result <- compute_circ_interval(hd, stat = "sd")
  expect_equal(attr(result, "display_convention"), "clock")
})

test_that("compute_circ_interval leaves display_convention NULL when absent", {
  hd <- data.frame(heading = c(0, pi / 4, pi / 2, pi))
  result <- compute_circ_interval(hd, stat = "sd")
  expect_null(attr(result, "display_convention"))
})
```

Also add tests within the `# ---- compute_circ_mean ----` section, immediately before the `# ---- add_circ_mean ----` header:

```r
test_that("compute_circ_mean propagates display_convention='clock'", {
  hd <- data.frame(heading = c(0, pi / 2, pi))
  attr(hd, "display_convention") <- "clock"
  result <- suppressMessages(compute_circ_mean(hd))
  expect_equal(attr(result, "display_convention"), "clock")
})

test_that("compute_circ_mean leaves display_convention NULL when absent", {
  hd <- data.frame(heading = c(0, pi / 2, pi))
  result <- suppressMessages(compute_circ_mean(hd))
  expect_null(attr(result, "display_convention"))
})
```

(`suppressMessages` suppresses the "defaulting to unit_circle" message since we're not setting `angle_convention` on `hd`.)

- [ ] **Step 2: Run tests to verify they fail**

```bash
Rscript -e "devtools::test(filter = 'circular-plotting')" 2>&1 | grep -E "FAIL|ERROR|propagates" | head -10
```

Expected: 4 new tests fail.

- [ ] **Step 3: Add attribute propagation to `compute_circ_interval()`**

In `R/circular_plotting.R`, the `compute_circ_interval()` body ends around lines 731–736:
```r
  out <- do.call(rbind, out_list)
  if (use_colour && is.factor(headings_df[[colour_col]]))
    out[[colour_col]] <- factor(out[[colour_col]],
                                levels = levels(headings_df[[colour_col]]))
  out
}
```

Replace those last two lines (`  out` and `}`) with:
```r
  attr(out, "display_convention") <- attr(headings_df, "display_convention")
  out
}
```

- [ ] **Step 4: Add attribute propagation to `compute_circ_mean()`**

The `compute_circ_mean()` body ends around lines 943–947:
```r
  out <- do.call(rbind, out_list)
  if (use_colour && is.factor(headings_df[[colour_col]]))
    out[[colour_col]] <- factor(out[[colour_col]], levels = levels(headings_df[[colour_col]]))
  out
}
```

Replace the last `  out` and `}` with:
```r
  attr(out, "display_convention") <- attr(headings_df, "display_convention")
  out
}
```

- [ ] **Step 5: Run tests to verify they pass**

```bash
Rscript -e "devtools::test(filter = 'circular-plotting')" 2>&1 | tail -20
```

Expected: all tests pass.

- [ ] **Step 6: Commit**

```bash
git add R/circular_plotting.R tests/testthat/test-circular-plotting.R
git commit -m "feat: propagate display_convention attr through compute_circ_mean/interval"
```

---

### Task 3: `add_heading_points()` clock rotation

When `display_convention = "clock"`, marker positions rotate 90° CCW: East (1, 0) → North (0, 1).

**Files:**
- Modify: `R/circular_plotting.R:1091–1092` — inside `add_heading_points()`
- Test: `tests/testthat/test-circular-plotting.R` — append after the `# ---- add_heading_points / vectors endpoint arithmetic ----` section (after line 128)

**Background:** Lines 1091–1092 compute the marker position:
```r
  headings_df[[".x_head"]] <- cos(headings_df$heading)
  headings_df[[".y_head"]] <- sin(headings_df$heading)
```
In clock display, `(cos θ, sin θ)` is transformed to `(-sin θ, cos θ)`.

- [ ] **Step 1: Write failing test**

After the test ending at line 128 (`add_heading_vectors segments run from…`), add:

```r
test_that("add_heading_points with display_convention='clock' rotates marker 90 CCW", {
  library(ggplot2)
  hd <- data.frame(heading = 0)          # heading 0 = East in UC
  attr(hd, "display_convention") <- "clock"
  p     <- ggplot() + add_heading_points(hd)
  built <- ggplot_build(p)
  pts   <- built$data[[1]]
  # East (1,0) rotated 90 CCW -> North (0,1)
  expect_equal(pts$x, 0, tolerance = 1e-6)
  expect_equal(pts$y, 1, tolerance = 1e-6)
})
```

- [ ] **Step 2: Run test to verify it fails**

```bash
Rscript -e "devtools::test(filter = 'circular-plotting')" 2>&1 | grep -E "FAIL|rotates marker" | head -5
```

Expected: 1 new test fails.

- [ ] **Step 3: Implement rotation in `add_heading_points()`**

In `R/circular_plotting.R`, replace lines 1091–1092:
```r
  headings_df[[".x_head"]] <- cos(headings_df$heading)
  headings_df[[".y_head"]] <- sin(headings_df$heading)
```

With:
```r
  if (identical(attr(headings_df, "display_convention"), "clock")) {
    disp <- .to_clock_display(cos(headings_df$heading), sin(headings_df$heading))
    headings_df[[".x_head"]] <- disp$x
    headings_df[[".y_head"]] <- disp$y
  } else {
    headings_df[[".x_head"]] <- cos(headings_df$heading)
    headings_df[[".y_head"]] <- sin(headings_df$heading)
  }
```

- [ ] **Step 4: Run tests to verify they pass**

```bash
Rscript -e "devtools::test(filter = 'circular-plotting')" 2>&1 | tail -20
```

Expected: all tests pass. Confirm the existing `add_heading_points places markers at (cos(h), sin(h))` test still passes (no `display_convention` set → non-clock path).

- [ ] **Step 5: Commit**

```bash
git add R/circular_plotting.R tests/testthat/test-circular-plotting.R
git commit -m "feat: add_heading_points applies 90 CCW rotation for clock display convention"
```

---

### Task 4: `add_heading_vectors()` clock rotation

When `display_convention = "clock"`, rotate both the segment start (`x_inner`/`y_inner`) and the segment end (the heading endpoint).

**Files:**
- Modify: `R/circular_plotting.R:1135–1163` — `add_heading_vectors()` body (replace the coordinate computation and mapping)
- Test: `tests/testthat/test-circular-plotting.R` — append after the Task 3 test

**Background:** The current body builds `.x_head`/`.y_head` from `cos`/`sin`, then maps `x = .data$x_inner, y = .data$y_inner`. In clock mode, both start and end points must be rotated. We introduce `.x_inner`/`.y_inner` temp columns and update the mapping.

The current function (lines 1135–1163) is:
```r
add_heading_vectors <- function(headings_df, colour_col = NULL, colour = "black",
                               linetype = "dotted") {
  required <- c("heading", "x_inner", "y_inner")
  missing_cols <- setdiff(required, names(headings_df))
  if (length(missing_cols)) {
    stop(sprintf(
      paste0("`headings_df` is missing columns: %s. ",
             "Call derive_headings(rule = 'crossing', return_coords = TRUE)."),
      paste(missing_cols, collapse = ", ")
    ))
  }

  headings_df[[".x_head"]] <- cos(headings_df$heading)
  headings_df[[".y_head"]] <- sin(headings_df$heading)

  mapping <- ggplot2::aes(
    x    = .data$x_inner,
    y    = .data$y_inner,
    xend = .data[[".x_head"]],
    yend = .data[[".y_head"]]
  )
  use_fixed_colour <- is.null(colour_col) || !colour_col %in% names(headings_df)
  if (!use_fixed_colour) mapping[["colour"]] <- rlang::sym(colour_col)

  args <- list(data = headings_df, mapping = mapping,
               linetype = linetype, inherit.aes = FALSE)
  if (use_fixed_colour) args$colour <- colour
  do.call(ggplot2::geom_segment, args)
}
```

- [ ] **Step 1: Write failing test**

Append after the Task 3 test:

```r
test_that("add_heading_vectors with display_convention='clock' rotates both endpoints 90 CCW", {
  library(ggplot2)
  # heading=0 (East), inner crossing at (0.5, 0)
  hd <- data.frame(heading = 0, x_inner = 0.5, y_inner = 0)
  attr(hd, "display_convention") <- "clock"
  p     <- ggplot() + add_heading_vectors(hd)
  built <- ggplot_build(p)
  seg   <- built$data[[1]]
  # start (0.5, 0) rotated -> (0, 0.5)
  # end   (1,   0) rotated -> (0, 1)
  expect_equal(seg$x,    0,   tolerance = 1e-6)
  expect_equal(seg$y,    0.5, tolerance = 1e-6)
  expect_equal(seg$xend, 0,   tolerance = 1e-6)
  expect_equal(seg$yend, 1,   tolerance = 1e-6)
})
```

- [ ] **Step 2: Run test to verify it fails**

```bash
Rscript -e "devtools::test(filter = 'circular-plotting')" 2>&1 | grep -E "FAIL|rotates both" | head -5
```

Expected: 1 new test fails.

- [ ] **Step 3: Implement rotation in `add_heading_vectors()`**

Replace the full body of `add_heading_vectors` (lines 1135–1163) with:

```r
add_heading_vectors <- function(headings_df, colour_col = NULL, colour = "black",
                               linetype = "dotted") {
  required <- c("heading", "x_inner", "y_inner")
  missing_cols <- setdiff(required, names(headings_df))
  if (length(missing_cols)) {
    stop(sprintf(
      paste0("`headings_df` is missing columns: %s. ",
             "Call derive_headings(rule = 'crossing', return_coords = TRUE)."),
      paste(missing_cols, collapse = ", ")
    ))
  }

  headings_df[[".x_head"]] <- cos(headings_df$heading)
  headings_df[[".y_head"]] <- sin(headings_df$heading)

  if (identical(attr(headings_df, "display_convention"), "clock")) {
    disp_end   <- .to_clock_display(headings_df[[".x_head"]], headings_df[[".y_head"]])
    disp_start <- .to_clock_display(headings_df$x_inner,     headings_df$y_inner)
    headings_df[[".x_head"]]  <- disp_end$x
    headings_df[[".y_head"]]  <- disp_end$y
    headings_df[[".x_inner"]] <- disp_start$x
    headings_df[[".y_inner"]] <- disp_start$y
  } else {
    headings_df[[".x_inner"]] <- headings_df$x_inner
    headings_df[[".y_inner"]] <- headings_df$y_inner
  }

  mapping <- ggplot2::aes(
    x    = .data[[".x_inner"]],
    y    = .data[[".y_inner"]],
    xend = .data[[".x_head"]],
    yend = .data[[".y_head"]]
  )
  use_fixed_colour <- is.null(colour_col) || !colour_col %in% names(headings_df)
  if (!use_fixed_colour) mapping[["colour"]] <- rlang::sym(colour_col)

  args <- list(data = headings_df, mapping = mapping,
               linetype = linetype, inherit.aes = FALSE)
  if (use_fixed_colour) args$colour <- colour
  do.call(ggplot2::geom_segment, args)
}
```

Note: the mapping now uses `.data[[".x_inner"]]`/`.data[[".y_inner"]]` (temp columns) instead of `.data$x_inner`/`.data$y_inner`. In the non-clock branch, `.x_inner = x_inner`, so existing tests are unaffected.

- [ ] **Step 4: Run tests to verify they pass**

```bash
Rscript -e "devtools::test(filter = 'circular-plotting')" 2>&1 | tail -20
```

Expected: all tests pass. Confirm the existing `add_heading_vectors segments run from (x_inner,y_inner) to (cos(h),sin(h))` test still passes.

- [ ] **Step 5: Commit**

```bash
git add R/circular_plotting.R tests/testthat/test-circular-plotting.R
git commit -m "feat: add_heading_vectors applies 90 CCW rotation for clock display convention"
```

---

### Task 5: `add_circ_mean()` clock rotation

When `display_convention = "clock"`, the mean-direction arrow endpoint uses `(-R·sin θ, R·cos θ)` instead of `(R·cos θ, R·sin θ)`.

**Files:**
- Modify: `R/circular_plotting.R:1000–1003` — inside `add_circ_mean()`, the `.xend`/`.yend` computation
- Test: `tests/testthat/test-circular-plotting.R` — append within the `# ---- add_circ_mean ----` section

**Background:** Lines 1000–1003:
```r
  summary_df$.x    <- 0
  summary_df$.y    <- 0
  summary_df$.xend <- summary_df$resultant_R * cos(summary_df$mean_dir)
  summary_df$.yend <- summary_df$resultant_R * sin(summary_df$mean_dir)
```
In clock mode: `.to_clock_display(R·cos θ, R·sin θ) = (-R·sin θ, R·cos θ)`.

- [ ] **Step 1: Write failing test**

In the `# ---- add_circ_mean ----` section of `test-circular-plotting.R`, append after the last test in that section (currently ending around line 891, just before `# ---- add_heading_arrow ----`):

```r
test_that("add_circ_mean with display_convention='clock' rotates arrow 90 CCW", {
  library(ggplot2)
  # mean_dir=0 (East in UC), R=0.8
  # Clock display: endpoint = (-R*sin(0), R*cos(0)) = (0, 0.8) -> North
  sm <- data.frame(mean_dir = 0, resultant_R = 0.8)
  attr(sm, "display_convention") <- "clock"
  built <- ggplot_build(ggplot() + add_circ_mean(sm))
  seg   <- built$data[[1]]
  expect_equal(seg$xend, 0,   tolerance = 1e-6)
  expect_equal(seg$yend, 0.8, tolerance = 1e-6)
})
```

- [ ] **Step 2: Run test to verify it fails**

```bash
Rscript -e "devtools::test(filter = 'circular-plotting')" 2>&1 | grep -E "FAIL|rotates arrow" | head -5
```

Expected: 1 new test fails.

- [ ] **Step 3: Implement rotation in `add_circ_mean()`**

In `R/circular_plotting.R`, replace lines 1000–1003:
```r
  summary_df$.x    <- 0
  summary_df$.y    <- 0
  summary_df$.xend <- summary_df$resultant_R * cos(summary_df$mean_dir)
  summary_df$.yend <- summary_df$resultant_R * sin(summary_df$mean_dir)
```

With:
```r
  summary_df$.x <- 0
  summary_df$.y <- 0
  if (identical(attr(summary_df, "display_convention"), "clock")) {
    summary_df$.xend <- -summary_df$resultant_R * sin(summary_df$mean_dir)
    summary_df$.yend <-  summary_df$resultant_R * cos(summary_df$mean_dir)
  } else {
    summary_df$.xend <- summary_df$resultant_R * cos(summary_df$mean_dir)
    summary_df$.yend <- summary_df$resultant_R * sin(summary_df$mean_dir)
  }
```

- [ ] **Step 4: Run tests to verify they pass**

```bash
Rscript -e "devtools::test(filter = 'circular-plotting')" 2>&1 | tail -20
```

Expected: all tests pass.

- [ ] **Step 5: Commit**

```bash
git add R/circular_plotting.R tests/testthat/test-circular-plotting.R
git commit -m "feat: add_circ_mean applies 90 CCW rotation for clock display convention"
```

---

### Task 6: `add_circ_interval()` clock rotation

When `display_convention = "clock"`, the arc polygon vertices rotate 90° CCW.

**Files:**
- Modify: `R/circular_plotting.R:797–813` — inside `add_circ_interval()`, inside the `lapply` that builds arc segments
- Test: `tests/testthat/test-circular-plotting.R` — append within the `# ---- add_circ_interval ----` section

**Background:** Inside the `parts <- lapply(valid_rows, function(i) { ... })` block (lines 797–813), a data frame `d` is built:
```r
    d <- data.frame(
      .x        = radius * cos(theta_seq),
      .y        = radius * sin(theta_seq),
      .group_id = i
    )
```
The `interval_df` variable is in scope via closure, so `attr(interval_df, "display_convention")` is accessible inside the lambda.

- [ ] **Step 1: Write failing test**

In the `# ---- add_circ_interval ----` section of `test-circular-plotting.R`, append after the last test in that section (currently ending around line 744, just before `# ---- add_heading_interval ----`):

```r
test_that("add_circ_interval with display_convention='clock' rotates arc 90 CCW", {
  library(ggplot2)
  # Degenerate arc at theta=0 (East in UC): all points at (radius, 0)
  # After clock rotation: all points at (0, radius) (North)
  iv <- data.frame(lower = 0, upper = 0)
  attr(iv, "display_convention") <- "clock"
  built <- ggplot_build(ggplot() + add_circ_interval(iv, radius = 1.0, n_theta = 3))
  pts   <- built$data[[1]]
  expect_equal(pts$x, rep(0,   3), tolerance = 1e-6)
  expect_equal(pts$y, rep(1.0, 3), tolerance = 1e-6)
})
```

- [ ] **Step 2: Run test to verify it fails**

```bash
Rscript -e "devtools::test(filter = 'circular-plotting')" 2>&1 | grep -E "FAIL|rotates arc" | head -5
```

Expected: 1 new test fails.

- [ ] **Step 3: Implement rotation in `add_circ_interval()`**

In `R/circular_plotting.R`, replace the `d <- data.frame(...)` block inside the `parts <- lapply(...)` closure (lines 806–810):
```r
    d <- data.frame(
      .x        = radius * cos(theta_seq),
      .y        = radius * sin(theta_seq),
      .group_id = i
    )
```

With:
```r
    cos_vals <- radius * cos(theta_seq)
    sin_vals <- radius * sin(theta_seq)
    if (identical(attr(interval_df, "display_convention"), "clock")) {
      disp     <- .to_clock_display(cos_vals, sin_vals)
      cos_vals <- disp$x
      sin_vals <- disp$y
    }
    d <- data.frame(
      .x        = cos_vals,
      .y        = sin_vals,
      .group_id = i
    )
```

- [ ] **Step 4: Run tests to verify they pass**

```bash
Rscript -e "devtools::test(filter = 'circular-plotting')" 2>&1 | tail -20
```

Expected: all tests pass.

- [ ] **Step 5: Commit**

```bash
git add R/circular_plotting.R tests/testthat/test-circular-plotting.R
git commit -m "feat: add_circ_interval applies 90 CCW rotation for clock display convention"
```

---

### Task 7: `get_tracked_object_pos()` meta keys and `radiate()` coordinate switch

This task wires everything together: TrajSets built from real camera data automatically use relative coordinates and clock display in `radiate()`.

**Files:**
- Modify: `R/circular_trials.R:271–276` — `get_tracked_object_pos()`, after TrajSet construction
- Modify: `R/circular_plotting.R:1563–1565` — `radiate()`, column resolution + rotation block
- Test: `tests/testthat/test-circular-trials.R` — append after the existing test (line 47)
- Test: `tests/testthat/test-circular-plotting.R` — append after the `# ---- add_heading_arrow ----` section

**Background (`R/circular_trials.R`):** `get_tracked_object_pos()` builds the TrajSet at lines 262–276:
```r
  trajset <- TrajSet(
    combined,
    id = "trial_id", time = "frame", angle = "rel_theta",
    x = "trans_x", y = "trans_y",
    angle_unit = "radians", normalize_xy = FALSE,
    meta = list(source = "get_tracked_object_pos")
  )
  trajset <- set_transform_history(trajset, transform_history)
  trajset@meta$trial_limits <- trial_limits

  trajset
```
Add the three meta key assignments after `trajset@meta$trial_limits <- trial_limits`.

**Background (`R/circular_plotting.R`):** `radiate()` resolves `x_col`/`y_col` at lines 1563–1565:
```r
  data <- ts@data
  if (!is.null(ts@cols$x) && identical(x_col, "rel_x")) x_col <- ts@cols$x
  if (!is.null(ts@cols$y) && identical(y_col, "rel_y")) y_col <- ts@cols$y
```
Replace those two `if` lines (1564–1565) with the new resolution + rotation block below.

- [ ] **Step 1: Write failing tests**

Append to `tests/testthat/test-circular-trials.R` (after the closing `})` at line 47):

```r
test_that("get_tracked_object_pos sets display convention meta keys", {
  landmarks <- data.frame(
    frame = c(1, 2, 6, 7),
    x = c(0, 1, 0, -1),
    y = c(0, 0, 0, 0)
  )
  animal_track <- data.frame(
    frame = 1:10,
    x = seq(0, -1, length.out = 10),
    y = seq(0, 0.2, length.out = 10)
  )
  file_tbl <- tibble::tibble(
    basename = "video1", arc = 0, type = "Herm", obstacle = "none", id = "animal"
  )
  limits <- suppressWarnings(get_trial_limits(landmarks, animal_track, file_tbl, vid_num = 1))
  ts     <- suppressWarnings(get_tracked_object_pos(limits, animal_track))
  expect_equal(ts@meta$display_convention, "clock")
  expect_equal(ts@meta$plot_x_col,         "rel_x")
  expect_equal(ts@meta$plot_y_col,         "rel_y")
})
```

Append to `tests/testthat/test-circular-plotting.R` (after the last test in `# ---- add_heading_arrow ----`, i.e., after the clock display tests from earlier tasks):

```r
# ---- radiate() clock display convention -------------------------------------

test_that("radiate uses meta plot_x_col/plot_y_col and rotates when display_convention='clock'", {
  library(ggplot2)
  # A TrajSet with trans_x going East but rel_x going North (toward stimulus).
  # With display_convention='clock', rel coords rotate: disp_x=-rel_y=0, disp_y=rel_x
  df <- data.frame(
    id = "A", frame = 1:3,
    trans_x   = c(0, 0.3, 0.6),   # absolute (East-going; would be wrong if used)
    trans_y   = c(0, 0,   0),
    rel_x     = c(0, 0.3, 0.6),   # relative toward stimulus (East in UC)
    rel_y     = c(0, 0,   0),
    rel_theta = rep(0, 3)
  )
  ts <- TrajSet(df, id = "id", time = "frame", x = "trans_x", y = "trans_y",
                rel_x = "rel_x", rel_y = "rel_y",
                angle = "rel_theta", angle_unit = "radians", normalize_xy = FALSE)
  ts@meta$display_convention <- "clock"
  ts@meta$plot_x_col         <- "rel_x"
  ts@meta$plot_y_col         <- "rel_y"

  p <- radiate(ts, show_arrow = FALSE, show_labels = FALSE)
  # After rotation: disp_x = -rel_y = 0, disp_y = rel_x = 0, 0.3, 0.6
  expect_true(".disp_x" %in% names(p$data))
  expect_equal(p$data$.disp_x, rep(0, 3),        tolerance = 1e-6)
  expect_equal(p$data$.disp_y, c(0, 0.3, 0.6),   tolerance = 1e-6)
})

test_that("radiate without clock meta uses @cols$x as before", {
  library(ggplot2)
  df <- data.frame(
    id = "A", frame = 1:3,
    trans_x = c(0, 0.3, 0.6), trans_y = rep(0, 3),
    rel_theta = rep(0, 3)
  )
  ts <- TrajSet(df, id = "id", time = "frame", x = "trans_x", y = "trans_y",
                angle = "rel_theta", angle_unit = "radians", normalize_xy = FALSE)
  # No meta keys set -> should fall through to @cols$x = "trans_x"
  p <- radiate(ts, show_arrow = FALSE, show_labels = FALSE)
  expect_false(".disp_x" %in% names(p$data))
})
```

- [ ] **Step 2: Run tests to verify they fail**

```bash
Rscript -e "devtools::test(filter = 'circular-plotting|circular-trials')" 2>&1 | grep -E "FAIL|ERROR|meta|display_convention" | head -10
```

Expected: 3 new tests fail (meta keys not set yet, `radiate()` rotation not yet implemented).

- [ ] **Step 3: Add meta keys in `get_tracked_object_pos()`**

In `R/circular_trials.R`, locate the block after TrajSet construction (around lines 273–276):
```r
  trajset <- set_transform_history(trajset, transform_history)
  trajset@meta$trial_limits <- trial_limits

  trajset
}
```

Insert three lines between `trajset@meta$trial_limits <- trial_limits` and the final `trajset`:
```r
  trajset <- set_transform_history(trajset, transform_history)
  trajset@meta$trial_limits        <- trial_limits
  trajset@meta$display_convention  <- "clock"
  trajset@meta$plot_x_col          <- "rel_x"
  trajset@meta$plot_y_col          <- "rel_y"

  trajset
}
```

- [ ] **Step 4: Update `radiate()` column resolution and apply rotation**

In `R/circular_plotting.R`, replace lines 1564–1565:
```r
  if (!is.null(ts@cols$x) && identical(x_col, "rel_x")) x_col <- ts@cols$x
  if (!is.null(ts@cols$y) && identical(y_col, "rel_y")) y_col <- ts@cols$y
```

With:
```r
  if (!is.null(ts@meta$plot_x_col) && identical(x_col, "rel_x")) {
    x_col <- ts@meta$plot_x_col
    if (!is.null(ts@meta$plot_y_col) && identical(y_col, "rel_y"))
      y_col <- ts@meta$plot_y_col
  } else if (!is.null(ts@cols$x) && identical(x_col, "rel_x")) {
    x_col <- ts@cols$x
    if (!is.null(ts@cols$y) && identical(y_col, "rel_y")) y_col <- ts@cols$y
  }
  if (identical(ts@meta$display_convention, "clock") &&
      all(c(x_col, y_col) %in% names(data))) {
    disp <- .to_clock_display(data[[x_col]], data[[y_col]])
    data[[".disp_x"]] <- disp$x
    data[[".disp_y"]] <- disp$y
    x_col <- ".disp_x"
    y_col <- ".disp_y"
  }
```

This replaces the two original lines with twelve lines. The `meta$plot_x_col` check only fires when the caller is using the default `x_col = "rel_x"`, preserving explicit overrides.

- [ ] **Step 5: Run tests to verify they pass**

```bash
Rscript -e "devtools::test(filter = 'circular-plotting|circular-trials')" 2>&1 | tail -20
```

Expected: all tests pass.

- [ ] **Step 6: Run the full test suite**

```bash
Rscript -e "devtools::test()" 2>&1 | tail -30
```

Expected: all tests pass. If any pre-existing test now fails, investigate before committing.

- [ ] **Step 7: Commit**

```bash
git add R/circular_trials.R R/circular_plotting.R \
        tests/testthat/test-circular-trials.R tests/testthat/test-circular-plotting.R
git commit -m "feat: get_tracked_object_pos sets clock display meta; radiate() applies rotation"
```

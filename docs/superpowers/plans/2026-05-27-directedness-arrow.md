# Directedness Arrow Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add `compute_circ_mean()` / `add_circ_mean()` / `add_heading_arrow()` so a directedness arrow composes with `+` like every other heading layer, fix the coordinate-convention bug in the old code, deprecate `circ_mean_segments()` and `gg_add_circ_mean()`, and clean up the vignette.

**Architecture:** Three-function pattern parallel to the density and interval trios. `compute_circ_mean()` converts input angles to unit-circle (UC) radians using `.clock_to_uc()` (which correctly handles `coords = "relative"` vs `"absolute"`), then uses the `circular` package for the mean and resultant length. `mean_dir` in the output is **always UC** — no re-conversion needed in `add_circ_mean()`. Angle convention and coords are read from `attr(headings_df, ...)` with parameter overrides.

**Tech Stack:** R, ggplot2, circular package, grid. Internal helpers already in the package: `.clock_to_uc()` (headings.R:52), `.wrap_to_2pi()` (headings.R:78), `%||%` (headings.R:16).

---

## File Map

| File | Change |
|---|---|
| `R/circular_plotting.R` | Add three new functions after `add_heading_interval` (line 861) |
| `R/headings.R` | Add `.Deprecated()` stubs to `circ_mean_segments` (line 683) and `gg_add_circ_mean` (line 700) |
| `tests/testthat/test-circular-plotting.R` | Append three new `# ----` test sections after line 784 |
| `vignettes/radiatR.Rmd` | Replace old arrow pattern in 5 chunks; add hidden display chunk |

---

## Task 1: `compute_circ_mean()` — tests and implementation

**Files:**
- Modify: `R/circular_plotting.R` — append after line 861 (end of `add_heading_interval`)
- Modify: `tests/testthat/test-circular-plotting.R` — append after line 784

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-circular-plotting.R` after line 784:

```r
# ---- compute_circ_mean -------------------------------------------------------

test_that("compute_circ_mean UC input returns correct mean_dir and R", {
  hd <- data.frame(heading = c(0, 0, 0, 0, 0))
  result <- compute_circ_mean(hd, angle_convention = "unit_circle", coords = "absolute")
  expect_equal(result$mean_dir,    0, tolerance = 1e-10)
  expect_equal(result$resultant_R, 1, tolerance = 1e-10)
})

test_that("compute_circ_mean clock+absolute maps 0 to UC pi/2", {
  # .clock_to_uc(0, "absolute") = wrap_to_2pi(rad_unclock(0)) = wrap_to_2pi(pi/2) = pi/2
  hd <- data.frame(heading = c(0, 0, 0, 0, 0))
  result <- compute_circ_mean(hd, angle_convention = "clock", coords = "absolute")
  expect_equal(result$mean_dir, pi / 2, tolerance = 1e-10)
})

test_that("compute_circ_mean clock+relative maps 0 to UC 0", {
  # .clock_to_uc(0, "relative") = (-0) %% (2*pi) = 0
  hd <- data.frame(heading = c(0, 0, 0, 0, 0))
  result <- compute_circ_mean(hd, angle_convention = "clock", coords = "relative")
  expect_equal(result$mean_dir, 0, tolerance = 1e-10)
})

test_that("compute_circ_mean clock+relative and clock+absolute differ for nonzero angle", {
  hd <- data.frame(heading = rep(pi / 4, 5))
  r_rel <- compute_circ_mean(hd, angle_convention = "clock", coords = "relative")
  r_abs <- compute_circ_mean(hd, angle_convention = "clock", coords = "absolute")
  expect_false(isTRUE(all.equal(r_rel$mean_dir, r_abs$mean_dir)))
})

test_that("compute_circ_mean grouped output has one row per group", {
  hd <- data.frame(heading = c(0, 0, pi, pi),
                   grp     = c("A", "A", "B", "B"))
  result <- compute_circ_mean(hd, colour_col = "grp",
                              angle_convention = "unit_circle", coords = "absolute")
  expect_equal(nrow(result), 2L)
  expect_true("grp" %in% names(result))
})

test_that("compute_circ_mean returns NA for fewer than 2 finite angles", {
  hd <- data.frame(heading = c(0.5))
  result <- compute_circ_mean(hd, angle_convention = "unit_circle", coords = "absolute")
  expect_true(is.na(result$mean_dir))
  expect_true(is.na(result$resultant_R))
})

test_that("compute_circ_mean handles all-NA input without error", {
  hd <- data.frame(heading = c(NA_real_, NA_real_, NA_real_))
  result <- compute_circ_mean(hd, angle_convention = "unit_circle", coords = "absolute")
  expect_equal(nrow(result), 1L)
  expect_true(is.na(result$mean_dir))
})

test_that("compute_circ_mean preserves factor levels on colour_col", {
  hd <- data.frame(heading = c(0.1, 0.2, 0.3, 0.4),
                   grp     = factor(c("B", "A", "B", "A"), levels = c("A", "B", "C")))
  result <- compute_circ_mean(hd, colour_col = "grp",
                              angle_convention = "unit_circle", coords = "absolute")
  expect_equal(levels(result$grp), c("A", "B", "C"))
})

test_that("compute_circ_mean reads convention from headings_df attributes", {
  hd <- data.frame(heading = c(0.1, 0.2, 0.3, 0.2, 0.1))
  attr(hd, "angle_convention") <- "clock"
  attr(hd, "coords")           <- "absolute"
  result_attr    <- compute_circ_mean(hd)
  result_explicit <- compute_circ_mean(hd, angle_convention = "clock", coords = "absolute")
  expect_equal(result_attr$mean_dir,    result_explicit$mean_dir,    tolerance = 1e-12)
  expect_equal(result_attr$resultant_R, result_explicit$resultant_R, tolerance = 1e-12)
})
```

- [ ] **Step 2: Run tests to confirm they fail**

```bash
Rscript -e "devtools::test(filter='circular-plotting')" 2>&1 | tail -3
```

Expected: FAIL — `compute_circ_mean` not found.

- [ ] **Step 3: Implement `compute_circ_mean()`**

Append to `R/circular_plotting.R` after line 861 (after the closing `}` of `add_heading_interval`):

```r
# ---- circular mean arrow -----------------------------------------------------

#' Compute circular mean direction and resultant length from a headings data frame
#'
#' Computes the circular mean direction and resultant length (R) per group from
#' a headings data frame, typically the output of [derive_headings()].
#' `mean_dir` in the returned data frame is **always in unit-circle convention**
#' (0 = East, counterclockwise), regardless of the input convention, making it
#' suitable for direct use in [add_circ_mean()].
#'
#' @param headings_df Data frame with a column of heading angles in radians.
#'   [derive_headings()] sets `attr(headings_df, "angle_convention")` and
#'   `attr(headings_df, "coords")` automatically.
#' @param heading_col Name of the column containing heading angles. Default
#'   `"heading"`.
#' @param colour_col Optional. Name of a column to group by. One row is
#'   returned per group. The same column maps to colour in [add_circ_mean()].
#' @param angle_convention Convention for angles in `heading_col`: `"clock"`
#'   (0 = North, clockwise) or `"unit_circle"` (0 = East, CCW). If `NULL`
#'   (default), read from `attr(headings_df, "angle_convention")`; defaults to
#'   `"unit_circle"` with a message if the attribute is also absent.
#' @param coords Coordinate system: `"relative"` or `"absolute"`. If `NULL`
#'   (default), read from `attr(headings_df, "coords")`; defaults to
#'   `"absolute"` with a message if absent.
#'
#' @return A data frame with columns `mean_dir` (unit-circle radians, 0 to
#'   2π), `resultant_R` (0–1), and `colour_col` when supplied. Both are `NA`
#'   when a group contains fewer than 2 finite angles.
#'
#' @seealso [add_circ_mean()], [add_heading_arrow()]
#' @importFrom circular circular mean.circular rho.circular
#' @export
compute_circ_mean <- function(headings_df,
                              heading_col      = "heading",
                              colour_col       = NULL,
                              angle_convention = NULL,
                              coords           = NULL) {
  if (!heading_col %in% names(headings_df))
    stop("`heading_col` '", heading_col, "' not found in headings_df.")

  if (is.null(angle_convention)) {
    angle_convention <- attr(headings_df, "angle_convention")
    if (is.null(angle_convention)) {
      message("`angle_convention` not found in headings_df attributes; defaulting to 'unit_circle'.")
      angle_convention <- "unit_circle"
    }
  }
  angle_convention <- match.arg(angle_convention, c("clock", "unit_circle"))

  if (is.null(coords)) {
    coords <- attr(headings_df, "coords")
    if (is.null(coords)) {
      message("`coords` not found in headings_df attributes; defaulting to 'absolute'.")
      coords <- "absolute"
    }
  }
  coords <- match.arg(coords, c("relative", "absolute"))

  use_colour <- !is.null(colour_col) && colour_col %in% names(headings_df)
  groups     <- if (use_colour) split(headings_df, headings_df[[colour_col]]) else list(headings_df)

  out_list <- lapply(seq_along(groups), function(i) {
    angles <- groups[[i]][[heading_col]]
    angles <- angles[is.finite(angles)]

    if (length(angles) < 2L) {
      row <- data.frame(mean_dir = NA_real_, resultant_R = NA_real_,
                        stringsAsFactors = FALSE)
    } else {
      uc_angles <- if (angle_convention == "clock") .clock_to_uc(angles, coords) else angles
      circ_obj  <- circular::circular(uc_angles, units = "radians", modulo = "2pi")
      mean_dir  <- .wrap_to_2pi(as.numeric(circular::mean.circular(circ_obj, na.rm = TRUE)))
      R         <- as.numeric(circular::rho.circular(circ_obj, na.rm = TRUE))
      row <- data.frame(mean_dir = mean_dir, resultant_R = R, stringsAsFactors = FALSE)
    }

    if (use_colour) row[[colour_col]] <- names(groups)[[i]]
    row
  })

  out <- do.call(rbind, out_list)
  if (use_colour && is.factor(headings_df[[colour_col]]))
    out[[colour_col]] <- factor(out[[colour_col]], levels = levels(headings_df[[colour_col]]))
  out
}
```

- [ ] **Step 4: Run tests — confirm all 9 `compute_circ_mean` tests pass**

```bash
Rscript -e "devtools::test(filter='circular-plotting')" 2>&1 | tail -3
```

Expected: `[ FAIL 0 | ... | PASS N ]` where the 9 new tests now pass.

- [ ] **Step 5: Commit**

```bash
git add R/circular_plotting.R tests/testthat/test-circular-plotting.R
git commit -m "feat: add compute_circ_mean() for circular mean direction summary"
```

---

## Task 2: `add_circ_mean()` — tests and implementation

**Files:**
- Modify: `R/circular_plotting.R` — append after `compute_circ_mean`
- Modify: `tests/testthat/test-circular-plotting.R` — append after `compute_circ_mean` tests

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-circular-plotting.R`:

```r
# ---- add_circ_mean -----------------------------------------------------------

test_that("add_circ_mean returns a LayerInstance", {
  sm <- data.frame(mean_dir = 0, resultant_R = 0.8)
  expect_s3_class(add_circ_mean(sm), "LayerInstance")
})

test_that("add_circ_mean returns empty layer for all-NA summary", {
  library(ggplot2)
  sm    <- data.frame(mean_dir = NA_real_, resultant_R = NA_real_)
  layer <- add_circ_mean(sm)
  expect_s3_class(layer, "LayerInstance")
  p <- ggplot() + coord_fixed() + layer
  expect_silent(ggplot_build(p))
})

test_that("add_circ_mean maps colour_col as aesthetic when present", {
  library(ggplot2)
  sm <- data.frame(mean_dir = c(0, pi / 2), resultant_R = c(0.8, 0.6),
                   grp = c("A", "B"))
  layer <- add_circ_mean(sm, colour_col = "grp")
  p     <- ggplot() + coord_fixed() + layer
  built <- ggplot_build(p)
  expect_equal(length(unique(built$data[[1]]$colour)), 2L)
})
```

- [ ] **Step 2: Run tests to confirm they fail**

```bash
Rscript -e "devtools::test(filter='circular-plotting')" 2>&1 | tail -3
```

Expected: FAIL — `add_circ_mean` not found.

- [ ] **Step 3: Implement `add_circ_mean()`**

Append to `R/circular_plotting.R` immediately after `compute_circ_mean`:

```r
#' Render pre-computed circular mean arrows on a radial plot
#'
#' Takes a data frame produced by [compute_circ_mean()] and renders each row
#' as a `geom_segment()` arrow. `mean_dir` must be in unit-circle convention
#' (0 = East, CCW), as returned by [compute_circ_mean()]. Rows where
#' `mean_dir` or `resultant_R` is `NA` are silently skipped.
#'
#' @param summary_df Data frame with columns `mean_dir` (UC radians, 0 to 2π)
#'   and `resultant_R` (0–1). Typically the output of [compute_circ_mean()].
#' @param colour_col Optional. Name of a column in `summary_df` to map to the
#'   colour aesthetic.
#' @param linewidth Line width of the arrow segment. Default `1`.
#' @param colour Fixed colour when `colour_col` is `NULL`. Default `"black"`.
#' @param arrow_length_cm Arrowhead length in cm. Default `0.2`.
#' @param ... Additional arguments forwarded to `geom_segment` (e.g.
#'   `linetype`, `alpha`, or a custom `arrow` spec that overrides the default).
#'
#' @return A `geom_segment()` layer.
#'
#' @seealso [compute_circ_mean()], [add_heading_arrow()]
#' @importFrom ggplot2 geom_segment aes
#' @importFrom rlang .data sym
#' @importFrom grid arrow unit
#' @export
add_circ_mean <- function(summary_df,
                          colour_col      = NULL,
                          linewidth       = 1,
                          colour          = "black",
                          arrow_length_cm = 0.2,
                          ...) {
  for (col in c("mean_dir", "resultant_R")) {
    if (!col %in% names(summary_df))
      stop("`summary_df` is missing required column '", col, "'.")
  }

  use_colour <- !is.null(colour_col) && colour_col %in% names(summary_df)
  valid_rows <- which(!is.na(summary_df$mean_dir) & !is.na(summary_df$resultant_R))

  if (!length(valid_rows)) {
    empty <- data.frame(.x = numeric(0), .y = numeric(0),
                        .xend = numeric(0), .yend = numeric(0))
    return(ggplot2::geom_segment(
      data    = empty,
      mapping = ggplot2::aes(x = .data$.x, y = .data$.y,
                             xend = .data$.xend, yend = .data$.yend),
      inherit.aes = FALSE
    ))
  }

  summary_df$.x    <- 0
  summary_df$.y    <- 0
  summary_df$.xend <- summary_df$resultant_R * cos(summary_df$mean_dir)
  summary_df$.yend <- summary_df$resultant_R * sin(summary_df$mean_dir)

  seg_map <- ggplot2::aes(x = .data$.x, y = .data$.y,
                          xend = .data$.xend, yend = .data$.yend)
  if (use_colour) seg_map[["colour"]] <- rlang::sym(colour_col)

  seg_args <- list(
    data        = summary_df,
    mapping     = seg_map,
    linewidth   = linewidth,
    arrow       = grid::arrow(length = grid::unit(arrow_length_cm, "cm")),
    inherit.aes = FALSE,
    ...
  )
  if (!use_colour) seg_args$colour <- colour

  do.call(ggplot2::geom_segment, seg_args)
}
```

- [ ] **Step 4: Run tests — confirm all 3 `add_circ_mean` tests pass**

```bash
Rscript -e "devtools::test(filter='circular-plotting')" 2>&1 | tail -3
```

Expected: `[ FAIL 0 | ... ]`

- [ ] **Step 5: Commit**

```bash
git add R/circular_plotting.R tests/testthat/test-circular-plotting.R
git commit -m "feat: add add_circ_mean() arrow rendering layer"
```

---

## Task 3: `add_heading_arrow()` wrapper — tests and implementation

**Files:**
- Modify: `R/circular_plotting.R` — append after `add_circ_mean`
- Modify: `tests/testthat/test-circular-plotting.R` — append after `add_circ_mean` tests

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-circular-plotting.R`:

```r
# ---- add_heading_arrow -------------------------------------------------------

test_that("add_heading_arrow returns a LayerInstance", {
  hd <- data.frame(heading = c(0.1, 0.2, 0.3, 0.4, 0.5))
  expect_s3_class(add_heading_arrow(hd, angle_convention = "unit_circle"), "LayerInstance")
})

test_that("add_heading_arrow matches two-step compute_circ_mean + add_circ_mean", {
  library(ggplot2)
  hd     <- data.frame(heading = c(0.1, 0.2, 0.15, 0.12, 0.18))
  p_wrap <- ggplot() + coord_fixed() +
    add_heading_arrow(hd, angle_convention = "unit_circle")
  sm     <- compute_circ_mean(hd, angle_convention = "unit_circle")
  p_step <- ggplot() + coord_fixed() + add_circ_mean(sm)
  bw <- ggplot_build(p_wrap)
  bs <- ggplot_build(p_step)
  expect_equal(bw$data[[1]]$x,    bs$data[[1]]$x,    tolerance = 1e-8)
  expect_equal(bw$data[[1]]$xend, bs$data[[1]]$xend, tolerance = 1e-8)
  expect_equal(bw$data[[1]]$y,    bs$data[[1]]$y,    tolerance = 1e-8)
  expect_equal(bw$data[[1]]$yend, bs$data[[1]]$yend, tolerance = 1e-8)
})

test_that("add_heading_arrow integrates with radiate() without error", {
  library(ggplot2)
  sim <- simulate_tracks(conditions = data.frame(n_trials = 5L), n_points = 20, seed = 1)
  hd  <- suppressWarnings(
    derive_headings(
      TrajSet(sim, id = "trial_id", time = "frame",
              angle = "rel_theta", x = "rel_x", y = "rel_y",
              angle_unit = "radians", normalize_xy = FALSE),
      rule = "crossing", circ0 = 0.2, circ1 = 0.4
    )
  )
  p <- radiate(sim, x_col = "rel_x", y_col = "rel_y",
               group_col = "trial_id",
               show_arrow = FALSE, show_labels = FALSE) +
    add_heading_arrow(hd)
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot_build(p))
})
```

- [ ] **Step 2: Run tests to confirm they fail**

```bash
Rscript -e "devtools::test(filter='circular-plotting')" 2>&1 | tail -3
```

Expected: FAIL — `add_heading_arrow` not found.

- [ ] **Step 3: Implement `add_heading_arrow()`**

Append to `R/circular_plotting.R` immediately after `add_circ_mean`:

```r
#' Compute a circular mean arrow and add it to a radial plot in one step
#'
#' Convenience wrapper that calls [compute_circ_mean()] followed by
#' [add_circ_mean()]. Use the two-step form directly when you need to inspect
#' or modify the summary data frame before rendering.
#'
#' @inheritParams compute_circ_mean
#' @inheritParams add_circ_mean
#'
#' @return A `geom_segment()` layer.
#'
#' @seealso [compute_circ_mean()], [add_circ_mean()]
#' @importFrom circular circular mean.circular rho.circular
#' @importFrom ggplot2 geom_segment aes
#' @importFrom rlang .data sym
#' @importFrom grid arrow unit
#' @export
add_heading_arrow <- function(headings_df,
                              heading_col      = "heading",
                              colour_col       = NULL,
                              angle_convention = NULL,
                              coords           = NULL,
                              linewidth        = 1,
                              colour           = "black",
                              arrow_length_cm  = 0.2,
                              ...) {
  sm <- compute_circ_mean(headings_df, heading_col = heading_col,
                          colour_col = colour_col,
                          angle_convention = angle_convention, coords = coords)
  add_circ_mean(sm, colour_col = colour_col,
                linewidth = linewidth, colour = colour,
                arrow_length_cm = arrow_length_cm, ...)
}
```

- [ ] **Step 4: Run tests — confirm all 3 `add_heading_arrow` tests pass**

```bash
Rscript -e "devtools::test(filter='circular-plotting')" 2>&1 | tail -3
```

Expected: `[ FAIL 0 | ... ]`

- [ ] **Step 5: Commit**

```bash
git add R/circular_plotting.R tests/testthat/test-circular-plotting.R
git commit -m "feat: add add_heading_arrow() convenience wrapper"
```

---

## Task 4: Deprecate `circ_mean_segments()` and `gg_add_circ_mean()`

**Files:**
- Modify: `R/headings.R:683` (`circ_mean_segments`)
- Modify: `R/headings.R:700` (`gg_add_circ_mean`)

- [ ] **Step 1: Add `.Deprecated()` to `circ_mean_segments()`**

In `R/headings.R`, find `circ_mean_segments <- function(stats_df, x0 = 0, y0 = 0, scale = 1) {` (line 683) and add the deprecation warning as the first line of the body:

```r
circ_mean_segments <- function(stats_df, x0 = 0, y0 = 0, scale = 1) {
  .Deprecated("compute_circ_mean",
              msg = paste0("circ_mean_segments() is deprecated. ",
                           "Use compute_circ_mean() + add_circ_mean() instead."))
  stopifnot(all(c("mean_dir","resultant_R") %in% names(stats_df)))
  convention <- attr(stats_df, "angle_convention") %||% "unit_circle"
  uc_dir <- if (convention == "clock") rad_unclock(stats_df$mean_dir) else stats_df$mean_dir
  xend <- x0 + scale * stats_df$resultant_R * cos(uc_dir)
  yend <- y0 + scale * stats_df$resultant_R * sin(uc_dir)
  cbind(stats_df, x = x0, y = y0, xend = xend, yend = yend)
}
```

- [ ] **Step 2: Add `.Deprecated()` to `gg_add_circ_mean()`**

Find `gg_add_circ_mean <- function(p, segments_df, ...)` (line 700) and add the deprecation warning:

```r
gg_add_circ_mean <- function(p, segments_df, color = "black", linewidth = 0.8,
                             arrow_spec = ggplot2::arrow(length = grid::unit(0.02, "npc")),
                             inherit_aes = FALSE) {
  .Deprecated("add_heading_arrow",
              msg = paste0("gg_add_circ_mean() is deprecated. ",
                           "Use p + add_heading_arrow(headings_df) instead."))
  p + ggplot2::geom_segment(data = segments_df,
                           ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
                           color = color, linewidth = linewidth,
                           arrow = arrow_spec,
                           inherit.aes = inherit_aes)
}
```

- [ ] **Step 3: Run tests to confirm nothing broke**

```bash
Rscript -e "devtools::test(filter='circular-plotting')" 2>&1 | tail -3
```

Expected: `[ FAIL 0 | ... ]` (deprecation warnings do not cause failures).

- [ ] **Step 4: Commit**

```bash
git add R/headings.R
git commit -m "deprecate: add .Deprecated() stubs to circ_mean_segments and gg_add_circ_mean"
```

---

## Task 5: Update vignette

**Files:**
- Modify: `vignettes/radiatR.Rmd`

The vignette currently uses the old pattern in five places. All are replaced with `add_heading_arrow()`. The intermediate variables `summ_all`, `seg_all`, `arc_summ`, and `segs` that exist only to feed the old pattern are removed.

- [ ] **Step 1: Replace `headings-overlay` chunk and add display chunk**

Find this entire chunk (lines 98–114):

```r
```{r headings-overlay}
summ_all <- circ_summary_headings(plividus, rule = "crossing",
                                  circ0 = 0.2, circ1 = 0.4,
                                  group_by = NULL,
                                  coords = "relative",
                                  angle_convention = "clock")
seg_all <- circ_mean_segments(summ_all)

p_all <- radiate(plividus,
                 group_col   = "trial_id",
                 colour_col  = "arc",
                 show_labels = FALSE,
                 show_arrow  = FALSE) +
  add_heading_points(hd, colour_col = "arc", size = 1, alpha = 0.6)

gg_add_circ_mean(p_all, seg_all)
```
```

Replace with:

```r
```{r headings-overlay}
p_all <- radiate(plividus,
                 group_col   = "trial_id",
                 colour_col  = "arc",
                 show_labels = FALSE,
                 show_arrow  = FALSE) +
  add_heading_points(hd, colour_col = "arc", size = 1, alpha = 0.6)

p_all + add_heading_arrow(hd)
```

```{r summ-display, include=FALSE}
summ_hd <- compute_circ_mean(hd)
# Convert UC mean_dir to relative-clock degrees for the narrative below:
# .clock_to_uc(theta, "relative") = (-theta) %% (2*pi), so inverse is same negation
summ_hd_deg <- round(((-summ_hd$mean_dir) %% (2 * pi)) * 180 / pi, 1)
summ_hd_R   <- round(summ_hd$resultant_R, 2)
```
```

- [ ] **Step 2: Update the inline narrative on line 116**

Find:
```
The grand mean arrow points at `r round(summ_all$mean_dir * 180 / pi, 1)`° relative to the stimulus (clock convention; 0° = toward stimulus) with *R* = `r round(summ_all$resultant_R, 2)`, reflecting the overall stimulus-relative tendency across all conditions.
```

Replace with:
```
The grand mean arrow points at `r summ_hd_deg`° relative to the stimulus (clock convention; 0° = toward stimulus) with *R* = `r summ_hd_R`, reflecting the overall stimulus-relative tendency across all conditions.
```

- [ ] **Step 3: Replace `arc-panels` chunk**

Find this entire chunk (lines 215–242, from `` ```{r arc-panels`` to the closing `` ``` ``):

```r
```{r arc-panels, fig.width=8, fig.height=5}
arc_summ <- do.call(rbind, lapply(split(hd, hd$arc), function(sub) {
  # Reverse relative clock -> UC: clock = (-uc) %% 2pi, so uc = (-clock) %% 2pi
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

p <-  p +
  geom_segment(
    data        = segs,
    aes(x = x, y = y, xend = xend, yend = yend),
    arrow       = grid::arrow(length = grid::unit(0.2, "cm")),
    colour      = "black",
    linewidth   = 1,
    inherit.aes = FALSE
  )
p
```
```

Replace with:

```r
```{r arc-panels, fig.width=8, fig.height=5}
p <- p + add_heading_arrow(hd, colour_col = "arc", colour = "black")
p
```
```

- [ ] **Step 4: Replace `density-overlay` chunk**

Find inside the `density-overlay` chunk:

```r
  geom_segment(
    data        = segs,
    aes(x = x, y = y, xend = xend, yend = yend),
    arrow       = grid::arrow(length = grid::unit(0.2, "cm")),
    colour      = "black",
    linewidth   = 1,
    inherit.aes = FALSE
  )
```

Replace with:

```r
  add_heading_arrow(hd, colour_col = "arc", colour = "black")
```

- [ ] **Step 5: Replace `bootstrap-ci` chunk**

Find the same `geom_segment(data = segs, ...)` block inside the `bootstrap-ci` chunk and replace with:

```r
  add_heading_arrow(hd, colour_col = "arc", colour = "black")
```

- [ ] **Step 6: Replace `build-3` chunk**

Find the `build-3` chunk:

```r
```{r build-3}
p <- p + geom_segment(
  data        = seg_all,
  aes(x = x, y = y, xend = xend, yend = yend),
  arrow       = grid::arrow(length = grid::unit(0.25, "cm")),
  colour      = "black",
  linewidth   = 1.2,
  inherit.aes = FALSE
)
p
```
```

Replace with:

```r
```{r build-3}
p <- p + add_heading_arrow(hd)
p
```
```

- [ ] **Step 7: Replace `stats` chunk and its heading**

Find the `stats` chunk and its preceding section heading/narrative:

```
## Circular Summary Statistics

`circ_summary_headings()` provides the per-condition statistics behind the arrows above:

```{r stats}
arc_summ[, c("arc", "mean_dir", "resultant_R")]
```
```

Replace with:

```
## Circular Summary Statistics

`compute_circ_mean()` returns the per-condition statistics behind the arrows above:

```{r stats}
compute_circ_mean(hd, colour_col = "arc")[, c("arc", "mean_dir", "resultant_R")]
```
```

- [ ] **Step 8: Verify the vignette knits cleanly**

```bash
Rscript -e "
pkgload::load_all('.', export_all=FALSE, helpers=FALSE, quiet=TRUE)
library(ggplot2)
rmarkdown::render('vignettes/radiatR.Rmd', quiet=TRUE)
cat('OK\n')
" 2>&1 | grep -E "^OK|^Error|Quitting"
```

Expected: `OK`

- [ ] **Step 9: Run full test suite**

```bash
Rscript -e "devtools::test()" 2>&1 | tail -3
```

Expected: same pass count as before this task (pre-existing failures in calibration/simulate-tracks are unrelated).

- [ ] **Step 10: Commit**

```bash
git add vignettes/radiatR.Rmd
git commit -m "docs: replace circ_mean_segments/gg_add_circ_mean with add_heading_arrow in vignette"
```

---

## Task 6: Regenerate docs and final check

**Files:**
- Modify: `NAMESPACE` (generated)
- Create: `man/compute_circ_mean.Rd`, `man/add_circ_mean.Rd`, `man/add_heading_arrow.Rd` (generated)

- [ ] **Step 1: Regenerate documentation**

```bash
Rscript -e "devtools::document()" 2>&1
```

Expected: no errors. The `draw_tracks @inheritParams` warning and `calibration_session.Rd` skip are pre-existing and harmless.

- [ ] **Step 2: Check new man pages exist**

```bash
ls man/compute_circ_mean.Rd man/add_circ_mean.Rd man/add_heading_arrow.Rd
```

Expected: all three files present.

- [ ] **Step 3: Run full test suite one more time**

```bash
Rscript -e "devtools::test(filter='circular-plotting')" 2>&1 | tail -3
```

Expected: `[ FAIL 0 | ... ]`

- [ ] **Step 4: Commit generated docs**

```bash
git add NAMESPACE man/compute_circ_mean.Rd man/add_circ_mean.Rd man/add_heading_arrow.Rd
git commit -m "docs: regenerate man pages for compute_circ_mean, add_circ_mean, add_heading_arrow"
```

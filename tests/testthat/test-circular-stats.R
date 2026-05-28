test_that("circ_summary matches circular package statistics", {
  df <- data.frame(
    id = rep(c("a", "b"), each = 4),
    time = rep(seq_len(4), times = 2),
    angle = c(0, pi / 6, pi / 3, pi / 2, pi, 5 * pi / 4, 3 * pi / 2, 11 * pi / 6)
  )

  ts <- TrajSet(df, id = "id", time = "time", angle = "angle", angle_unit = "radians")
  summary_by_id <- circ_summary(ts, by = "id")

  wrap_to_2pi_local <- function(theta) {
    out <- theta %% (2 * pi)
    out[out < 0] <- out[out < 0] + 2 * pi
    out
  }

  expected <- lapply(split(df$angle, df$id), function(theta) {
    tc <- circular::circular(theta, units = "radians", modulo = "2pi")
    list(
      mean = wrap_to_2pi_local(as.numeric(circular::mean.circular(tc))),
      R = as.numeric(circular::rho.circular(tc))
    )
  })

  for (grp in names(expected)) {
    row <- summary_by_id[summary_by_id$id == grp, ]
    expect_equal(row$mean_dir, expected[[grp]]$mean, tolerance = 1e-8)
    expect_equal(row$resultant_R, expected[[grp]]$R, tolerance = 1e-8)
  }

  global <- circ_summary(ts, by = "global")
  tc_global <- circular::circular(df$angle, units = "radians", modulo = "2pi")
  expect_equal(global$mean_dir, wrap_to_2pi_local(as.numeric(circular::mean.circular(tc_global))), tolerance = 1e-8)
  expect_equal(global$resultant_R, as.numeric(circular::rho.circular(tc_global)), tolerance = 1e-8)
})

test_that("clockwise conversions preserve angles", {
  angles <- seq(-2 * pi, 2 * pi, length.out = 11)
  clock <- rad2clock(angles)
  back_to_unit <- rad_unclock(clock)
  expect_true(all(back_to_unit <= pi & back_to_unit > -pi + 1e-12))
  expect_equal(back_to_unit, ((angles + pi) %% (2 * pi)) - pi, tolerance = 1e-8)
})

test_that("shepherding functions wrap angles into valid ranges", {
  angles <- c(-4*pi, -pi, -pi/2, 0, pi, 3*pi)
  wrapped_unit <- rad_shepherd(angles)
  expect_true(all(wrapped_unit <= pi & wrapped_unit > -pi - 1e-10))

  clock_angles <- c(-2*pi, -0.1, 0, pi, 3*pi, 5*pi)
  wrapped_clock <- rad_shepherd_clock(clock_angles)
  expect_true(all(wrapped_clock >= 0 & wrapped_clock < 2*pi + 1e-10))
})

test_that("circ_summary angle_convention='clock' converts mean_dir output", {
  # x=0, y>0 gives atan2(y,x) = pi/2 for all points (North in unit-circle)
  df <- data.frame(id = "A", time = 1:4,
                   x = c(0, 0, 0, 0), y = c(0.2, 0.4, 0.6, 0.8))
  ts <- TrajSet(df, id = "id", time = "time", x = "x", y = "y",
                normalize_xy = FALSE)
  summ_clock <- circ_summary(ts, angle_convention = "clock")
  # Absolute clock: (pi/2 - pi/2) %% 2pi = 0
  expect_equal(summ_clock$mean_dir, 0, tolerance = 1e-6)
  # Default (unit_circle) unchanged
  summ_uc <- circ_summary(ts)
  expect_equal(summ_uc$mean_dir, pi / 2, tolerance = 1e-6)
})

# ---- zone_dwell ---------------------------------------------------------------

test_that("zone_dwell assigns points to correct quadrant and ring", {
  df <- data.frame(
    id   = "t1",
    time = 1:4,
    x    = c( 0.3,  0.9, 0.0, -0.9),
    y    = c( 0.0,  0.0, 0.6,  0.0)
  )
  # target_angle = 0 (East). ring_breaks default: c(0, 0.5, 0.8, 1)
  # (0.3, 0)  -> r=0.30, R1, angle=0    -> Q1  => Q1.R1
  # (0.9, 0)  -> r=0.90, R3, angle=0    -> Q1  => Q1.R3
  # (0.0, 0.6)-> r=0.60, R2, angle=pi/2 -> Q2  => Q2.R2
  # (-0.9, 0) -> r=0.90, R3, angle=pi   -> Q3  => Q3.R3
  ts <- TrajSet(df, id = "id", time = "time", x = "x", y = "y",
                normalize_xy = FALSE)
  dw <- zone_dwell(ts, target_angle = 0)

  expect_equal(nrow(dw), 4L)
  expect_equal(sort(dw$zone), sort(c("Q1.R1", "Q1.R3", "Q2.R2", "Q3.R3")))
  expect_equal(sum(dw$proportion), 1.0, tolerance = 1e-10)
  expect_equal(dw[dw$zone == "Q1.R1", "n_frames"], 1L)
  expect_equal(dw[dw$zone == "Q1.R1", "proportion"], 0.25, tolerance = 1e-10)
})

test_that("zone_dwell gives proportion 1 when all points in same zone", {
  df <- data.frame(
    id   = "t1",
    time = 1:3,
    x    = c(0.30, 0.40, 0.35),
    y    = c(0.00, 0.00, 0.05)
  )
  # All r < 0.5 (R1), all angle near 0 (Q1) with target_angle = 0
  ts <- TrajSet(df, id = "id", time = "time", x = "x", y = "y",
                normalize_xy = FALSE)
  dw <- zone_dwell(ts, target_angle = 0)

  expect_equal(nrow(dw), 1L)
  expect_equal(dw$zone, "Q1.R1")
  expect_equal(dw$n_frames, 3L)
  expect_equal(dw$proportion, 1.0, tolerance = 1e-10)
})

test_that("zone_dwell excludes out-of-range points from proportion denominator", {
  df <- data.frame(
    id   = "t1",
    time = 1:3,
    x    = c(0.3, 1.5, 0.4),   # point 2: r=1.5 > max ring_break
    y    = c(0.0, 0.0, 0.0)
  )
  ts <- TrajSet(df, id = "id", time = "time", x = "x", y = "y",
                normalize_xy = FALSE)
  dw <- zone_dwell(ts, target_angle = 0, ring_breaks = c(0, 0.5, 0.8, 1))

  # Only 2 valid points; both land in Q1.R1
  expect_equal(nrow(dw), 1L)
  expect_equal(dw$n_frames, 2L)
  expect_equal(dw$proportion, 1.0, tolerance = 1e-10)
})

test_that("zone_dwell handles multiple trials independently", {
  df <- data.frame(
    id   = c("t1", "t1", "t2", "t2"),
    time = c(1L, 2L, 1L, 2L),
    x    = c(0.3, -0.3, 0.6, 0.6),
    y    = c(0.0,  0.0, 0.0, 0.0)
  )
  # t1: Q1.R1 (x=0.3) and Q3.R1 (x=-0.3) -> each 0.5
  # t2: both Q1.R2 (x=0.6)               -> proportion 1.0
  ts <- TrajSet(df, id = "id", time = "time", x = "x", y = "y",
                normalize_xy = FALSE)
  dw <- zone_dwell(ts, target_angle = 0)

  expect_true(all(c("t1", "t2") %in% dw$id))
  t2_row <- dw[dw$id == "t2", ]
  expect_equal(nrow(t2_row), 1L)
  expect_equal(t2_row$zone, "Q1.R2")
  expect_equal(t2_row$proportion, 1.0, tolerance = 1e-10)
  t1_rows <- dw[dw$id == "t1", ]
  expect_equal(nrow(t1_rows), 2L)
  expect_equal(sum(t1_rows$proportion), 1.0, tolerance = 1e-10)
})

test_that("zone_dwell uses rel_x/rel_y when coords='relative'", {
  df <- data.frame(
    id    = "t1",
    time  = 1:2,
    x     = c(50.0, 50.0),    # absolute pixel coords — far from origin
    y     = c(50.0, 50.0),
    rel_x = c(0.3, 0.6),      # relative coords in unit circle
    rel_y = c(0.0, 0.0)
  )
  ts <- TrajSet(df, id = "id", time = "time", x = "x", y = "y",
                rel_x = "rel_x", rel_y = "rel_y", normalize_xy = FALSE)

  dw <- zone_dwell(ts, target_angle = 0, coords = "relative")
  # rel_x=0.3 -> R1; rel_x=0.6 -> R2; both Q1
  expect_equal(nrow(dw), 2L)
  expect_true("Q1.R1" %in% dw$zone)
  expect_true("Q1.R2" %in% dw$zone)
})

# ---- count_goal_entries -------------------------------------------------------

test_that("count_goal_entries counts a single entry correctly", {
  # Goal at (1, 0); crossing_radius = 0.2 -> zone is x in [0.8, 1.2]
  # Path: outside -> inside -> inside -> outside => 1 entry
  df <- data.frame(
    id   = "t1",
    time = 1:4,
    x    = c(0.5, 0.9, 0.85, 0.5),
    y    = c(0.0, 0.0, 0.00, 0.0)
  )
  # distances from (1,0): 0.5, 0.1, 0.15, 0.5
  ts <- TrajSet(df, id = "id", time = "time", x = "x", y = "y",
                normalize_xy = FALSE)
  res <- count_goal_entries(ts, target_angle = 0, target_radius = 1,
                            crossing_radius = 0.2)
  expect_equal(res$n_entries, 1L)
})

test_that("count_goal_entries counts multiple entries", {
  # distances from (1,0): 0.5, 0.1, 0.5, 0.1 -> enter, exit, enter = 2 entries
  df <- data.frame(
    id   = "t1",
    time = 1:4,
    x    = c(0.5, 0.9, 0.5, 0.9),
    y    = c(0.0, 0.0, 0.0, 0.0)
  )
  ts <- TrajSet(df, id = "id", time = "time", x = "x", y = "y",
                normalize_xy = FALSE)
  res <- count_goal_entries(ts, target_angle = 0, target_radius = 1,
                            crossing_radius = 0.2)
  expect_equal(res$n_entries, 2L)
})

test_that("count_goal_entries counts initial inside-zone position as one entry", {
  # Animal starts at goal; should count as 1 entry
  df <- data.frame(
    id   = "t1",
    time = 1:3,
    x    = c(1.0, 0.5, 0.3),
    y    = c(0.0, 0.0, 0.0)
  )
  # distances from (1,0): 0.0, 0.5, 0.7 -> inside=T,F,F -> prepend F -> diff=1,-1,0 -> 1 entry
  ts <- TrajSet(df, id = "id", time = "time", x = "x", y = "y",
                normalize_xy = FALSE)
  res <- count_goal_entries(ts, target_angle = 0, target_radius = 1,
                            crossing_radius = 0.2)
  expect_equal(res$n_entries, 1L)
})

test_that("count_goal_entries returns zero when path never enters zone", {
  df <- data.frame(
    id   = "t1",
    time = 1:3,
    x    = c(0.0, -0.5, 0.0),
    y    = c(0.0,  0.0, 0.5)
  )
  # all distances from (1,0) > 0.2
  ts <- TrajSet(df, id = "id", time = "time", x = "x", y = "y",
                normalize_xy = FALSE)
  res <- count_goal_entries(ts, target_angle = 0, target_radius = 1,
                            crossing_radius = 0.2)
  expect_equal(res$n_entries, 0L)
})

test_that("count_goal_entries handles multiple trials", {
  df <- data.frame(
    id   = c("t1", "t1", "t2", "t2"),
    time = c(1L, 2L, 1L, 2L),
    x    = c(0.5, 0.9, 0.0, 0.0),
    y    = c(0.0, 0.0, 0.0, 0.0)
  )
  # t1: 1 entry (enters zone at x=0.9); t2: 0 entries (far from (1,0))
  ts <- TrajSet(df, id = "id", time = "time", x = "x", y = "y",
                normalize_xy = FALSE)
  res <- count_goal_entries(ts, target_angle = 0, target_radius = 1,
                            crossing_radius = 0.2)
  expect_equal(nrow(res), 2L)
  expect_equal(res[res$id == "t1", "n_entries"], 1L)
  expect_equal(res[res$id == "t2", "n_entries"], 0L)
})

test_that("count_goal_entries uses rel_x/rel_y when coords='relative'", {
  df <- data.frame(
    id    = "t1",
    time  = 1:3,
    x     = c(50.0, 50.0, 50.0),   # absolute coords — far from (1,0)
    y     = c(50.0, 50.0, 50.0),
    rel_x = c(0.5, 0.9, 0.5),      # relative: enters goal zone at frame 2
    rel_y = c(0.0, 0.0, 0.0)
  )
  ts <- TrajSet(df, id = "id", time = "time", x = "x", y = "y",
                rel_x = "rel_x", rel_y = "rel_y", normalize_xy = FALSE)
  res <- count_goal_entries(ts, target_angle = 0, target_radius = 1,
                            crossing_radius = 0.2, coords = "relative")
  expect_equal(res$n_entries, 1L)
  # Verify absolute coords give 0 entries (confirming dispatch is correct)
  res_abs <- count_goal_entries(ts, target_angle = 0, target_radius = 1,
                                crossing_radius = 0.2, coords = "absolute")
  expect_equal(res_abs$n_entries, 0L)
})

# ---- circ_summarise -----------------------------------------------------------

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

  expect_equal(result$n,           3L,       tolerance = 1e-8)
  expect_equal(result$mean_dir,    exp_mean, tolerance = 1e-8)
  expect_equal(result$mean_dir_deg, exp_mean * 180 / pi, tolerance = 1e-8)
  expect_equal(result$resultant_R, exp_R,    tolerance = 1e-8)
})

test_that("circ_summarise result is ungrouped tibble", {
  result <- circ_summarise(data.frame(heading = c(0, pi/2)), heading)
  expect_false(inherits(result, "grouped_df"))
  expect_s3_class(result, "tbl_df")
})

test_that("circ_summary matches circular package statistics", {
  df <- data.frame(
    id = rep(c("a", "b"), each = 4),
    time = rep(seq_len(4), times = 2),
    angle = c(0, pi / 6, pi / 3, pi / 2, pi, 5 * pi / 4, 3 * pi / 2, 11 * pi / 6)
  )

  ts <- tracks(df, id = "id", time = "time", angle = "angle", angle_unit = "radians")
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
  expect_true(all(back_to_unit >= 0 & back_to_unit < 2 * pi + 1e-12))
  expect_equal(back_to_unit, (pi / 2 - clock) %% (2 * pi), tolerance = 1e-8)
})

test_that("shepherding functions wrap angles into valid ranges", {
  angles <- c(-4*pi, -pi, -pi/2, 0, pi, 3*pi)
  wrapped_unit <- rad_shepherd(angles)
  expect_true(all(wrapped_unit <= pi & wrapped_unit > -pi - 1e-10))


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
  ts <- tracks(df, id = "id", time = "time", x = "x", y = "y",
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
  ts <- tracks(df, id = "id", time = "time", x = "x", y = "y",
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
  ts <- tracks(df, id = "id", time = "time", x = "x", y = "y",
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
  ts <- tracks(df, id = "id", time = "time", x = "x", y = "y",
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
  ts <- tracks(df, id = "id", time = "time", x = "x", y = "y",
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
  ts <- tracks(df, id = "id", time = "time", x = "x", y = "y",
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
  ts <- tracks(df, id = "id", time = "time", x = "x", y = "y",
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
  ts <- tracks(df, id = "id", time = "time", x = "x", y = "y",
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
  ts <- tracks(df, id = "id", time = "time", x = "x", y = "y",
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
  ts <- tracks(df, id = "id", time = "time", x = "x", y = "y",
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
  ts <- tracks(df, id = "id", time = "time", x = "x", y = "y",
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
  result <- circ_summarise(hd, heading, units = "radians")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1L)
  expect_named(result, c("n", "mean_dir", "mean_dir_deg", "resultant_R", "kappa"))
})

test_that("circ_summarise grand summary matches circular package values", {
  angles <- c(pi/6, pi/4, pi/3)
  tc     <- circular::circular(angles, units = "radians", modulo = "2pi")
  exp_mean <- as.numeric(circular::mean.circular(tc)) %% (2 * pi)
  exp_R    <- as.numeric(circular::rho.circular(tc))

  result <- circ_summarise(data.frame(heading = angles), heading, units = "radians")

  expect_equal(result$n,           3L,       tolerance = 1e-8)
  expect_equal(result$mean_dir,    exp_mean, tolerance = 1e-8)
  expect_equal(result$mean_dir_deg, (pi/2 - exp_mean) %% (2*pi) * 180/pi, tolerance = 1e-8)
  expect_equal(result$resultant_R, exp_R,    tolerance = 1e-8)
})

test_that("circ_summarise result is ungrouped tibble", {
  result <- circ_summarise(data.frame(heading = c(0, pi/2)), heading, units = "radians")
  expect_false(inherits(result, "grouped_df"))
  expect_s3_class(result, "tbl_df")
})

test_that("circ_summarise .by returns one row per group with correct columns", {
  hd <- data.frame(
    heading = c(pi/6, pi/4, pi/3, pi),
    arc     = c("a", "a", "b", "b")
  )
  result <- circ_summarise(hd, heading, units = "radians", .by = "arc")
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

  result <- circ_summarise(hd, heading, units = "radians", .by = "arc")

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
  result <- circ_summarise(hd, heading, units = "radians", .by = c("arc", "cond"))
  expect_equal(names(result)[1:2], c("arc", "cond"))
})

test_that("circ_summarise .by factor column preserves level order in output", {
  hd <- data.frame(
    heading = c(0, pi/2, pi, 3*pi/2),
    arc     = factor(c("b","b","a","a"), levels = c("b","a"))
  )
  result <- circ_summarise(hd, heading, units = "radians", .by = "arc")
  expect_equal(as.character(result$arc), c("b", "a"))
})

test_that("circ_summarise .by missing column raises informative error", {
  hd <- data.frame(heading = pi/4)
  expect_error(
    circ_summarise(hd, heading, units = "radians", .by = "nonexistent"),
    ".by column 'nonexistent' not found in data"
  )
})

test_that("circ_summarise respects group_by groups on a grouped tibble", {
  skip_if_not_installed("dplyr")
  hd <- dplyr::group_by(
    data.frame(heading = c(0, pi/2, pi, 3*pi/2), arc = c("a","a","b","b")),
    arc
  )
  result <- circ_summarise(hd, heading, units = "radians")
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
  result <- circ_summarise(hd, heading, units = "radians", .by = "cond")
  expect_true("cond" %in% names(result))
  expect_false("arc"  %in% names(result))
  expect_equal(nrow(result), 2L)
})

test_that("circ_summarise stats subset returns only requested columns", {
  hd     <- data.frame(heading = c(0, pi/2, pi))
  result <- circ_summarise(hd, heading, units = "radians", stats = c("n", "resultant_R"))
  expect_named(result, c("n", "resultant_R"))
})

test_that("circ_summarise stats column order matches stats argument order", {
  hd     <- data.frame(heading = c(0, pi/2))
  result <- circ_summarise(hd, heading, units = "radians",
                            stats = c("resultant_R", "n", "mean_dir"))
  expect_equal(names(result), c("resultant_R", "n", "mean_dir"))
})

test_that("circ_summarise unknown stats value raises informative error", {
  hd <- data.frame(heading = c(0, pi/2))
  expect_error(
    circ_summarise(hd, heading, units = "radians", stats = c("n", "foo")),
    "Unknown stats: 'foo'"
  )
})

test_that("circ_summarise stats ordering applies within grouped output too", {
  hd     <- data.frame(heading = c(0, pi), arc = c("a","b"))
  result <- circ_summarise(hd, heading, units = "radians", .by = "arc",
                            stats = c("resultant_R", "n"))
  expect_equal(names(result), c("arc", "resultant_R", "n"))
})

test_that("circ_summarise mean_dir_deg uses clock degrees by default", {
  hd <- data.frame(heading = rep(pi / 2, 4))  # UC North = clock 0
  out <- circ_summarise(hd, heading, units = "radians")
  expect_equal(out$mean_dir_deg, 0, tolerance = 1e-6)
})

test_that("circ_summarise mean_dir_deg respects custom display", {
  hd <- data.frame(heading = rep(0, 4))  # UC East
  d  <- circ_display(zero = 0)           # East = display 0
  out <- circ_summarise(hd, heading, units = "radians", display = d)
  expect_equal(out$mean_dir_deg, 0, tolerance = 1e-6)
})

test_that("circ_summarise display units=radians returns mean_dir_deg in radians", {
  hd <- data.frame(heading = rep(0, 4))   # UC East = clock pi/2
  out <- circ_summarise(hd, heading, units = "radians",
                         display = circ_display(units = "radians"))
  expect_equal(out$mean_dir_deg, pi / 2, tolerance = 1e-6)
})

# ---- circ_summarise edge cases ------------------------------------------------

test_that("circ_summarise all-NA angles returns n=0 and NA stats", {
  hd     <- data.frame(heading = c(NA_real_, NA_real_))
  result <- circ_summarise(hd, heading, units = "radians")
  expect_equal(result$n, 0L)
  expect_true(is.na(result$mean_dir))
  expect_true(is.na(result$mean_dir_deg))
  expect_true(is.na(result$resultant_R))
  expect_true(is.na(result$kappa))
})

test_that("circ_summarise n=1 returns kappa=NA but valid mean_dir and R", {
  hd     <- data.frame(heading = pi/4)
  result <- circ_summarise(hd, heading, units = "radians")
  expect_equal(result$n, 1L)
  expect_true(is.na(result$kappa))
  expect_false(is.na(result$mean_dir))
  expect_equal(result$resultant_R, 1, tolerance = 1e-6)
})

test_that("circ_summarise n=2 returns kappa=NA but valid mean_dir and R", {
  hd     <- data.frame(heading = c(pi/4, pi/4))
  result <- circ_summarise(hd, heading, units = "radians")
  expect_equal(result$n, 2L)
  expect_true(is.na(result$kappa))
  expect_false(is.na(result$mean_dir))
})

test_that("circ_summarise non-finite angles are excluded from n", {
  hd     <- data.frame(heading = c(pi/4, Inf, -Inf, NaN, NA))
  result <- circ_summarise(hd, heading, units = "radians")
  expect_equal(result$n, 1L)
  expect_equal(result$mean_dir, pi/4, tolerance = 1e-6)
})

test_that("circ_summarise all-NA group within multi-group data", {
  hd <- data.frame(
    heading = c(pi/4, NA_real_, NA_real_),
    arc     = c("a",  "b",     "b")
  )
  result <- circ_summarise(hd, heading, units = "radians", .by = "arc")
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
  expect_no_error(circ_summarise(hd, "angle", units = "radians"))
})

# ---- circ_dispersion ---------------------------------------------------------

test_that("circ_dispersion returns correct mean and R for known angles", {
  hd <- data.frame(heading = c(0, 0, 0))
  res <- circ_dispersion(hd)
  expect_equal(res$resultant_R, 1, tolerance = 1e-9)
  expect_equal(res$mean_dir,    0, tolerance = 1e-9)
  expect_equal(res$n,           3L)
  expect_equal(res$circ_sd,     0, tolerance = 1e-9)
})

test_that("circ_dispersion R near 0 for uniform spread", {
  hd <- data.frame(heading = c(0, pi/2, pi, 3*pi/2))
  res <- circ_dispersion(hd)
  expect_equal(res$resultant_R, 0, tolerance = 1e-9)
})

test_that("circ_dispersion groups correctly", {
  hd <- data.frame(id = c("A","A","B","B"),
                   heading = c(0, 0, pi/2, pi/2))
  res <- circ_dispersion(hd, group_col = "id")
  expect_equal(nrow(res), 2L)
  a <- res[res$id == "A", ]; b <- res[res$id == "B", ]
  expect_equal(a$resultant_R, 1, tolerance = 1e-9)
  expect_equal(b$mean_dir, pi/2, tolerance = 1e-9)
})

# ---- sector_summary ----------------------------------------------------------

test_that("sector_summary proportions sum to 1", {
  hd <- data.frame(heading = seq(-pi, pi, length.out = 100))
  ss <- sector_summary(hd, sectors = 8)
  expect_equal(sum(ss$proportion), 1, tolerance = 1e-9)
  expect_equal(nrow(ss), 8L)
})

test_that("sector_summary concentrates count in the correct sector", {
  hd <- data.frame(heading = rep(0, 20))  # all pointing East
  ss <- sector_summary(hd, sectors = 4)
  east <- ss[which.max(ss$count), ]
  expect_equal(east$mid_angle, 0, tolerance = pi/4 + 1e-9)
  expect_equal(east$count, 20L)
})

test_that("sector_summary groups by id", {
  hd <- data.frame(id = rep(c("A","B"), each = 10),
                   heading = c(rep(0, 10), rep(pi, 10)))
  ss <- sector_summary(hd, sectors = 4, group_col = "id")
  expect_equal(nrow(ss), 8L)
  # All of A's frames (heading=0) must land in exactly one sector
  a_ss <- ss[ss$id == "A", ]
  expect_equal(max(a_ss$count), 10L)
  expect_equal(sum(a_ss$count), 10L)
})

test_that("sector_summary accepts explicit break points", {
  hd <- data.frame(heading = c(-pi/2, 0, pi/2))
  ss <- sector_summary(hd, sectors = c(-pi/2, 0, pi/2))
  # breaks: -pi, -pi/2, 0, pi/2, pi → 4 sectors
  expect_equal(nrow(ss), 4L)
  expect_equal(sum(ss$count), 3L)
})

# ---- add_angle_rose ----------------------------------------------------------

test_that("add_angle_rose returns a ggplot2 layer", {
  hd <- data.frame(heading = seq(-pi, pi, length.out = 24))
  layer <- add_angle_rose(hd)
  expect_s3_class(layer, "LayerInstance")
})

test_that("add_angle_rose contains polygon data with x/y columns", {
  hd <- data.frame(heading = rep(0, 20))
  layer <- add_angle_rose(hd, bins = 4)
  expect_true(all(c("x", "y", ".rose_grp") %in% names(layer$data)))
})

# ---- vonmises_fit ------------------------------------------------------------

test_that("vonmises_fit returns high kappa for concentrated angles", {
  hd <- data.frame(heading = rep(0, 40))
  fit <- vonmises_fit(hd)
  expect_gt(fit$kappa, 10)
  expect_equal(fit$n, 40L)
  expect_equal(fit$mu, 0, tolerance = 1e-3)
})

test_that("vonmises_fit returns low kappa for near-uniform angles", {
  set.seed(1)
  hd <- data.frame(heading = runif(200, -pi, pi))
  fit <- vonmises_fit(hd)
  expect_lt(fit$kappa, 0.5)
})

test_that("vonmises_fit ci_lo < mu < ci_hi", {
  hd <- data.frame(heading = rnorm(50, mean = pi / 4, sd = 0.3))
  fit <- vonmises_fit(hd)
  expect_lt(fit$ci_lo, fit$mu)
  expect_gt(fit$ci_hi, fit$mu)
})

test_that("vonmises_fit groups correctly", {
  set.seed(42)
  hd <- data.frame(
    id      = rep(c("A", "B"), each = 50),
    heading = c(rep(0, 50), rep(pi, 50))
  )
  fit <- vonmises_fit(hd, group_col = "id")
  expect_equal(nrow(fit), 2L)
  fit_a <- fit[fit$id == "A", ]
  fit_b <- fit[fit$id == "B", ]
  expect_gt(fit_a$kappa, 10)
  expect_gt(fit_b$kappa, 10)
  expect_equal(fit_a$mu, 0,  tolerance = 1e-3)
  expect_equal(abs(fit_b$mu), pi, tolerance = 1e-3)
})

test_that("vonmises_fit returns NA row for n < 2", {
  hd <- data.frame(heading = 0.5)
  fit <- vonmises_fit(hd)
  expect_equal(fit$n, 1L)
  expect_true(is.na(fit$kappa))
})

# ---- test_uniformity ---------------------------------------------------------

test_that("test_uniformity returns low p for concentrated angles", {
  hd <- data.frame(heading = rnorm(40, 0, 0.3))
  res <- test_uniformity(hd)
  expect_equal(res$test, "rayleigh")
  expect_lt(res$p_value, 0.05)
  expect_equal(res$n, 40L)
})

test_that("test_uniformity returns high p for uniform angles", {
  set.seed(1)
  hd <- data.frame(heading = runif(60, -pi, pi))
  res <- test_uniformity(hd)
  expect_gt(res$p_value, 0.05)
})

test_that("test_uniformity groups by condition column", {
  set.seed(1)
  hd <- data.frame(
    grp     = rep(c("A","B"), each = 40),
    heading = c(rnorm(40, 0, 0.2), runif(40, -pi, pi))
  )
  res <- test_uniformity(hd, group_col = "grp")
  expect_equal(nrow(res), 2L)
  a_p <- res$p_value[res$grp == "A"]
  b_p <- res$p_value[res$grp == "B"]
  expect_lt(a_p, 0.05)
  expect_gt(b_p, 0.05)
})

# ---- test_mean_directions ----------------------------------------------------

test_that("test_mean_directions detects different mean directions", {
  set.seed(1)
  hd <- data.frame(
    cond    = rep(c("A","B"), each = 30),
    heading = c(
      as.numeric(circular::rvonmises(30, circular::circular(0),    kappa = 4)),
      as.numeric(circular::rvonmises(30, circular::circular(pi/2), kappa = 4))
    )
  )
  res <- test_mean_directions(hd, group_col = "cond")
  expect_equal(res$n_groups, 2L)
  expect_equal(res$test, "Watson-Williams")
  expect_lt(res$p_value, 0.01)
})

test_that("test_mean_directions pairwise returns correct number of rows", {
  set.seed(2)
  hd <- data.frame(
    cond    = rep(c("A","B","C"), each = 25),
    heading = as.numeric(circular::rvonmises(
      75, circular::circular(0), kappa = 2))
  )
  res <- test_mean_directions(hd, group_col = "cond", pairwise = TRUE)
  # 3 groups -> C(3,2) = 3 pairs
  expect_equal(nrow(res), 3L)
  expect_true(all(c("group1","group2","p_value") %in% names(res)))
})

test_that("test_mean_directions same-direction groups give high p", {
  set.seed(3)
  hd <- data.frame(
    cond    = rep(c("A","B"), each = 40),
    heading = as.numeric(circular::rvonmises(
      80, circular::circular(0), kappa = 3))
  )
  res <- test_mean_directions(hd, group_col = "cond")
  expect_gt(res$p_value, 0.05)
})

# ---- test_concentration ------------------------------------------------------

test_that("test_concentration returns a one-row data frame", {
  set.seed(1)
  hd <- data.frame(
    cond    = rep(c("A","B"), each = 30),
    heading = as.numeric(circular::rvonmises(
      60, circular::circular(0), kappa = 2))
  )
  res_p <- test_concentration(hd, group_col = "cond")
  expect_equal(nrow(res_p), 1L)
  expect_equal(res_p$test, "equal.kappa")

  res_np <- test_concentration(hd, group_col = "cond", parametric = FALSE)
  expect_equal(nrow(res_np), 1L)
  expect_equal(res_np$test, "wallraff")
})

test_that("test_concentration detects unequal concentrations", {
  set.seed(1)
  hd <- data.frame(
    cond    = rep(c("low","high"), each = 50),
    heading = c(
      as.numeric(circular::rvonmises(50, circular::circular(0), kappa = 0.5)),
      as.numeric(circular::rvonmises(50, circular::circular(0), kappa = 10))
    )
  )
  res <- suppressWarnings(test_concentration(hd, group_col = "cond"))
  expect_lt(res$p_value, 0.05)
})

# ---- p_adjust in test_uniformity and test_mean_directions --------------------

test_that("test_uniformity p_adjust adds p_value_adj column", {
  set.seed(1)
  hd <- data.frame(
    grp     = rep(c("A", "B", "C"), each = 30),
    heading = c(rnorm(30, 0, 0.3),
                rnorm(30, pi, 0.3),
                runif(30, -pi, pi))
  )
  res_none <- test_uniformity(hd, group_col = "grp")
  expect_false("p_value_adj" %in% names(res_none))

  res_bh <- test_uniformity(hd, group_col = "grp", p_adjust = "BH")
  expect_true("p_value_adj" %in% names(res_bh))
  expect_equal(nrow(res_bh), 3L)
  # BH p_value_adj >= p_value (adjustment can only increase or equal)
  expect_true(all(res_bh$p_value_adj >= res_bh$p_value - 1e-9, na.rm = TRUE))
})

test_that("test_mean_directions p_adjust adds p_value_adj for pairwise only", {
  set.seed(2)
  hd <- data.frame(
    cond    = rep(c("A", "B", "C"), each = 30),
    heading = c(
      as.numeric(circular::rvonmises(30, circular::circular(0),    kappa = 3)),
      as.numeric(circular::rvonmises(30, circular::circular(pi/2), kappa = 3)),
      as.numeric(circular::rvonmises(30, circular::circular(pi),   kappa = 3))
    )
  )
  # Omnibus: no p_value_adj even when p_adjust is set
  omni <- test_mean_directions(hd, group_col = "cond", p_adjust = "BH")
  expect_false("p_value_adj" %in% names(omni))

  # Pairwise without correction
  pw_none <- test_mean_directions(hd, group_col = "cond", pairwise = TRUE)
  expect_false("p_value_adj" %in% names(pw_none))

  # Pairwise with BH correction
  pw_bh <- test_mean_directions(hd, group_col = "cond", pairwise = TRUE,
                                 p_adjust = "BH")
  expect_true("p_value_adj" %in% names(pw_bh))
  expect_equal(nrow(pw_bh), 3L)   # C(3,2) = 3 pairs
  expect_true(all(pw_bh$p_value_adj >= pw_bh$p_value - 1e-9))
})

test_that("test_mean_directions holm gives family-wise control", {
  set.seed(3)
  # All three groups same direction: all pairs should have high p_value_adj
  hd <- data.frame(
    cond    = rep(c("A", "B", "C"), each = 40),
    heading = as.numeric(circular::rvonmises(
      120, circular::circular(0), kappa = 3))
  )
  pw <- test_mean_directions(hd, group_col = "cond", pairwise = TRUE,
                              p_adjust = "holm")
  # All adjusted p-values should be > 0.05 (no spurious significance)
  expect_true(all(pw$p_value_adj > 0.05))
})

# ---- wrappedcauchy_fit -------------------------------------------------------

test_that("wrappedcauchy_fit recovers rho near true value", {
  set.seed(7)
  hd <- data.frame(
    heading = as.numeric(circular::rwrappedcauchy(
      150, circular::circular(0), rho = 0.6))
  )
  fit <- wrappedcauchy_fit(hd)
  expect_true(is.integer(fit$convergence))  # code passed through; 1 is common
  expect_equal(fit$n, 150L)
  expect_equal(fit$rho, 0.6, tolerance = 0.15)
  # mu near 0: check minimum circular distance to 0 is small
  mu_dist <- min(abs(fit$mu), abs(fit$mu - 2*pi), abs(fit$mu + 2*pi))
  expect_lt(mu_dist, 0.4)
})

test_that("wrappedcauchy_fit groups by condition", {
  set.seed(2)
  hd <- data.frame(
    grp     = rep(c("A", "B"), each = 50),
    heading = c(
      as.numeric(circular::rwrappedcauchy(50, circular::circular(0),    rho = 0.7)),
      as.numeric(circular::rwrappedcauchy(50, circular::circular(pi/2), rho = 0.4))
    )
  )
  fit <- wrappedcauchy_fit(hd, group_col = "grp")
  expect_equal(nrow(fit), 2L)
  expect_gt(fit$rho[fit$grp == "A"], fit$rho[fit$grp == "B"])
})

test_that("wrappedcauchy_fit returns NA row for n < 2", {
  fit <- wrappedcauchy_fit(data.frame(heading = 0.5))
  expect_equal(fit$n, 1L)
  expect_true(is.na(fit$rho))
})

# ---- circ_cor ----------------------------------------------------------------

test_that("circ_cor circular-linear detects strong association", {
  set.seed(1)
  theta <- as.numeric(circular::rvonmises(60, circular::circular(0), kappa=3))
  hd <- data.frame(heading = theta, body_size = theta + rnorm(60, 0, 0.3))
  res <- circ_cor(hd, x_col = "body_size")
  expect_equal(res$type, "circular-linear")
  expect_gt(res$r, 0.5)
  expect_lt(res$p_value, 0.05)
  expect_equal(res$df, 2L)
})

test_that("circ_cor circular-linear near zero for independent variables", {
  set.seed(2)
  hd <- data.frame(
    heading   = as.numeric(circular::rvonmises(80, circular::circular(0), kappa=2)),
    covariate = rnorm(80)
  )
  res <- circ_cor(hd, x_col = "covariate")
  expect_lt(res$r, 0.3)
  expect_gt(res$p_value, 0.05)
})

test_that("circ_cor circular-circular works and returns r in [-1,1]", {
  set.seed(3)
  a <- as.numeric(circular::rvonmises(50, circular::circular(pi/4), kappa=2))
  hd <- data.frame(heading = a, angle2 = a + rnorm(50, 0, 0.2))
  res <- circ_cor(hd, x_col = "angle2", x_type = "circular")
  expect_equal(res$type, "circular-circular")
  expect_true(res$r >= -1 && res$r <= 1)
})

test_that("circ_cor groups correctly", {
  set.seed(4)
  theta_a <- as.numeric(circular::rvonmises(40, circular::circular(0), kappa=3))
  hd <- data.frame(
    grp     = rep(c("A","B"), each=40),
    heading = c(theta_a, as.numeric(circular::rvonmises(40, circular::circular(pi), kappa=3))),
    x       = c(theta_a + rnorm(40,0,0.2), rnorm(40))  # A correlated, B not
  )
  res <- circ_cor(hd, x_col = "x", group_col = "grp")
  expect_equal(nrow(res), 2L)
  expect_gt(res$r[res$grp == "A"], res$r[res$grp == "B"])
})

test_that("circ_cor test=FALSE omits test columns", {
  hd <- data.frame(heading = rnorm(20), x = rnorm(20))
  res <- circ_cor(hd, x_col = "x", test = FALSE)
  expect_false("p_value" %in% names(res))
  expect_false("statistic" %in% names(res))
  expect_true("r" %in% names(res))
})

test_that("circ_summarise reports n_total and n_missing when requested", {
  df <- data.frame(
    g = c("a", "a", "a", "b", "b"),
    angle = c(0.1, NA, 1.2, NA, 2.0)
  )
  out <- circ_summarise(df, angle, units = "radians", .by = "g",
                        stats = c("n", "n_total", "n_missing"))
  out <- out[order(out$g), ]
  expect_equal(out$n,         c(2L, 1L))   # valid (non-NA) per group
  expect_equal(out$n_total,   c(3L, 2L))
  expect_equal(out$n_missing, c(1L, 1L))
  expect_true(all(out$n_total == out$n + out$n_missing))
})

test_that("circ_summarise default stats are unchanged (no denominator columns)", {
  df <- data.frame(angle = c(0.1, 0.2, NA))
  out <- circ_summarise(df, angle, units = "radians")
  expect_false("n_total"   %in% names(out))
  expect_false("n_missing" %in% names(out))
  expect_true("n" %in% names(out))
})

test_that("vonmises_fit(axial = TRUE) recovers an axis from antipodal data", {
  set.seed(11)
  hd <- data.frame(heading = c(rnorm(50, 0, 0.2), rnorm(50, pi, 0.2)) %% (2 * pi))

  ax  <- vonmises_fit(hd, axial = TRUE)
  dir <- vonmises_fit(hd, axial = FALSE)

  expect_gt(ax$kappa, 1)
  expect_gte(ax$mu, 0)
  expect_lt(ax$mu, pi)
  expect_lt(min(abs(ax$mu - c(0, pi))), 0.15)
  expect_lt(dir$kappa, 0.3)
})

test_that("vonmises_fit(axial = FALSE) is unchanged on directional data", {
  set.seed(12)
  hd <- data.frame(heading = rnorm(60, 1, 0.3) %% (2 * pi))
  expect_equal(vonmises_fit(hd, axial = FALSE), vonmises_fit(hd))
  expect_equal(names(vonmises_fit(hd, axial = TRUE)), names(vonmises_fit(hd)))
})

test_that("wrappedcauchy_fit(axial = TRUE) recovers an axis from antipodal data", {
  set.seed(21)
  hd <- data.frame(heading = c(rnorm(50, 0, 0.2), rnorm(50, pi, 0.2)) %% (2 * pi))

  ax  <- wrappedcauchy_fit(hd, axial = TRUE)
  dir <- wrappedcauchy_fit(hd, axial = FALSE)

  expect_gt(ax$rho, 0.3)
  expect_gte(ax$mu, 0)
  expect_lt(ax$mu, pi)
  expect_lt(min(abs(ax$mu - c(0, pi))), 0.15)
  expect_lt(dir$rho, 0.2)
})

test_that("wrappedcauchy_fit(axial = FALSE) is unchanged on directional data", {
  set.seed(22)
  hd <- data.frame(heading = rnorm(60, 1, 0.3) %% (2 * pi))
  expect_equal(wrappedcauchy_fit(hd, axial = FALSE), wrappedcauchy_fit(hd))
  expect_equal(names(wrappedcauchy_fit(hd, axial = TRUE)), names(wrappedcauchy_fit(hd)))
})

test_that(".hr_statistic matches the closed-form value and is order-invariant", {
  th <- c(0, pi / 2, pi)
  # Reference (Landler et al. 2019 / CircMLE HermansRasson2T) skewness term is
  # subtracted: dispersion pi (pi/2 + 0 + pi/2) minus 2.895 * skew (1 + 0 + 1).
  expect_equal(radiatR:::.hr_statistic(th), pi - 2.895 * 2, tolerance = 1e-4)
  expect_equal(radiatR:::.hr_statistic(th),
               radiatR:::.hr_statistic(rev(th)), tolerance = 1e-12)
})

test_that(".hr_statistic is larger for clustered than for spread data", {
  set.seed(1)
  clustered <- rnorm(30, 0, 0.15)
  spread    <- seq(0, 2 * pi, length.out = 31)[-31]
  expect_gt(radiatR:::.hr_statistic(clustered), radiatR:::.hr_statistic(spread))
})

test_that("test_uniformity(hermans_rasson) rejects clustered, not uniform data", {
  set.seed(101)
  clustered <- data.frame(heading = rnorm(40, 1, 0.3) %% (2 * pi))
  uniform   <- data.frame(heading = seq(0, 2 * pi, length.out = 41)[-41])

  rc <- test_uniformity(clustered, test = "hermans_rasson", n_sim = 499)
  ru <- test_uniformity(uniform,   test = "hermans_rasson", n_sim = 499)

  expect_equal(rc$test, "hermans_rasson")
  expect_lt(rc$p_value, 0.05)
  expect_gt(ru$p_value, 0.10)
  expect_gt(rc$p_value, 0)
  expect_lte(ru$p_value, 1)
})

test_that("test_uniformity(hermans_rasson) catches bimodal data the Rayleigh test misses", {
  set.seed(102)
  bimodal <- data.frame(heading = c(rnorm(30, 0, 0.2), rnorm(30, pi, 0.2)) %% (2 * pi))
  ray <- test_uniformity(bimodal, test = "rayleigh")
  hr  <- test_uniformity(bimodal, test = "hermans_rasson", n_sim = 499)
  expect_gt(ray$p_value, 0.10)
  expect_lt(hr$p_value, 0.05)
})

test_that("test_uniformity(hermans_rasson) is reproducible and integrates with grouping", {
  set.seed(7); a <- test_uniformity(
    data.frame(heading = rnorm(30, 1, 0.3)), test = "hermans_rasson", n_sim = 299)
  set.seed(7); b <- test_uniformity(
    data.frame(heading = rnorm(30, 1, 0.3)), test = "hermans_rasson", n_sim = 299)
  expect_equal(a$p_value, b$p_value)

  set.seed(8)
  hd <- data.frame(heading = c(rnorm(25, 0, 0.3), rnorm(25, 2, 0.3)),
                   grp = rep(c("a", "b"), each = 25))
  g <- test_uniformity(hd, group_col = "grp", test = "hermans_rasson",
                       n_sim = 299, p_adjust = "BH")
  expect_equal(nrow(g), 2L)
  expect_true(all(c("grp", "statistic", "p_value", "n", "test", "p_value_adj") %in% names(g)))
})

# ---- test_uniformity: V-test -------------------------------------------------

test_that("test_uniformity V-test detects concentration at the specified mu", {
  set.seed(1)
  hd <- data.frame(heading = as.numeric(circular::rvonmises(
    60, circular::circular(pi / 2), kappa = 4)))
  res <- test_uniformity(hd, test = "vtest", mu = pi / 2)
  expect_equal(res$test, "vtest")
  expect_lt(res$p_value, 0.001)
})

test_that("test_uniformity V-test has little power against the wrong mu", {
  set.seed(1)
  # data concentrated at 0, tested against the orthogonal direction pi/2
  hd <- data.frame(heading = as.numeric(circular::rvonmises(
    60, circular::circular(0), kappa = 4)))
  res <- test_uniformity(hd, test = "vtest", mu = pi / 2)
  expect_gt(res$p_value, 0.05)
})

test_that("test_uniformity V-test requires mu", {
  hd <- data.frame(heading = rnorm(30, 0, 0.3))
  expect_error(test_uniformity(hd, test = "vtest"), "mu")
})

# ---- test_distributions ------------------------------------------------------

test_that("test_distributions watson_wheeler detects different distributions", {
  set.seed(1)
  hd <- data.frame(
    grp = rep(c("A", "B"), each = 40),
    heading = c(as.numeric(circular::rvonmises(40, circular::circular(0),  4)),
                as.numeric(circular::rvonmises(40, circular::circular(pi), 4)))
  )
  res <- test_distributions(hd, "grp", method = "watson_wheeler")
  expect_equal(nrow(res), 1L)
  expect_equal(res$component, "distribution")
  expect_equal(res$n_groups, 2L)
  expect_lt(res$p_value, 0.01)
})

test_that("test_distributions watson_wheeler gives high p for same distribution", {
  set.seed(7)
  hd <- data.frame(
    grp = rep(c("A", "B"), each = 50),
    heading = as.numeric(circular::rvonmises(100, circular::circular(0), 3))
  )
  res <- test_distributions(hd, "grp", method = "watson_wheeler")
  expect_gt(res$p_value, 0.05)
})

test_that("test_distributions watson_two returns a tabled p and statistic", {
  set.seed(2)
  hd <- data.frame(
    grp = rep(c("A", "B"), each = 40),
    heading = c(as.numeric(circular::rvonmises(40, circular::circular(0),  4)),
                as.numeric(circular::rvonmises(40, circular::circular(pi), 4)))
  )
  res <- test_distributions(hd, "grp", method = "watson_two")
  expect_equal(nrow(res), 1L)
  expect_true(is.na(res$df))
  expect_gt(res$statistic, 0.385)   # clearly different -> smallest p band
  expect_equal(res$p_value, 0.001)
})

test_that("test_distributions watson_two omnibus needs exactly two groups", {
  set.seed(2)
  hd <- data.frame(
    grp = rep(c("A", "B", "C"), each = 20),
    heading = as.numeric(circular::rvonmises(60, circular::circular(0), 3))
  )
  expect_error(test_distributions(hd, "grp", method = "watson_two"),
               "two|pairwise")
})

test_that("test_distributions watson_two pairwise gives one row per pair", {
  set.seed(3)
  hd <- data.frame(
    grp = rep(c("A", "B", "C"), each = 25),
    heading = as.numeric(circular::rvonmises(75, circular::circular(0), 3))
  )
  res <- test_distributions(hd, "grp", method = "watson_two", pairwise = TRUE)
  expect_equal(nrow(res), 3L)   # C(3,2)
  expect_true(all(c("group1", "group2") %in% names(res)))
})

test_that("test_distributions rao returns mean and dispersion components", {
  # Rao's polar-vector (mean) test is famously low-powered, so assert on the
  # dispersion component, which reliably detects clearly unequal concentration.
  set.seed(9)
  hd <- data.frame(
    grp = rep(c("A", "B"), each = 60),
    heading = c(as.numeric(circular::rvonmises(60, circular::circular(0), 0.5)),
                as.numeric(circular::rvonmises(60, circular::circular(0), 12)))
  )
  res <- test_distributions(hd, "grp", method = "rao")
  expect_equal(nrow(res), 2L)
  expect_equal(sort(res$component), c("dispersion", "mean"))
  expect_lt(res$p_value[res$component == "dispersion"], 0.05)
})

test_that("test_distributions pairwise p_adjust adds p_value_adj", {
  set.seed(5)
  hd <- data.frame(
    grp = rep(c("A", "B", "C"), each = 30),
    heading = c(as.numeric(circular::rvonmises(30, circular::circular(0),    4)),
                as.numeric(circular::rvonmises(30, circular::circular(pi/2), 4)),
                as.numeric(circular::rvonmises(30, circular::circular(pi),   4)))
  )
  res <- test_distributions(hd, "grp", method = "watson_wheeler",
                            pairwise = TRUE, p_adjust = "BH")
  expect_true("p_value_adj" %in% names(res))
  expect_equal(nrow(res), 3L)
  expect_true(all(res$p_value_adj >= res$p_value - 1e-9))
})

# ---- boot_mean_ci ------------------------------------------------------------

test_that("boot_mean_ci recovers the mean and brackets it", {
  set.seed(1)
  hd <- data.frame(heading = as.numeric(circular::rvonmises(
    60, circular::circular(1), kappa = 8)))
  res <- boot_mean_ci(hd, R = 499)
  expect_equal(res$n, 60L)
  expect_lt(abs(res$mu - 1), 0.15)
  expect_lt(res$ci_lo, res$mu)
  expect_gt(res$ci_hi, res$mu)
  expect_equal(res$mu_deg, res$mu * 180 / pi, tolerance = 1e-8)
})

test_that("boot_mean_ci gives a wider interval for lower concentration", {
  set.seed(2)
  hi <- data.frame(heading = as.numeric(circular::rvonmises(
    60, circular::circular(1), kappa = 20)))
  lo <- data.frame(heading = as.numeric(circular::rvonmises(
    60, circular::circular(1), kappa = 0.8)))
  w_hi <- with(boot_mean_ci(hi, R = 499), ci_hi - ci_lo)
  w_lo <- with(boot_mean_ci(lo, R = 499), ci_hi - ci_lo)
  expect_gt(w_lo, w_hi)
})

test_that("boot_mean_ci is reproducible under a fixed seed", {
  hd <- data.frame(heading = as.numeric(circular::rvonmises(
    40, circular::circular(0.5), kappa = 5)))
  set.seed(99); a <- boot_mean_ci(hd, R = 299)
  set.seed(99); b <- boot_mean_ci(hd, R = 299)
  expect_equal(a, b)
})

test_that("boot_mean_ci groups by condition", {
  set.seed(3)
  hd <- data.frame(
    grp     = rep(c("A", "B"), each = 40),
    heading = c(as.numeric(circular::rvonmises(40, circular::circular(0),    6)),
                as.numeric(circular::rvonmises(40, circular::circular(pi/2), 6)))
  )
  res <- boot_mean_ci(hd, group_col = "grp", R = 299)
  expect_equal(nrow(res), 2L)
  expect_true("grp" %in% names(res))
})

test_that("boot_mean_ci returns an NA row for n < 2", {
  hd <- data.frame(heading = 0.3)
  res <- boot_mean_ci(hd, R = 99)
  expect_true(is.na(res$mu))
  expect_equal(res$n, 1L)
})

test_that("boot_mean_ci axial recovers an axis in [0, pi)", {
  set.seed(4)
  hd <- data.frame(heading = c(rnorm(40, 0, 0.2),
                               rnorm(40, pi, 0.2)) %% (2 * pi))
  res <- boot_mean_ci(hd, R = 299, axial = TRUE)
  expect_gte(res$mu, 0)
  expect_lt(res$mu, pi)
  expect_lt(min(abs(res$mu - c(0, pi))), 0.15)
})

# ---- test_mean_directions: permutation method --------------------------------

test_that("test_mean_directions permutation detects different means", {
  set.seed(1)
  hd <- data.frame(
    cond    = rep(c("A", "B"), each = 35),
    heading = c(as.numeric(circular::rvonmises(35, circular::circular(0),    4)),
                as.numeric(circular::rvonmises(35, circular::circular(pi/2), 4)))
  )
  res <- test_mean_directions(hd, "cond", method = "permutation", n_perm = 999)
  expect_equal(res$test, "permutation")
  expect_true(is.na(res$df1))
  expect_lt(res$p_value, 0.01)
})

test_that("test_mean_directions permutation gives high p for equal means", {
  set.seed(2)
  hd <- data.frame(
    cond    = rep(c("A", "B"), each = 40),
    heading = as.numeric(circular::rvonmises(80, circular::circular(0), 3))
  )
  res <- test_mean_directions(hd, "cond", method = "permutation", n_perm = 999)
  expect_gt(res$p_value, 0.05)
})

test_that("test_mean_directions permutation pairwise with p_adjust", {
  set.seed(3)
  hd <- data.frame(
    cond    = rep(c("A", "B", "C"), each = 30),
    heading = c(as.numeric(circular::rvonmises(30, circular::circular(0),    4)),
                as.numeric(circular::rvonmises(30, circular::circular(pi/2), 4)),
                as.numeric(circular::rvonmises(30, circular::circular(pi),   4)))
  )
  res <- test_mean_directions(hd, "cond", method = "permutation",
                              pairwise = TRUE, p_adjust = "BH", n_perm = 499)
  expect_equal(nrow(res), 3L)
  expect_true(all(c("group1", "group2", "p_value_adj") %in% names(res)))
})

test_that("test_mean_directions default method is still Watson-Williams", {
  set.seed(4)
  hd <- data.frame(
    cond    = rep(c("A", "B"), each = 30),
    heading = as.numeric(circular::rvonmises(60, circular::circular(0), 3))
  )
  res <- test_mean_directions(hd, "cond")
  expect_equal(res$test, "Watson-Williams")
})

# ---- test_uniformity: Monte-Carlo p-values for the tabled tests --------------

test_that("test_uniformity Monte-Carlo gives a continuous p for kuiper/watson/rao", {
  set.seed(1)
  hd <- data.frame(heading = as.numeric(circular::rvonmises(
    50, circular::circular(0), kappa = 4)))
  for (tst in c("kuiper", "watson", "rao")) {
    set.seed(10)
    res <- test_uniformity(hd, test = tst, p_method = "monte_carlo", n_sim = 499)
    expect_equal(res$test, tst)
    expect_false(is.na(res$p_value))          # continuous, not a tabled NA
    expect_lt(res$p_value, 0.01)              # clearly concentrated
    expect_gt(res$p_value, 0)
  }
})

test_that("test_uniformity Monte-Carlo p is reproducible under a fixed seed", {
  hd <- data.frame(heading = as.numeric(circular::rvonmises(
    40, circular::circular(0), kappa = 2)))
  set.seed(7); a <- test_uniformity(hd, test = "kuiper",
                                    p_method = "monte_carlo", n_sim = 299)
  set.seed(7); b <- test_uniformity(hd, test = "kuiper",
                                    p_method = "monte_carlo", n_sim = 299)
  expect_equal(a$p_value, b$p_value)
})

test_that("test_uniformity Monte-Carlo gives high p for uniform data", {
  set.seed(3)
  hd <- data.frame(heading = runif(80, 0, 2 * pi))
  res <- test_uniformity(hd, test = "watson", p_method = "monte_carlo", n_sim = 499)
  expect_gt(res$p_value, 0.05)
})

test_that("test_uniformity Monte-Carlo enables p_adjust across groups", {
  set.seed(5)
  hd <- data.frame(
    grp     = rep(c("A", "B", "C"), each = 30),
    heading = c(as.numeric(circular::rvonmises(30, circular::circular(0), 4)),
                as.numeric(circular::rvonmises(30, circular::circular(pi), 4)),
                runif(30, 0, 2 * pi))
  )
  res <- test_uniformity(hd, group_col = "grp", test = "kuiper",
                         p_method = "monte_carlo", n_sim = 299, p_adjust = "BH")
  expect_true("p_value_adj" %in% names(res))
  expect_true(all(!is.na(res$p_value)))
  expect_true(all(res$p_value_adj >= res$p_value - 1e-9))
})

test_that("test_uniformity default p_method leaves kuiper tabled", {
  set.seed(1)
  hd <- data.frame(heading = as.numeric(circular::rvonmises(
    40, circular::circular(0), kappa = 4)))
  res <- test_uniformity(hd, test = "kuiper")   # default p_method = "table"
  # tabled path: p_value is a tabled level or NA, never a fine-grained MC value
  expect_true(is.na(res$p_value) || res$p_value %in% c(0.01, 0.025, 0.05, 0.10, 0.15))
})

# ---- test_distributions: Monte-Carlo watson_two ------------------------------

test_that("test_distributions watson_two Monte-Carlo gives a continuous p", {
  set.seed(2)
  hd <- data.frame(
    grp = rep(c("A", "B"), each = 40),
    heading = c(as.numeric(circular::rvonmises(40, circular::circular(0),  4)),
                as.numeric(circular::rvonmises(40, circular::circular(pi), 4)))
  )
  set.seed(11)
  res <- test_distributions(hd, "grp", method = "watson_two",
                            p_method = "monte_carlo", n_perm = 499)
  expect_equal(nrow(res), 1L)
  expect_false(is.na(res$p_value))
  expect_lt(res$p_value, 0.01)
})

test_that("test_distributions watson_two Monte-Carlo high p for same distribution", {
  set.seed(8)
  hd <- data.frame(
    grp = rep(c("A", "B"), each = 50),
    heading = as.numeric(circular::rvonmises(100, circular::circular(0), 3))
  )
  res <- test_distributions(hd, "grp", method = "watson_two",
                            p_method = "monte_carlo", n_perm = 499)
  expect_gt(res$p_value, 0.05)
})

test_that("test_distributions watson_two Monte-Carlo is reproducible", {
  set.seed(4)
  hd <- data.frame(
    grp = rep(c("A", "B"), each = 30),
    heading = c(as.numeric(circular::rvonmises(30, circular::circular(0),    3)),
                as.numeric(circular::rvonmises(30, circular::circular(pi/2), 3)))
  )
  set.seed(21); a <- test_distributions(hd, "grp", method = "watson_two",
                                        p_method = "monte_carlo", n_perm = 299)
  set.seed(21); b <- test_distributions(hd, "grp", method = "watson_two",
                                        p_method = "monte_carlo", n_perm = 299)
  expect_equal(a$p_value, b$p_value)
})

# ---- test_symmetry -----------------------------------------------------------

test_that("test_symmetry does not reject a symmetric von Mises", {
  set.seed(2)
  hd <- data.frame(heading = as.numeric(circular::rvonmises(
    300, circular::circular(0), kappa = 2)))
  res <- test_symmetry(hd)
  expect_equal(res$test, "reflective_symmetry")
  expect_equal(res$n, 300L)
  expect_gt(res$p_value, 0.05)
})

test_that("test_symmetry rejects a clearly asymmetric distribution", {
  set.seed(3)
  hd <- data.frame(heading = c(
    as.numeric(circular::rvonmises(240, circular::circular(0),   4)),
    as.numeric(circular::rvonmises(60,  circular::circular(1.4), 6))))
  res <- test_symmetry(hd)
  expect_lt(res$p_value, 0.01)
  expect_gt(res$statistic, 0)
})

test_that("test_symmetry matches the Pewsey reference statistic", {
  set.seed(1)
  x  <- as.numeric(circular::rvonmises(200, circular::circular(0.7), 3))
  mu <- atan2(mean(sin(x)), mean(cos(x))); cen <- x - mu
  a2 <- mean(cos(2 * cen)); a3 <- mean(cos(3 * cen)); a4 <- mean(cos(4 * cen))
  b2 <- mean(sin(2 * cen)); r <- mean(cos(cen)); n <- length(x)
  v  <- ((1 - a4) / 2 - 2 * a2 + (2 * a2 / r) * (a3 + a2 * (1 - a2) / r)) / n
  ts_ref <- abs(b2 / sqrt(v))
  res <- test_symmetry(data.frame(heading = x))
  expect_equal(res$statistic, ts_ref, tolerance = 1e-9)
  expect_equal(res$p_value, 2 * stats::pnorm(ts_ref, lower.tail = FALSE),
               tolerance = 1e-9)
})

test_that("test_symmetry groups and applies p_adjust", {
  set.seed(4)
  hd <- data.frame(
    grp = rep(c("sym", "skew"), times = c(300, 300)),
    heading = c(
      as.numeric(circular::rvonmises(300, circular::circular(0), 2)),
      c(as.numeric(circular::rvonmises(240, circular::circular(0),   4)),
        as.numeric(circular::rvonmises(60,  circular::circular(1.4), 6))))
  )
  res <- test_symmetry(hd, group_col = "grp", p_adjust = "BH")
  expect_equal(nrow(res), 2L)
  expect_true("p_value_adj" %in% names(res))
  expect_gt(res$p_value[res$grp == "sym"],  0.05)
  expect_lt(res$p_value[res$grp == "skew"], 0.05)
})

test_that("test_symmetry returns an NA row for too few observations", {
  res <- test_symmetry(data.frame(heading = c(0.1, 0.4, 1.2)))
  expect_true(is.na(res$statistic))
  expect_equal(res$n, 3L)
})

test_that("test_symmetry axial does not reject a symmetric axial sample", {
  set.seed(5)
  hd <- data.frame(heading = c(
    as.numeric(circular::rvonmises(150, circular::circular(0),  5)),
    as.numeric(circular::rvonmises(150, circular::circular(pi), 5))) %% (2 * pi))
  res <- test_symmetry(hd, axial = TRUE)
  expect_gt(res$p_value, 0.05)
})

# ---- test_unimodality --------------------------------------------------------

test_that("test_unimodality does not reject a concentrated unimodal sample", {
  set.seed(11)
  hd <- data.frame(heading = as.numeric(circular::rvonmises(
    60, circular::circular(0), kappa = 5)))
  res <- test_unimodality(hd, n_boot = 49)
  expect_equal(res$test, "unimodality_lrt")
  expect_equal(res$n, 60L)
  expect_gte(res$statistic, 0)
  expect_gt(res$p_value, 0.05)
})

test_that("test_unimodality rejects a clearly bimodal sample", {
  set.seed(12)
  hd <- data.frame(heading = c(
    as.numeric(circular::rvonmises(50, circular::circular(0),  8)),
    as.numeric(circular::rvonmises(50, circular::circular(pi), 8))))
  res <- test_unimodality(hd, n_boot = 99)
  expect_lt(res$p_value, 0.05)
  expect_gt(res$statistic, 0)
})

test_that("test_unimodality returns an NA row for too few observations", {
  res <- test_unimodality(data.frame(heading = runif(6, 0, 2 * pi)), n_boot = 19)
  expect_true(is.na(res$statistic))
  expect_true(is.na(res$p_value))
  expect_equal(res$n, 6L)
})

test_that("test_unimodality is reproducible under a fixed seed", {
  hd <- data.frame(heading = as.numeric(circular::rvonmises(
    40, circular::circular(0.5), kappa = 3)))
  set.seed(5); a <- test_unimodality(hd, n_boot = 39)
  set.seed(5); b <- test_unimodality(hd, n_boot = 39)
  expect_equal(a$p_value, b$p_value)
})

# ---- boot_kappa_ci -----------------------------------------------------------

test_that("boot_kappa_ci recovers concentration and brackets it", {
  set.seed(11)
  hd <- data.frame(heading = as.numeric(circular::rvonmises(
    80, circular::circular(1), kappa = 8)))
  res <- boot_kappa_ci(hd, R = 499)
  expect_equal(res$n, 80L)
  expect_gt(res$resultant_R, 0.8)
  expect_lt(res$kappa_ci_lo, res$kappa)
  expect_gt(res$kappa_ci_hi, res$kappa)
  expect_lt(res$R_ci_lo, res$resultant_R)
  expect_gt(res$R_ci_hi, res$resultant_R)
  expect_true(is.finite(res$kappa_bias))
  expect_true(is.finite(res$R_bias))
})

test_that("boot_kappa_ci gives a wider kappa CI at lower concentration", {
  set.seed(12)
  hi <- data.frame(heading = as.numeric(circular::rvonmises(
    80, circular::circular(1), kappa = 20)))
  lo <- data.frame(heading = as.numeric(circular::rvonmises(
    80, circular::circular(1), kappa = 2)))
  w_hi <- with(boot_kappa_ci(hi, R = 499), R_ci_hi - R_ci_lo)
  w_lo <- with(boot_kappa_ci(lo, R = 499), R_ci_hi - R_ci_lo)
  expect_gt(w_lo, w_hi)
})

test_that("boot_kappa_ci is reproducible under a fixed seed", {
  hd <- data.frame(heading = as.numeric(circular::rvonmises(
    50, circular::circular(0.5), kappa = 5)))
  set.seed(77); a <- boot_kappa_ci(hd, R = 299)
  set.seed(77); b <- boot_kappa_ci(hd, R = 299)
  expect_equal(a, b)
})

test_that("boot_kappa_ci groups by condition", {
  set.seed(13)
  hd <- data.frame(
    grp     = rep(c("A", "B"), each = 50),
    heading = c(as.numeric(circular::rvonmises(50, circular::circular(0), 10)),
                as.numeric(circular::rvonmises(50, circular::circular(0), 2)))
  )
  res <- boot_kappa_ci(hd, group_col = "grp", R = 299)
  expect_equal(nrow(res), 2L)
  expect_true("grp" %in% names(res))
  expect_true(all(c("kappa_bias", "R_bias") %in% names(res)))
})

test_that("boot_kappa_ci returns an NA row for n < 2", {
  hd <- data.frame(heading = 0.3)
  res <- boot_kappa_ci(hd, R = 99)
  expect_true(is.na(res$kappa))
  expect_true(is.na(res$resultant_R))
  expect_equal(res$n, 1L)
})

test_that("boot_kappa_ci axial returns finite concentration", {
  set.seed(14)
  hd <- data.frame(heading = c(rnorm(50, 0, 0.3),
                               rnorm(50, pi, 0.3)) %% (2 * pi))
  res <- boot_kappa_ci(hd, R = 299, axial = TRUE)
  expect_true(is.finite(res$kappa))
  expect_gt(res$resultant_R, 0.5)
})

# ---- boot_kappa_contrast -----------------------------------------------------

test_that("boot_kappa_contrast CI contains 0 for equal concentrations", {
  set.seed(21)
  hd <- data.frame(
    grp     = rep(c("A", "B"), each = 80),
    heading = c(as.numeric(circular::rvonmises(80, circular::circular(0), 6)),
                as.numeric(circular::rvonmises(80, circular::circular(0), 6)))
  )
  res <- boot_kappa_contrast(hd, group_col = "grp", R = 499)
  expect_equal(nrow(res), 1L)
  expect_lt(res$delta_kappa_ci_lo, 0)
  expect_gt(res$delta_kappa_ci_hi, 0)
  expect_gt(res$p_value, 0.05)
  expect_lt(abs(res$kappa_ratio - 1), 0.8)
})

test_that("boot_kappa_contrast detects a large concentration difference", {
  set.seed(22)
  hd <- data.frame(
    grp     = rep(c("A", "B"), each = 80),
    heading = c(as.numeric(circular::rvonmises(80, circular::circular(0), 20)),
                as.numeric(circular::rvonmises(80, circular::circular(0), 1)))
  )
  res <- boot_kappa_contrast(hd, group_col = "grp", R = 499)
  expect_gt(res$delta_kappa_ci_lo, 0)
  expect_lt(res$p_value, 0.05)
})

test_that("boot_kappa_contrast returns all pairs for >2 groups", {
  set.seed(23)
  hd <- data.frame(
    grp     = rep(c("A", "B", "C"), each = 40),
    heading = as.numeric(circular::rvonmises(120, circular::circular(0), 5))
  )
  res <- boot_kappa_contrast(hd, group_col = "grp", R = 199)
  expect_equal(nrow(res), 3L)
  expect_true(all(c("group1", "group2") %in% names(res)))
})

test_that("boot_kappa_contrast is reproducible under a fixed seed", {
  set.seed(24)
  hd <- data.frame(
    grp     = rep(c("A", "B"), each = 50),
    heading = as.numeric(circular::rvonmises(100, circular::circular(0), 5))
  )
  set.seed(88); a <- boot_kappa_contrast(hd, group_col = "grp", R = 199)
  set.seed(88); b <- boot_kappa_contrast(hd, group_col = "grp", R = 199)
  expect_equal(a, b)
})

test_that("boot_kappa_contrast errors on fewer than two groups", {
  hd <- data.frame(grp = rep("A", 20),
                   heading = as.numeric(circular::rvonmises(
                     20, circular::circular(0), 5)))
  expect_error(boot_kappa_contrast(hd, group_col = "grp", R = 99),
               "at least two groups")
})


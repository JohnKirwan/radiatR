test_that("derive_headings crossing rule returns heading angle", {
  # Straight-line trajectory heading NE, crosses circ0=0.2 then circ1=0.4.
  # normalize_xy=FALSE preserves raw coords; no angle arg avoids time-column overwrite.
  theta <- pi / 4
  n <- 20
  r <- seq(0, 0.8, length.out = n)
  df <- data.frame(id = "A", time = seq_len(n),
                   x = r * cos(theta), y = r * sin(theta))
  ts <- TrajSet(df, id = "id", time = "time", x = "x", y = "y",
                normalize_xy = FALSE)
  hd <- derive_headings(ts, rule = "crossing", circ0 = 0.2, circ1 = 0.4)
  expect_equal(nrow(hd), 1)
  expect_named(hd, c("id", "time", "heading"))
  expect_equal(hd$heading, theta %% (2 * pi), tolerance = 1e-6)
})

test_that("derive_headings crossing rule with return_coords adds inner/outer crossings", {
  theta <- pi / 4
  n <- 20
  r <- seq(0, 0.8, length.out = n)
  df <- data.frame(id = "A", time = seq_len(n),
                   x = r * cos(theta), y = r * sin(theta))
  ts <- TrajSet(df, id = "id", time = "time", x = "x", y = "y",
                normalize_xy = FALSE)
  hd <- derive_headings(ts, rule = "crossing", circ0 = 0.2, circ1 = 0.4,
                        return_coords = TRUE)
  expect_named(hd, c("id", "time", "heading", "x_inner", "y_inner",
                     "x_outer", "y_outer"))
  r_inner <- sqrt(hd$x_inner^2 + hd$y_inner^2)
  expect_equal(r_inner, 0.2, tolerance = 1e-6)
  expect_equal(atan2(hd$y_inner, hd$x_inner), theta, tolerance = 1e-6)
  r_outer <- sqrt(hd$x_outer^2 + hd$y_outer^2)
  expect_equal(r_outer, 0.4, tolerance = 1e-6)
  expect_equal(atan2(hd$y_outer, hd$x_outer), theta, tolerance = 1e-6)
})

test_that("crossing heading is the boundary projection, not the segment slope", {
  # Horizontal track offset in y so the crossing line misses the origin: the
  # segment slope is due-East (0), but the projection to the unit circle is not.
  y0 <- 0.1
  xs <- seq(0.05, 0.7, length.out = 40)
  df <- data.frame(id = "A", time = seq_along(xs), x = xs, y = y0)
  ts <- TrajSet(df, id = "id", time = "time", x = "x", y = "y",
                normalize_xy = FALSE)
  hd <- derive_headings(ts, rule = "crossing", circ0 = 0.2, circ1 = 0.4,
                        return_coords = TRUE)
  expect_equal(nrow(hd), 1)
  expect_true(is.finite(hd$heading))

  c0 <- c(hd$x_inner, hd$y_inner)
  c1 <- c(hd$x_outer, hd$y_outer)
  p  <- c(cos(hd$heading), sin(hd$heading))

  # the heading point is on the unit circle (the arena boundary)
  expect_equal(sqrt(sum(p^2)), 1, tolerance = 1e-8)
  # p is collinear with the inner->outer line (cross product ~ 0)
  cross <- (c1[1] - c0[1]) * (p[2] - c0[2]) - (c1[2] - c0[2]) * (p[1] - c0[1])
  expect_equal(cross, 0, tolerance = 1e-8)
  # p is the forward intersection (same side as the outer crossing)
  expect_gt(sum((p - c0) * (c1 - c0)), 0)
  # and it differs from the old segment-slope definition (this is the fix)
  slope <- atan2(c1[2] - c0[2], c1[1] - c0[1]) %% (2 * pi)
  ang_diff <- abs(((hd$heading - slope + pi) %% (2 * pi)) - pi)
  expect_gt(ang_diff, 0.02)
})

test_that("derive_headings crossing without crossing returns NA row", {
  # Trajectory that never leaves circ0=0.2; normalize_xy=FALSE keeps raw coords.
  df <- data.frame(id = "A", time = 1:5,
                   x = c(0, 0.05, 0.1, 0.05, 0), y = rep(0, 5))
  ts <- TrajSet(df, id = "id", time = "time", x = "x", y = "y",
                normalize_xy = FALSE)
  hd <- derive_headings(ts, rule = "crossing", circ0 = 0.2, circ1 = 0.4,
                        on_missing = "quiet")
  expect_equal(nrow(hd), 1)
  expect_true(is.na(hd$heading))
})

# ---- multi-trajectory crossing accuracy -------------------------------------

# Helper: build a TrajSet of N straight-line trajectories, each heading at
# a different angle.  normalize_xy=FALSE keeps raw coordinates intact.
make_multi_crossing_ts <- function(angles, n = 20, r_max = 0.8) {
  r <- seq(0, r_max, length.out = n)
  rows <- do.call(rbind, lapply(seq_along(angles), function(i) {
    th <- angles[i]
    data.frame(id = paste0("T", i), time = seq_len(n),
               x  = r * cos(th), y = r * sin(th))
  }))
  TrajSet(rows, id = "id", time = "time", x = "x", y = "y",
          normalize_xy = FALSE)
}

test_that("crossing rule recovers correct heading for each of N trajectories", {
  # Four cardinal directions; each should produce the correct angle after wrapping.
  angles <- c(0, pi / 2, pi, 3 * pi / 2)
  ts  <- make_multi_crossing_ts(angles)
  hd  <- derive_headings(ts, rule = "crossing", circ0 = 0.2, circ1 = 0.4)

  expect_equal(nrow(hd), 4)
  # align by id order so comparison is deterministic
  hd_s <- hd[order(hd$id), ]
  expect_equal(hd_s$heading, angles %% (2 * pi), tolerance = 1e-6)
})

test_that("return_coords places inner crossing on circ0 circle in correct direction", {
  angles <- c(pi / 6, pi / 2, 5 * pi / 6)
  ts <- make_multi_crossing_ts(angles)
  hd <- derive_headings(ts, rule = "crossing", circ0 = 0.2, circ1 = 0.4,
                        return_coords = TRUE)

  hd_s <- hd[order(hd$id), ]
  # radius of each inner crossing should equal circ0
  r_inner <- sqrt(hd_s$x_inner^2 + hd_s$y_inner^2)
  expect_equal(r_inner, rep(0.2, 3), tolerance = 1e-6)
  # direction of inner crossing should match heading
  inner_angle <- atan2(hd_s$y_inner, hd_s$x_inner) %% (2 * pi)
  expect_equal(inner_angle, angles %% (2 * pi), tolerance = 1e-6)
})

# ---- circ_summary_headings accuracy -----------------------------------------

test_that("circ_summary_headings mean_dir and resultant_R are analytically correct", {
  # Three headings: pi/6, pi/2, 5*pi/6
  #   mean_cos = (cos(pi/6) + cos(pi/2) + cos(5*pi/6)) / 3
  #            = (sqrt(3)/2 + 0 - sqrt(3)/2) / 3 = 0
  #   mean_sin = (sin(pi/6) + sin(pi/2) + sin(5*pi/6)) / 3
  #            = (1/2 + 1 + 1/2) / 3 = 2/3
  #   mean_dir = atan2(2/3, 0) = pi/2;  resultant_R = 2/3
  angles <- c(pi / 6, pi / 2, 5 * pi / 6)
  ts <- make_multi_crossing_ts(angles)

  summ <- circ_summary_headings(ts, rule = "crossing",
                                circ0 = 0.2, circ1 = 0.4,
                                group_by = NULL)

  expect_equal(summ$resultant_R,              2 / 3, tolerance = 1e-6)
  expect_equal(summ$mean_dir %% (2 * pi), pi / 2,   tolerance = 1e-6)
})

test_that("circ_summary_headings gives resultant_R = 1 when all headings are identical", {
  angles <- rep(pi / 3, 4)
  ts <- make_multi_crossing_ts(angles)

  summ <- circ_summary_headings(ts, rule = "crossing",
                                circ0 = 0.2, circ1 = 0.4,
                                group_by = NULL)

  expect_equal(summ$resultant_R,             1,       tolerance = 1e-6)
  expect_equal(summ$mean_dir %% (2 * pi), pi / 3,    tolerance = 1e-6)
})

test_that("circ_summary_headings gives resultant_R near 0 for uniformly spread headings", {
  # Four headings evenly at 0, pi/2, pi, 3*pi/2 cancel perfectly.
  angles <- c(0, pi / 2, pi, 3 * pi / 2)
  ts <- make_multi_crossing_ts(angles)

  summ <- circ_summary_headings(ts, rule = "crossing",
                                circ0 = 0.2, circ1 = 0.4,
                                group_by = NULL)

  expect_equal(summ$resultant_R, 0, tolerance = 1e-6)
})

test_that("derive_headings computes simple net direction", {
  df <- data.frame(
    id = c("A", "A", "A"),
    time = c(0, 1, 2),
    x = c(0, 0, 1),
    y = c(0, 1, 1)
  )
  ts <- TrajSet(df, id = "id", time = "time", x = "x", y = "y", angle = "time", angle_unit = "radians")
  headings <- derive_headings(ts, rule = "net")
  expect_equal(nrow(headings), 1)
  expect_equal(headings$heading, atan2(1, 1), tolerance = 1e-8)
})

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

test_that("custom heading rules can be registered and listed", {
  custom_rule <- function(d, cols, ...) {
    data.frame(
      id = d[[cols$id]][1],
      time = d[[cols$time]][1],
      heading = 0
    )
  }
  register_heading_rule("zero_heading", custom_rule, overwrite = TRUE)
  registry <- get(".heading_registry", envir = asNamespace("radiatR"))
  withr::defer(rm(list = "zero_heading", envir = registry), envir = parent.frame())

  df <- data.frame(id = "A", time = 0:1, x = c(0, 1), y = c(0, 0))
  ts <- TrajSet(df, id = "id", time = "time", x = "x", y = "y", angle = "time", angle_unit = "radians")
  res <- derive_headings(ts, rule = "zero_heading")
  expect_equal(res$heading, 0)
  expect_true("zero_heading" %in% list_heading_rules())
})

# ---- coords parameter tests --------------------------------------------------

# Helper: TrajSet with both abs and rel coord columns
make_ts_with_rel <- function() {
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
                        coords = "absolute")
  expect_equal(hd$heading, 0, tolerance = 1e-6)  # East
})

test_that("derive_headings coords='relative' uses rel_x/rel_y columns", {
  ts <- make_ts_with_rel()
  hd <- derive_headings(ts, rule = "crossing", circ0 = 0.2, circ1 = 0.4,
                        coords = "relative")
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




# ---- bodypart_axis heading rule ----------------------------------------------

make_ts_with_bodyparts <- function() {
  # head directly north of thorax at each frame (same x, head_y = thorax_y + 0.1)
  df <- data.frame(
    id       = "t1",
    time     = 1:5,
    x        = seq(0, 0.4, by = 0.1),
    y        = rep(0, 5),
    head_x   = seq(0, 0.4, by = 0.1),   # same x as thorax
    head_y   = rep(0.1, 5),
    thorax_x = seq(0, 0.4, by = 0.1),
    thorax_y = rep(0, 5)
  )
  TrajSet_read(df, mapping = list(id = "id", time = "time", x = "x", y = "y"),
               keep = c("head_x", "head_y", "thorax_x", "thorax_y"),
               normalize_xy = FALSE)
}

test_that("bodypart_axis is registered", {
  expect_true("bodypart_axis" %in% list_heading_rules())
})

test_that("bodypart_axis heading points from posterior to anterior", {
  ts <- make_ts_with_bodyparts()
  hd <- derive_headings(ts, rule = "bodypart_axis",
                        anterior = "head", posterior = "thorax",
                        coords = "absolute")
  expect_equal(nrow(hd), 1L)
  # head_y - thorax_y = 0.1, head_x - thorax_x = 0.1 → atan2(0.1,0) = pi/2
  expect_equal(hd$heading[1], pi / 2, tolerance = 1e-9)
})

test_that("bodypart_axis frame_select='last' uses final frame", {
  df <- data.frame(id = "t1", time = 1:4, x = 1:4, y = rep(0,4),
                   head_x = c(1,2,3,4), head_y = rep(1, 4),
                   tail_x = c(1,2,3,4), tail_y = rep(0, 4))
  ts <- TrajSet_read(df, mapping = list(id="id", time="time", x="x", y="y"),
                     keep = c("head_x","head_y","tail_x","tail_y"),
                     normalize_xy = FALSE)
  hd <- derive_headings(ts, rule = "bodypart_axis",
                        anterior = "head", posterior = "tail",
                        frame_select = "last",
                        coords = "absolute")
  expect_equal(hd$time[1], 4)
})

test_that("bodypart_axis errors when bodypart columns are absent", {
  ts <- make_ts_with_bodyparts()
  expect_error(
    derive_headings(ts, rule = "bodypart_axis",
                    anterior = "nose", posterior = "tail",
                    coords = "absolute"),
    "not found"
  )
})

# ---- ellipse_axis heading rule -----------------------------------------------

test_that("ellipse_axis is registered", {
  expect_true("ellipse_axis" %in% list_heading_rules())
})

test_that("ellipse_axis returns theta at distal frame", {
  df <- data.frame(id = "t1", time = 1:5,
                   x = c(0, .2, .4, .6, .8), y = rep(0, 5),
                   theta = c(0, pi/6, pi/4, pi/3, pi/2))
  ts <- TrajSet_read(df, mapping = list(id="id",time="time",x="x",y="y"),
                     normalize_xy = FALSE)
  hd <- derive_headings(ts, rule = "ellipse_axis",
                        coords = "absolute")
  # frame 5 is most distal (x=0.8); theta there is pi/2
  expect_equal(hd$heading[1], pi / 2, tolerance = 1e-9)
})

test_that("ellipse_axis frame_select='mean' gives circular mean of theta", {
  df <- data.frame(id = "t1", time = 1:4, x = rep(0.5, 4), y = rep(0, 4),
                   theta = c(0, 0, 0, 0))
  ts <- TrajSet_read(df, mapping = list(id="id",time="time",x="x",y="y"),
                     normalize_xy = FALSE)
  hd <- derive_headings(ts, rule = "ellipse_axis", frame_select = "mean",
                        coords = "absolute")
  expect_equal(hd$heading[1], 0, tolerance = 1e-9)
})

test_that("ellipse_axis errors when theta column absent", {
  df <- data.frame(id = "t1", time = 1:3, x = 1:3, y = rep(0,3))
  ts <- TrajSet_read(df, mapping = list(id="id",time="time",x="x",y="y"),
                     normalize_xy = FALSE)
  expect_error(
    derive_headings(ts, rule = "ellipse_axis",
                    coords = "absolute"),
    "not found"
  )
})

# ---- frame_select = "all" ----------------------------------------------------

test_that("bodypart_axis frame_select='all' returns one row per frame", {
  df <- data.frame(id = "t1", time = 1:5, x = seq(0, .4, .1), y = rep(0, 5),
                   head_x = seq(0, .4, .1), head_y = rep(.1, 5),
                   thorax_x = seq(0, .4, .1), thorax_y = rep(0, 5))
  ts <- TrajSet_read(df, mapping = list(id="id",time="time",x="x",y="y"),
                     keep = c("head_x","head_y","thorax_x","thorax_y"),
                     normalize_xy = FALSE)
  hd <- derive_headings(ts, rule = "bodypart_axis",
                        anterior = "head", posterior = "thorax",
                        frame_select = "all",
                        coords = "absolute")
  expect_equal(nrow(hd), 5L)
  expect_true(all(c("id","time","heading") %in% names(hd)))
  expect_equal(hd$heading, rep(pi/2, 5), tolerance = 1e-9)
})

test_that("ellipse_axis frame_select='all' returns one row per frame", {
  df <- data.frame(id = "t1", time = 1:4, x = 1:4, y = rep(0,4),
                   theta = rep(pi/3, 4))
  ts <- TrajSet_read(df, mapping = list(id="id",time="time",x="x",y="y"),
                     normalize_xy = FALSE)
  hd <- derive_headings(ts, rule = "ellipse_axis", frame_select = "all",
                        coords = "absolute")
  expect_equal(nrow(hd), 4L)
  expect_equal(hd$heading, rep(pi/3, 4), tolerance = 1e-9)
})

# ---- pose_to_headings --------------------------------------------------------

test_that("pose_to_headings computes bodypart axis heading for all frames", {
  df <- data.frame(id = "t1", frame = 1:4,
                   head_x = c(0,.1,.2,.3), head_y = rep(.1, 4),
                   thorax_x = c(0,.1,.2,.3), thorax_y = rep(0, 4))
  hd <- pose_to_headings(df, anterior = "head", posterior = "thorax")
  expect_equal(nrow(hd), 4L)
  expect_equal(hd$heading, rep(pi/2, 4), tolerance = 1e-9)
  expect_equal(attr(hd, "angle_convention"), "unit_circle")
})

test_that("pose_to_headings uses theta_col directly", {
  df <- data.frame(trial = "A", t = 1:3, theta = c(0, pi/4, pi/2))
  hd <- pose_to_headings(df, theta_col = "theta",
                         id_col = "trial", time_col = "t")
  expect_equal(hd$id, rep("A", 3))
  expect_equal(hd$heading, c(0, pi/4, pi/2), tolerance = 1e-9)
})

test_that("pose_to_headings handles no id/time columns by defaulting", {
  df <- data.frame(head_x = c(1,1), head_y = c(0,0),
                   tail_x = c(0,0), tail_y = c(0,0))
  hd <- pose_to_headings(df, anterior = "head", posterior = "tail")
  expect_equal(hd$id, rep("1", 2))
  expect_equal(hd$time, 1:2)
})

test_that("pose_to_headings clock convention flips axis correctly", {
  df <- data.frame(head_x = 0, head_y = 1, tail_x = 0, tail_y = 0)
  hd_uc <- pose_to_headings(df, anterior="head", posterior="tail",
                             angle_convention="unit_circle")
  hd_cl <- pose_to_headings(df, anterior="head", posterior="tail",
                             angle_convention="clock")
  expect_equal(hd_uc$heading, pi/2, tolerance=1e-9)  # pointing East
  expect_equal(hd_cl$heading, 0,    tolerance=1e-9)  # pointing North = 0 in clock
})

# ---- attrition reporting (on_missing) ---------------------------------------

# A TrajSet of 5 trials: 3 reach r=0.8 (cross circ0/circ1), 2 stay central
# (max r = 0.3 < circ1 = 0.4) so the crossing rule yields NA for them.
make_attrition_ts <- function() {
  mk <- function(id, theta, r_max, n = 15) {
    r <- seq(0, r_max, length.out = n)
    data.frame(id = id, time = seq_len(n),
               x = r * cos(theta), y = r * sin(theta))
  }
  rows <- rbind(
    mk("cross1", 0.3, 0.8), mk("cross2", 1.2, 0.8), mk("cross3", 2.5, 0.8),
    mk("central1", 0.5, 0.30), mk("central2", 1.8, 0.30)
  )
  TrajSet(rows, id = "id", time = "time", x = "x", y = "y", normalize_xy = FALSE)
}

test_that("derive_headings warns and reports attrition by default", {
  ts <- make_attrition_ts()
  expect_warning(
    hd <- derive_headings(ts, rule = "crossing", circ0 = 0.2, circ1 = 0.4),
    "produced no heading"
  )
  expect_equal(nrow(hd), 5L)                          # NA rows retained
  expect_equal(attr(hd, "n_total"), 5L)
  expect_equal(attr(hd, "n_missing"), 2L)
  expect_setequal(attr(hd, "missing_ids"), c("central1", "central2"))
})

test_that("derive_headings on_missing = 'error' stops", {
  ts <- make_attrition_ts()
  expect_error(
    derive_headings(ts, rule = "crossing", circ0 = 0.2, circ1 = 0.4,
                    on_missing = "error"),
    "produced no heading"
  )
})

test_that("derive_headings on_missing = 'quiet' is silent but still attaches attrs", {
  ts <- make_attrition_ts()
  expect_silent(
    hd <- derive_headings(ts, rule = "crossing", circ0 = 0.2, circ1 = 0.4,
                          on_missing = "quiet")
  )
  expect_equal(attr(hd, "n_missing"), 2L)
  expect_equal(attr(hd, "n_total"), 5L)
})

test_that("derive_headings does not warn when every trial yields a heading", {
  ts <- make_attrition_ts()
  # distal always produces a heading (furthest point) -> no attrition
  expect_silent(hd <- derive_headings(ts, rule = "distal"))
  expect_equal(attr(hd, "n_missing"), 0L)
})

test_that("velocity_axis returns the axial mean of step directions", {
  set.seed(1)
  ang <- c(rnorm(30, 30, 4), rnorm(30, 210, 4)) * pi/180   # back-and-forth ~30 deg
  x <- cumsum(c(0, cos(ang))); y <- cumsum(c(0, sin(ang)))
  ts <- TrajSet(data.frame(id = "t", time = seq_along(x), x = x, y = y),
                id = "id", time = "time", x = "x", y = "y", normalize_xy = FALSE)
  hd <- derive_headings(ts, rule = "velocity_axis")
  manual <- (Arg(mean(exp(1i * 2 * ang))) / 2) %% pi
  expect_equal(nrow(hd), 1L)
  expect_equal(as.numeric(hd$heading), manual, tolerance = 1e-8)
  expect_true(hd$heading >= 0 && hd$heading < pi)
})

test_that("velocity_axis differs from velocity_mean on bidirectional data", {
  set.seed(2)
  ang <- c(rnorm(30, 40, 4), rnorm(30, 220, 4)) * pi/180
  x <- cumsum(c(0, cos(ang))); y <- cumsum(c(0, sin(ang)))
  ts <- TrajSet(data.frame(id = "t", time = seq_along(x), x = x, y = y),
                id = "id", time = "time", x = "x", y = "y", normalize_xy = FALSE)
  ax <- derive_headings(ts, rule = "velocity_axis")$heading
  dm <- derive_headings(ts, rule = "velocity_mean")$heading
  expect_false(isTRUE(all.equal(as.numeric(ax), as.numeric(dm))))
})

test_that("velocity_axis weight_by changes the result on unequal steps", {
  ts <- TrajSet(data.frame(id = "t", time = 1:3, x = c(0, 10, 10), y = c(0, 0, 1)),
                id = "id", time = "time", x = "x", y = "y", normalize_xy = FALSE)
  a <- derive_headings(ts, rule = "velocity_axis", weight_by = "step_length")$heading
  b <- derive_headings(ts, rule = "velocity_axis", weight_by = "uniform")$heading
  expect_false(isTRUE(all.equal(as.numeric(a), as.numeric(b))))
})

test_that("velocity_axis on a single-point trajectory yields NA", {
  ts <- TrajSet(data.frame(id = "t", time = 1, x = 0.2, y = 0.1),
                id = "id", time = "time", x = "x", y = "y", normalize_xy = FALSE)
  hd <- suppressWarnings(derive_headings(ts, rule = "velocity_axis"))
  expect_true(is.na(hd$heading))
})

test_that("velocity_axis on cpunctatus yields one axis per trial in [0, pi)", {
  hd <- suppressWarnings(derive_headings(cpunctatus, rule = "velocity_axis"))
  expect_equal(nrow(hd), length(ids(cpunctatus)))
  fin <- hd$heading[is.finite(hd$heading)]
  expect_true(length(fin) > 0 && all(fin >= 0 & fin < pi))
})

test_that("derive_headings returns a headings_frame that survives a dplyr pipe", {
  data(cpunctatus, package = "radiatR", envir = environment())
  hd <- derive_headings(cpunctatus, rule = "distal")
  expect_s3_class(hd, "headings_frame")
  expect_s3_class(hf_display(hd), "circ_display")
  hd2 <- dplyr::filter(hd, is.finite(heading))
  expect_s3_class(hd2, "headings_frame")
  expect_equal(hf_display(hd2)$zero, hf_display(hd)$zero)
})

test_that("a plain data frame with a heading column still flows through the API", {
  df <- data.frame(id = "a", time = 1:5, heading = seq(0.1, 0.5, length.out = 5))
  expect_s3_class(circ_summarise(df, "heading", units = "radians"), "data.frame")
  expect_silent(circ_boxplot_stats(df))
  lyr <- add_heading_points(df)
  expect_true(inherits(lyr, "Layer"))
})

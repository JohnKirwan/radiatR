test_that("derive_headings crossing rule returns heading angle", {
  # Straight-line trajectory heading NE, crosses circ0=0.2 then circ1=0.4.
  # normalize_xy=FALSE preserves raw coords; no angle arg avoids time-column overwrite.
  theta <- pi / 4
  n <- 20
  r <- seq(0, 0.8, length.out = n)
  df <- data.frame(id = "A", time = seq_len(n),
                   x = r * cos(theta), y = r * sin(theta))
  ts <- tracks(df, id = "id", time = "time", x = "x", y = "y",
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
  ts <- tracks(df, id = "id", time = "time", x = "x", y = "y",
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
  ts <- tracks(df, id = "id", time = "time", x = "x", y = "y",
                normalize_xy = FALSE)
  hd <- derive_headings(ts, rule = "crossing", circ0 = 0.2, circ1 = 0.4,
                        return_coords = TRUE)
  expect_equal(nrow(hd), 1)
  expect_true(is.finite(hd$heading))

  c0 <- c(hd$x_inner, hd$y_inner)
  c1 <- c(hd$x_outer, hd$y_outer)
  p  <- c(cos(hd$heading), sin(hd$heading))

  # the heading point is on the unit circle (the circumference)
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
  ts <- tracks(df, id = "id", time = "time", x = "x", y = "y",
                normalize_xy = FALSE)
  hd <- derive_headings(ts, rule = "crossing", circ0 = 0.2, circ1 = 0.4,
                        on_missing = "quiet")
  expect_equal(nrow(hd), 1)
  expect_true(is.na(hd$heading))
})

# ---- multi-trajectory crossing accuracy -------------------------------------

# Helper: build a Tracks of N straight-line trajectories, each heading at
# a different angle.  normalize_xy=FALSE keeps raw coordinates intact.
make_multi_crossing_ts <- function(angles, n = 20, r_max = 0.8) {
  r <- seq(0, r_max, length.out = n)
  rows <- do.call(rbind, lapply(seq_along(angles), function(i) {
    th <- angles[i]
    data.frame(id = paste0("T", i), time = seq_len(n),
               x  = r * cos(th), y = r * sin(th))
  }))
  tracks(rows, id = "id", time = "time", x = "x", y = "y",
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

test_that("circ_summary_headings by id keeps trials that produced no heading", {
  # Trial "b" never reaches the crossing ring, so it has no heading.
  ts <- tracks(data.frame(id = rep(c("a", "b"), each = 3), time = rep(1:3, 2),
                          x = c(0, 0.3, 0.6, 0, 0.05, 0.1), y = 0),
               id = "id", time = "time", x = "x", y = "y")
  summ <- suppressWarnings(circ_summary_headings(ts, rule = "crossing",
                                                 circ0 = 0.2, circ1 = 0.4))
  expect_equal(summ$id, c("a", "b"))
  expect_equal(summ$n, c(1L, 0L))
  expect_true(is.na(summ$mean_dir[2]))
})

test_that("derive_headings computes simple net direction", {
  df <- data.frame(
    id = c("A", "A", "A"),
    time = c(0, 1, 2),
    x = c(0, 0, 1),
    y = c(0, 1, 1)
  )
  ts <- tracks(df, id = "id", time = "time", x = "x", y = "y", angle = "time", angle_unit = "radians")
  headings <- derive_headings(ts, rule = "net")
  expect_equal(nrow(headings), 1)
  expect_equal(headings$heading, atan2(1, 1), tolerance = 1e-8)
})

test_that("Tracks accepts rel_x/rel_y col pointers", {
  df <- data.frame(id = "A", time = 1:3,
                   x = c(0.1, 0.2, 0.3), y = c(0.0, 0.1, 0.2),
                   rx = c(-0.1, -0.2, -0.3), ry = c(0.0, -0.1, -0.2))
  ts <- tracks(df, id = "id", time = "time", x = "x", y = "y",
                rel_x = "rx", rel_y = "ry", normalize_xy = FALSE)
  expect_equal(ts@cols$rel_x, "rx")
  expect_equal(ts@cols$rel_y, "ry")
})

test_that("Tracks validator rejects rel_x without rel_y", {
  df <- data.frame(id = "A", time = 1:3,
                   x = c(0.1, 0.2, 0.3), y = c(0.0, 0.1, 0.2),
                   rx = c(-0.1, -0.2, -0.3))
  expect_error(
    tracks(df, id = "id", time = "time", x = "x", y = "y",
            rel_x = "rx", normalize_xy = FALSE),
    "rel_y"
  )
})

test_that("Tracks validator rejects rel_x pointing to absent column", {
  df <- data.frame(id = "A", time = 1:3,
                   x = c(0.1, 0.2, 0.3), y = c(0.0, 0.1, 0.2))
  expect_error(
    tracks(df, id = "id", time = "time", x = "x", y = "y",
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
  ts <- tracks(df, id = "id", time = "time", x = "x", y = "y", angle = "time", angle_unit = "radians")
  res <- derive_headings(ts, rule = "zero_heading")
  expect_equal(res$heading, 0)
  expect_true("zero_heading" %in% list_heading_rules())
})

# ---- coords parameter tests --------------------------------------------------

# Helper: Tracks with both abs and rel coord columns
make_ts_with_rel <- function() {
  df <- data.frame(
    id = "A", time = 1:10,
    x  = seq(0, 0.8, length.out = 10),  # heading East in absolute
    y  = rep(0, 10),
    rx = rep(0, 10),                     # heading North in relative
    ry = seq(0, 0.8, length.out = 10)
  )
  tracks(df, id = "id", time = "time", x = "x", y = "y",
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
  ts <- tracks(df, id = "id", time = "time", x = "x", y = "y",
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
  read_tracks(df, mapping = list(id = "id", time = "time", x = "x", y = "y"),
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
  ts <- read_tracks(df, mapping = list(id="id", time="time", x="x", y="y"),
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
  ts <- read_tracks(df, mapping = list(id="id",time="time",x="x",y="y"),
                     normalize_xy = FALSE)
  hd <- derive_headings(ts, rule = "ellipse_axis",
                        coords = "absolute")
  # frame 5 is most distal (x=0.8); theta there is pi/2
  expect_equal(hd$heading[1], pi / 2, tolerance = 1e-9)
})

test_that("ellipse_axis frame_select='mean' gives circular mean of theta", {
  df <- data.frame(id = "t1", time = 1:4, x = rep(0.5, 4), y = rep(0, 4),
                   theta = c(0, 0, 0, 0))
  ts <- read_tracks(df, mapping = list(id="id",time="time",x="x",y="y"),
                     normalize_xy = FALSE)
  hd <- derive_headings(ts, rule = "ellipse_axis", frame_select = "mean",
                        coords = "absolute")
  expect_equal(hd$heading[1], 0, tolerance = 1e-9)
})

test_that("ellipse_axis errors when theta column absent", {
  df <- data.frame(id = "t1", time = 1:3, x = 1:3, y = rep(0,3))
  ts <- read_tracks(df, mapping = list(id="id",time="time",x="x",y="y"),
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
  ts <- read_tracks(df, mapping = list(id="id",time="time",x="x",y="y"),
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
  ts <- read_tracks(df, mapping = list(id="id",time="time",x="x",y="y"),
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

# A Tracks of 5 trials: 3 reach r=0.8 (cross circ0/circ1), 2 stay central
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
  tracks(rows, id = "id", time = "time", x = "x", y = "y", normalize_xy = FALSE)
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
  ts <- tracks(data.frame(id = "t", time = seq_along(x), x = x, y = y),
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
  ts <- tracks(data.frame(id = "t", time = seq_along(x), x = x, y = y),
                id = "id", time = "time", x = "x", y = "y", normalize_xy = FALSE)
  ax <- derive_headings(ts, rule = "velocity_axis")$heading
  dm <- derive_headings(ts, rule = "velocity_mean")$heading
  expect_false(isTRUE(all.equal(as.numeric(ax), as.numeric(dm))))
})

test_that("velocity_axis weight_by changes the result on unequal steps", {
  ts <- tracks(data.frame(id = "t", time = 1:3, x = c(0, 10, 10), y = c(0, 0, 1)),
                id = "id", time = "time", x = "x", y = "y", normalize_xy = FALSE)
  a <- derive_headings(ts, rule = "velocity_axis", weight_by = "step_length")$heading
  b <- derive_headings(ts, rule = "velocity_axis", weight_by = "uniform")$heading
  expect_false(isTRUE(all.equal(as.numeric(a), as.numeric(b))))
})

test_that("velocity_axis on a single-point trajectory yields NA", {
  ts <- tracks(data.frame(id = "t", time = 1, x = 0.2, y = 0.1),
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
  skip_if_not_installed("dplyr")
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

test_that("derive_headings(carry=) preserves the carried column's class/levels", {
  d <- data.frame(
    id   = rep(c("a", "b"), each = 3L),
    t    = rep(1:3, 2L),
    x    = c(0, 0.5, 0.9,  0, -0.5, -0.9),
    y    = c(0, 0.1, 0.0,  0,  0.1,  0.0),
    grp  = factor(rep(c("low", "high"), each = 3L), levels = c("low", "high")),
    val  = rep(c(10, 20), each = 3L),
    lab  = rep(c("x", "y"), each = 3L))
  ts <- tracks(d, id = "id", time = "t", x = "x", y = "y",
               angle_unit = "radians", normalize_xy = FALSE)
  hd <- derive_headings(ts, rule = "distal", carry = c("grp", "val", "lab"))
  # factor stays a factor with its original labels (not coerced to 1/2 codes)
  expect_s3_class(hd$grp, "factor")
  expect_identical(levels(hd$grp), c("low", "high"))
  expect_identical(as.character(hd$grp[hd$id == "a"]), "low")
  expect_identical(as.character(hd$grp[hd$id == "b"]), "high")
  # other types preserved
  expect_type(hd$val, "double");   expect_equal(hd$val[hd$id == "b"], 20)
  expect_type(hd$lab, "character"); expect_identical(hd$lab[hd$id == "a"], "x")
})

test_that("derive_headings(carry=) keeps an ordered factor (cpunctatus arc)", {
  data(cpunctatus, package = "radiatR", envir = environment())
  hd <- derive_headings(cpunctatus, rule = "distal", carry = "arc")
  expect_s3_class(hd$arc, "factor")
  expect_true(is.ordered(hd$arc))
  expect_setequal(levels(hd$arc), c("0","5","10","15","20","30","40","50"))
})

test_that("distal rule with max_radius caps the furthest-point search", {
  data(cpunctatus, package = "radiatR")
  hd_capped <- derive_headings(cpunctatus, rule = "distal",
                               return_coords = TRUE, max_radius = 1)
  rho <- sqrt(hd_capped$x_distal^2 + hd_capped$y_distal^2)
  expect_true(all(rho <= 1 + 1e-8, na.rm = TRUE))
})

test_that("distal rule default (max_radius = Inf) is unchanged", {
  data(cpunctatus, package = "radiatR")
  hd_default <- derive_headings(cpunctatus, rule = "distal")
  hd_inf     <- derive_headings(cpunctatus, rule = "distal", max_radius = Inf)
  expect_equal(hd_default$heading, hd_inf$heading)
})

test_that("derive_headings(carry=) refuses a column that would overwrite a result column", {
  d <- data.frame(
    id      = rep(c("a", "b"), each = 3L),
    t       = rep(1:3, 2L),
    x       = c(0, 0.5, 0.9,  0, -0.5, -0.9),
    y       = c(0, 0.1, 0.0,  0,  0.1,  0.0),
    heading = 1.234)
  ts <- tracks(d, id = "id", time = "t", x = "x", y = "y",
               angle_unit = "radians", normalize_xy = FALSE)
  # Before this check the derived headings were replaced by the 1.234 column.
  err <- expect_error(derive_headings(ts, rule = "distal", carry = "heading"),
                      class = "radiatR_invalid_argument")
  expect_match(conditionMessage(err), "'heading'", fixed = TRUE)
})

# ---- weighting of the step- and position-mean rules --------------------------
xy_track <- function(x, y) {
  tracks(data.frame(id = "a", time = seq_along(x), x = x, y = y),
         id = "id", time = "time", x = "x", y = "y")
}
heading_of <- function(ts, ...) derive_headings(ts, ..., on_missing = "quiet")$heading

# one long step east, then three short steps north
unequal_steps <- function() xy_track(c(0, .5, .5, .5, .5), c(0, 0, .01, .02, .03))

test_that("velocity_mean and vm_fit weight step directions by step length when asked", {
  for (rule in c("velocity_mean", "vm_fit"))
    expect_equal(heading_of(unequal_steps(), rule = rule, weight_by = "step_length"),
                 atan2(.03, .5), info = rule)
})

test_that("velocity_mean and vm_fit weight step directions equally under weight_by = 'uniform'", {
  for (rule in c("velocity_mean", "vm_fit"))
    expect_equal(heading_of(unequal_steps(), rule = rule, weight_by = "uniform"),
                 atan2(3, 1), info = rule)
})

test_that("a step of zero length has no direction in the step-mean rules", {
  paused <- xy_track(c(0, 0, 0, 0, 0), c(0, .1, .1, .1, .2))
  for (rule in c("velocity_mean", "vm_fit"))
    for (wb in c("step_length", "uniform"))
      expect_equal(heading_of(paused, rule = rule, weight_by = wb), pi / 2,
                   info = paste(rule, wb))
  one_step <- xy_track(c(0, 0, 0, 0, 0), c(0, .1, .1, .1, .1))
  expect_equal(heading_of(one_step, rule = "velocity_axis", weight_by = "uniform"), pi / 2)
})

test_that("a track that never moves has no step-mean heading", {
  still <- xy_track(rep(.2, 4), rep(.3, 4))
  for (rule in c("velocity_mean", "vm_fit", "velocity_axis"))
    expect_identical(heading_of(still, rule = rule), NA_real_, info = rule)
})

test_that("origin_mean weights bearings by radius^r_power", {
  ts <- xy_track(c(.9, .1, 0), c(0, .1, .1))
  expect_equal(heading_of(ts, rule = "origin_mean"), pi / 4)
  expect_equal(heading_of(ts, rule = "origin_mean", r_power = 0), pi / 4)
  # r_power = 1 is the bearing of the centroid
  expect_equal(heading_of(ts, rule = "origin_mean", r_power = 1), atan2(.2, 1))
})

test_that("origin_mean gives no direction to a point at the origin", {
  ts <- xy_track(c(0, 0, 0), c(0, .5, .6))
  for (p in c(0, 1, -1))
    expect_equal(heading_of(ts, rule = "origin_mean", r_power = p), pi / 2, info = p)
})

test_that("origin_mean rejects an r_power that is not one finite number", {
  ts <- xy_track(c(.9, .1, 0), c(0, .1, .1))
  for (bad in list("a", NA_real_, c(1, 2), Inf)) {
    e <- tryCatch(derive_headings(ts, rule = "origin_mean", r_power = bad),
                  radiatR_invalid_argument = function(e) e)
    expect_s3_class(e, "radiatR_invalid_argument")
    expect_identical(e$arg, "r_power")
  }
})

test_that("velocity_mean and vm_fit weight steps equally by default; velocity_axis by length", {
  ts <- unequal_steps()
  for (rule in c("velocity_mean", "vm_fit"))
    expect_equal(heading_of(ts, rule = rule), atan2(3, 1), info = rule)
  expect_equal(heading_of(ts, rule = "velocity_axis"),
               heading_of(ts, rule = "velocity_axis", weight_by = "step_length"))
  expect_false(isTRUE(all.equal(
    heading_of(ts, rule = "velocity_axis"),
    heading_of(ts, rule = "velocity_axis", weight_by = "uniform"))))
})

test_that("velocity_mean weighted by step length is the net direction", {
  hd  <- derive_headings(cpunctatus, rule = "velocity_mean", weight_by = "step_length",
                         on_missing = "quiet")
  net <- derive_headings(cpunctatus, rule = "net", on_missing = "quiet")
  gap <- abs(hd$heading - net$heading[match(hd$id, net$id)])
  expect_lt(max(pmin(gap, 2 * pi - gap), na.rm = TRUE), 1e-8)
})

# A displacement of zero length has no direction, though atan2(0, 0) is 0 (East).
still_track <- function(x = .3, y = .2, n = 30) xy_track(rep(x, n), rep(y, n))
north_track <- function(n) xy_track(rep(0, n), seq(0, .5, length.out = n))

test_that("a track that never moves has no heading under the movement rules", {
  ts <- still_track()
  for (rule in c("net", "straight", "goal_bias", "maxspeed_window", "pca_axis",
                 "window_net", "ransac_straight"))
    expect_true(all(is.na(heading_of(ts, rule = rule))), info = rule)
  expect_true(is.na(heading_of(ts, rule = "pca_axis", source = "velocity")))
})

test_that("on_missing counts headings, not trials, for a multi-heading rule", {
  ts <- xy_track(c(0, .1, .1, .1, .2), rep(0, 5))   # middle window has no displacement
  expect_warning(hd <- derive_headings(ts, rule = "window_net", window_n = 3),
                 "1 of 3 headings .* from 1 of 1 trials")
  expect_identical(attr(hd, "missing_ids"), "a")
  expect_identical(attr(hd, "n_trials"), 1L)
  # one heading per trial keeps the trial wording
  expect_warning(derive_headings(still_track(), rule = "net"), "1 of 1 trials")
})

test_that("a track that never moves is reported by on_missing", {
  expect_warning(derive_headings(still_track(), rule = "net"), "1 of 1 trials")
  expect_error(derive_headings(still_track(), rule = "net", on_missing = "error"),
               "produced no heading")
})

test_that("distal gives no direction to a track that stays at the origin", {
  expect_true(is.na(heading_of(still_track(0, 0), rule = "distal")))
  expect_equal(heading_of(still_track(), rule = "distal"), atan2(.2, .3))
})

test_that("net gives no direction to a track that ends where it started", {
  loop <- xy_track(c(0, .2, .2, 0), c(0, 0, .2, 0))
  expect_true(is.na(heading_of(loop, rule = "net")))
  expect_equal(derive_headings(loop, rule = "net", return_coords = TRUE,
                               on_missing = "quiet")$x_end, 0)
})

test_that("pca_axis takes the step axis about zero", {
  even_steps <- xy_track(0:5, rep(0, 6))
  expect_equal(heading_of(even_steps, rule = "pca_axis", source = "velocity"), 0)
  # steps equal but for rounding: diff(seq(0, 1, .1)) is not constant
  expect_equal(heading_of(xy_track(rep(0, 11), seq(0, 1, .1)),
                          rule = "pca_axis", source = "velocity"), pi / 2)
  expect_equal(heading_of(xy_track(c(0, 1), c(0, 1)),     # a single step
                          rule = "pca_axis", source = "velocity"), pi / 4)
  expect_equal(heading_of(even_steps, rule = "pca_axis"), 0)
  out <- derive_headings(still_track(), rule = "pca_axis", return_coords = TRUE,
                         on_missing = "quiet")
  expect_true(is.na(out$axis_x) && is.na(out$axis_y))
})

test_that("pca_axis has no heading when the net displacement gives no sign", {
  # out along the x axis and back: a clear axis, no net displacement
  loop <- xy_track(c(0, .1, .2, .3, .2, .1, 0), c(0, .001, 0, .001, 0, .001, 0))
  for (src in c("position", "velocity"))
    expect_true(is.na(heading_of(loop, rule = "pca_axis", source = src)), info = src)
  out <- derive_headings(loop, rule = "pca_axis", return_coords = TRUE,
                         on_missing = "quiet")
  expect_true(is.na(out$axis_x))
})

test_that("window_net fits its window to a track shorter than window_n", {
  expect_equal(heading_of(north_track(6), rule = "window_net"), rep(pi / 2, 2))
  expect_equal(heading_of(north_track(3), rule = "window_net"), pi / 2)
  expect_true(is.na(heading_of(north_track(2), rule = "window_net")))
})

test_that("maxspeed_window gives the direction of a short moving track", {
  expect_equal(heading_of(north_track(6), rule = "maxspeed_window"), pi / 2)
})

# ---- straight: pauses ---------------------------------------------------------

test_that("straight: a pause does not hide a turn", {
  # east x3, pause, north x3
  ts <- xy_track(c(0, .1, .2, .3, .3, .3, .3, .3), c(0, 0, 0, 0, 0, .1, .2, .3))
  expect_true(is.na(heading_of(ts, rule = "straight", min_len = 6)))
  out <- derive_headings(ts, rule = "straight", min_len = 3, return_coords = TRUE,
                         on_missing = "quiet")
  expect_equal(out$heading, 0)                       # first of two equal runs
  expect_equal(c(out$x_seg0, out$y_seg0, out$x_seg1, out$y_seg1), c(0, 0, .3, 0))
})

test_that("straight: a pause inside a run neither breaks it nor counts toward min_len", {
  ts <- xy_track(c(0, .1, .2, .2, .3, .4), rep(0, 6))   # 4 moving steps, 1 pause
  expect_equal(heading_of(ts, rule = "straight", min_len = 4), 0)
  expect_true(is.na(heading_of(ts, rule = "straight", min_len = 5)))
})

test_that("straight: remaps run indices past leading pauses", {
  ts <- xy_track(c(0, 0, 0, .1, .2, .3, .4), rep(0, 7))  # rows 1-3 still, then east
  out <- derive_headings(ts, rule = "straight", min_len = 4, return_coords = TRUE,
                         on_missing = "quiet")
  expect_equal(out$heading, 0)
  expect_equal(out$time, 5)                              # rows 3..7, midpoint 5
  expect_equal(c(out$x_seg0, out$x_seg1), c(0, .4))
})

test_that("straight: a run that starts after a pause takes its first moving step", {
  ts <- xy_track(rep(0, 7), c(0, 0, .1, .2, .3, .4, .5))
  expect_equal(heading_of(ts, rule = "straight"), pi / 2)
})

# ---- rule options ------------------------------------------------------------

test_that("a built-in rule rejects an option it does not read", {
  expect_error(derive_headings(cpunctatus, rule = "distal", frame_select = "all"),
               class = "radiatR_invalid_argument")
  expect_error(derive_headings(cpunctatus, rule = "distal", frame_select = "all"),
               "frame_select")
  expect_error(derive_headings(cpunctatus, rule = "net", 3),
               class = "radiatR_invalid_argument")      # unnamed extra argument
  # a misspelling that is not a prefix (`$` partial matching let `min_length` through)
  expect_error(derive_headings(cpunctatus, rule = "straight", minlen = 4),
               "It takes: `tol`, `min_len`", fixed = TRUE)
  expect_error(derive_headings(cpunctatus, rule = "straight", min_length = 50),
               class = "radiatR_invalid_argument")
})

test_that("return_coords is accepted by every built-in rule", {
  args <- list(crossing = list(circ0 = .2, circ1 = .4), exit = list(r0 = .5),
               entry = list(r0 = .5), ring_tangent = list(r_star = .5))
  for (rule in names(radiatR:::.builtin_rule_options)) {
    expect_no_error(do.call(derive_headings,
      c(list(cpunctatus, rule = rule, return_coords = TRUE, on_missing = "quiet"),
        args[[rule]])))
  }
})

test_that("registered and function rules are not checked", {
  f <- function(df, cols, anything = 1, ...) data.frame(id = df[[cols$id]][1],
                                                        time = df[[cols$time]][1], heading = 0)
  expect_no_error(derive_headings(cpunctatus, rule = f, anything = 2, on_missing = "quiet"))
  register_heading_rule("tmp_rule_unknown_opts", f, overwrite = TRUE)
  on.exit(rm("tmp_rule_unknown_opts", envir = radiatR:::.heading_registry))
  expect_no_error(derive_headings(cpunctatus, rule = "tmp_rule_unknown_opts",
                                  whatever = TRUE, on_missing = "quiet"))
})

# ---- rule argument -----------------------------------------------------------
test_that("derive_headings() requires a rule and names the choices", {
  ts <- unequal_steps()
  expect_error(derive_headings(ts), "`rule` is required.*distal")
})

test_that("derive_headings() refuses a rule vector instead of a cryptic && error", {
  ts <- unequal_steps()
  expect_error(derive_headings(ts, rule = c("distal", "net")), "single rule name or a function")
  expect_error(derive_headings(ts, rule = NA_character_), "single rule name or a function")
  expect_error(derive_headings(ts, rule = 1), "single rule name or a function")
})

test_that("circ_summary_headings() requires a rule", {
  expect_error(circ_summary_headings(unequal_steps()), "`rule` is required")
})

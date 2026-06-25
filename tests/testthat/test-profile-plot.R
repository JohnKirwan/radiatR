test_that("plot_profile(speed) draws elapsed-time vs instantaneous speed, one line per track", {
  data(cpunctatus, package = "radiatR", envir = environment())
  ts <- set_frame_rate(cpunctatus, 30)
  p  <- plot_profile(ts, metric = "speed")
  expect_s3_class(p, "ggplot")
  b  <- ggplot2::ggplot_build(p)
  ld <- b$data[[which(vapply(p$layers,
    function(l) inherits(l$geom, "GeomLine"), logical(1)))[1]]]
  # built line y == instantaneous_speed, x == elapsed_seconds (on the non-NA rows)
  spd <- instantaneous_speed(ts); el <- elapsed_seconds(ts)
  ok  <- is.finite(spd)
  expect_equal(sort(round(ld$y, 6)), sort(round(spd[ok], 6)))
  expect_equal(p$labels$y, "speed (units/s)")
})

test_that("plot_profile(turning) uses angular_velocity and a turning y-label", {
  data(cpunctatus, package = "radiatR", envir = environment())
  ts <- set_frame_rate(cpunctatus, 30)
  p  <- plot_profile(ts, metric = "turning", units = "degrees")
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$y, "turning rate (deg/s)")
  b  <- ggplot2::ggplot_build(p)
  ld <- b$data[[which(vapply(p$layers,
    function(l) inherits(l$geom, "GeomLine"), logical(1)))[1]]]
  w  <- angular_velocity(ts, units = "degrees")
  expect_equal(sort(round(ld$y, 6)), sort(round(w[is.finite(w)], 6)))
})

test_that("plot_profile: distance scale -> physical y-label; needs a frame rate", {
  data(cpunctatus, package = "radiatR", envir = environment())
  bare <- methods::new("Tracks",
    data = data.frame(id = "a", frame = 0:3, x = (0:3) / 3, y = 0, angle = 0),
    cols = list(id = "id", time = "frame", angle = "angle", x = "x", y = "y"),
    angle_unit = "radians", meta = list())
  expect_error(plot_profile(bare, metric = "speed"), "frame rate")
  ts <- set_distance_scale(set_frame_rate(cpunctatus, 30), 50, "mm")
  expect_equal(plot_profile(ts, metric = "speed")$labels$y, "speed (mm/s)")
})

test_that("plot_profile: colour_by / panel_by map columns; bad column errors", {
  data(cpunctatus, package = "radiatR", envir = environment())
  ts <- set_frame_rate(cpunctatus, 30)
  grp <- intersect(c("arc","type"), names(as.data.frame(ts)))[1]
  skip_if(is.na(grp), "no grouping column")
  expect_s3_class(plot_profile(ts, metric = "speed", colour_by = grp), "ggplot")
  pf <- plot_profile(ts, metric = "speed", panel_by = grp)
  expect_s3_class(ggplot2::ggplot_build(pf), "ggplot_built")
  expect_error(plot_profile(ts, metric = "speed", colour_by = "nope"), "not found|column")
})

test_that("plot_profile(direction) uses velocity_angle and draws points, not a line", {
  data(cpunctatus, package = "radiatR", envir = environment())
  ts <- set_frame_rate(cpunctatus, 30)
  p  <- plot_profile(ts, metric = "direction")
  expect_s3_class(p, "ggplot")
  expect_true(any(vapply(p$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1))))
  expect_false(any(vapply(p$layers, function(l) inherits(l$geom, "GeomLine"), logical(1))))
  expect_equal(p$labels$y, "direction (rad)")
  b  <- ggplot2::ggplot_build(p)
  ld <- b$data[[which(vapply(p$layers,
    function(l) inherits(l$geom, "GeomPoint"), logical(1)))[1]]]
  a  <- velocity_angle(ts)
  expect_equal(sort(round(ld$y, 6)), sort(round(a[is.finite(a)], 6)))
  y <- ld$y[is.finite(ld$y)]
  expect_true(all(y >= 0 & y < 2 * pi))
})

test_that("plot_profile(direction, degrees) labels and ranges in degrees", {
  data(cpunctatus, package = "radiatR", envir = environment())
  ts <- set_frame_rate(cpunctatus, 30)
  p  <- plot_profile(ts, metric = "direction", units = "degrees")
  expect_equal(p$labels$y, "direction (deg)")
  b  <- ggplot2::ggplot_build(p)
  ld <- b$data[[which(vapply(p$layers,
    function(l) inherits(l$geom, "GeomPoint"), logical(1)))[1]]]
  y <- ld$y[is.finite(ld$y)]
  expect_true(all(y >= 0 & y < 360))
})

test_that("plot_profile: speed/turning still draw lines (back-compat)", {
  data(cpunctatus, package = "radiatR", envir = environment())
  ts <- set_frame_rate(cpunctatus, 30)
  for (m in c("speed", "turning")) {
    p <- plot_profile(ts, metric = m)
    expect_true(any(vapply(p$layers,
      function(l) inherits(l$geom, "GeomLine"), logical(1))), info = m)
  }
})

test_that("plot_profile(direction) honours colour_by and panel_by", {
  data(cpunctatus, package = "radiatR", envir = environment())
  ts  <- set_frame_rate(cpunctatus, 30)
  grp <- intersect(c("arc", "type"), names(as.data.frame(ts)))[1]
  skip_if(is.na(grp), "no grouping column")
  p <- plot_profile(ts, metric = "direction", colour_by = grp)
  expect_s3_class(p, "ggplot")
  expect_true(any(vapply(p$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1))))
  pf <- plot_profile(ts, metric = "direction", panel_by = grp)
  expect_s3_class(ggplot2::ggplot_build(pf), "ggplot_built")
})

test_that(".robust_speed_limit: quantile default, hard cap, Inf, off-scale count", {
  v <- c(rep(1, 998), 21, 50)                 # bulk at 1, two artifacts
  r <- .robust_speed_limit(v)                 # default NULL -> 99.5% quantile
  expect_equal(unname(r$limit), 1)            # 0.995 quantile of this vector is 1
  expect_equal(r$n_off, 2L)                   # the 21 and 50 are above
  expect_equal(r$max, 50)

  r3 <- .robust_speed_limit(v, max_speed = 3)
  expect_equal(r3$limit, 3); expect_equal(r3$n_off, 2L)

  ri <- .robust_speed_limit(v, max_speed = Inf)
  expect_equal(ri$limit, 50); expect_equal(ri$n_off, 0L)

  r0 <- .robust_speed_limit(rep(1, 100))      # no outliers
  expect_equal(r0$n_off, 0L)
})

test_that(".robust_speed_limit: rejects invalid max_speed", {
  v <- c(1, 2, 3)
  expect_error(.robust_speed_limit(v, max_speed = 0),  "max_speed")
  expect_error(.robust_speed_limit(v, max_speed = -1), "max_speed")
  expect_error(.robust_speed_limit(v, max_speed = c(1, 2)), "max_speed")
  expect_error(.robust_speed_limit(v, max_speed = "x"), "max_speed")
})

test_that(".speed_ylab: physical unit when calibrated, else units/s", {
  ts <- set_frame_rate(cpunctatus, 30)
  expect_equal(.speed_ylab(ts), "speed (units/s)")
  ts2 <- set_distance_scale(ts, 50, "mm")
  expect_equal(.speed_ylab(ts2), "speed (mm/s)")
})

test_that("plot_speed_direction: scatter of speed vs movement direction", {
  ts <- set_frame_rate(cpunctatus, 30)
  p  <- plot_speed_direction(ts)
  expect_s3_class(p, "ggplot")
  expect_true(any(vapply(p$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1))))
  expect_equal(p$labels$x, "direction (rad)")
  expect_equal(p$labels$y, "speed (units/s)")
  b  <- ggplot2::ggplot_build(p)
  ld <- b$data[[which(vapply(p$layers,
    function(l) inherits(l$geom, "GeomPoint"), logical(1)))[1]]]
  dir <- velocity_angle(ts); spd <- instantaneous_speed(ts)
  ok  <- is.finite(dir) & is.finite(spd)
  expect_equal(sort(round(ld$x, 6)), sort(round(dir[ok], 6)))
  expect_equal(sort(round(ld$y, 6)), sort(round(spd[ok], 6)))
})

test_that("plot_speed_direction: robust speed clip + off-scale caption by default", {
  ts <- set_frame_rate(cpunctatus, 30)
  p  <- plot_speed_direction(ts)
  rl <- .robust_speed_limit(instantaneous_speed(ts))
  expect_equal(p$coordinates$limits$y, c(0, rl$limit))   # clipped, not 0..50
  expect_true(rl$limit < 50)
  expect_true(!is.null(p$labels$caption) && grepl("off-scale", p$labels$caption))

  praw <- plot_speed_direction(ts, max_speed = Inf)
  expect_equal(praw$coordinates$limits$y, c(0, rl$max))   # no clip
  expect_null(praw$labels$caption)
})

test_that("plot_speed_direction: degrees x-label, colour_by maps, bad column errors", {
  ts  <- set_frame_rate(cpunctatus, 30)
  expect_equal(plot_speed_direction(ts, units = "degrees")$labels$x, "direction (deg)")
  grp <- intersect(c("arc", "type"), names(as.data.frame(ts)))[1]
  skip_if(is.na(grp), "no grouping column")
  pc <- plot_speed_direction(ts, colour_by = grp)
  expect_true("colour" %in% names(ggplot2::ggplot_build(pc)$data[[1]]))
  expect_error(plot_speed_direction(ts, colour_by = "nope"), "not found|column")
})

test_that("plot_profile(speed) clips robustly by default but keeps the data", {
  ts <- set_frame_rate(cpunctatus, 30)
  p  <- plot_profile(ts, metric = "speed")
  rl <- .robust_speed_limit(instantaneous_speed(ts))
  expect_equal(p$coordinates$limits$y, c(0, rl$limit))     # clipped (< 50)
  expect_true(rl$limit < 50)
  expect_true(grepl("off-scale", p$labels$caption))
  # back-compat: the layer data is NOT dropped (coord zoom only)
  b  <- ggplot2::ggplot_build(p)
  ld <- b$data[[which(vapply(p$layers,
    function(l) inherits(l$geom, "GeomLine"), logical(1)))[1]]]
  spd <- instantaneous_speed(ts)
  expect_equal(sort(round(ld$y, 6)), sort(round(spd[is.finite(spd)], 6)))
})

test_that("plot_profile(speed, max_speed=Inf) is the raw full range, no caption", {
  ts <- set_frame_rate(cpunctatus, 30)
  p  <- plot_profile(ts, metric = "speed", max_speed = Inf)
  expect_equal(p$coordinates$limits$y[2], max(instantaneous_speed(ts), na.rm = TRUE))
  expect_null(p$labels$caption)
})

test_that("plot_profile: max_speed does not affect turning/direction", {
  ts <- set_frame_rate(cpunctatus, 30)
  for (m in c("turning", "direction")) {
    p <- plot_profile(ts, metric = m, max_speed = 1)
    expect_null(p$coordinates$limits$y)     # no coord clip added
    expect_null(p$labels$caption)
  }
})

test_that("plot_speed_histogram: pooled histogram, robust x-clip, median/CV subtitle", {
  ts <- set_frame_rate(cpunctatus, 30)
  p  <- plot_speed_histogram(ts)
  expect_s3_class(p, "ggplot")
  expect_true(any(vapply(p$layers, function(l) inherits(l$geom, "GeomBar"), logical(1))))
  rl <- .robust_speed_limit(instantaneous_speed(ts))
  expect_equal(p$coordinates$limits$x, c(0, rl$limit))   # clipped (< 50)
  expect_true(rl$limit < 50)
  expect_equal(p$labels$x, "speed (units/s)")
  expect_equal(p$labels$y, "count")
  fin <- instantaneous_speed(ts); fin <- fin[is.finite(fin)]
  expect_match(p$labels$subtitle, sprintf("CV %.2f", stats::sd(fin) / mean(fin)), fixed = TRUE)
  expect_match(p$labels$subtitle, sprintf("median %.3g", stats::median(fin)), fixed = TRUE)
})

test_that("plot_speed_histogram: off-scale caption by default, none with max_speed=Inf", {
  ts <- set_frame_rate(cpunctatus, 30)
  expect_true(grepl("off-scale", plot_speed_histogram(ts)$labels$caption))
  praw <- plot_speed_histogram(ts, max_speed = Inf)
  expect_null(praw$labels$caption)
  expect_equal(praw$coordinates$limits$x[2], max(instantaneous_speed(ts), na.rm = TRUE))
})

test_that("plot_speed_histogram: physical x-label when calibrated; non-Tracks errors", {
  ts <- set_distance_scale(set_frame_rate(cpunctatus, 30), 50, "mm")
  expect_equal(plot_speed_histogram(ts)$labels$x, "speed (mm/s)")
  expect_error(plot_speed_histogram(data.frame(x = 1)), "Tracks")
})

# ---- profile smoothing -------------------------------------------------------

test_that(".smooth_profile: centered partial-window moving average, per track", {
  v  <- c(1, 2, 3, 4, 5)
  id <- rep("a", 5); t <- 1:5
  expect_equal(radiatR:::.smooth_profile(v, id, t, 1), v)          # no-op
  expect_equal(radiatR:::.smooth_profile(v, id, t, 3), c(1.5, 2, 3, 4, 4.5))
  # two tracks: smoothing must not bleed across the boundary
  v2  <- c(0, 0, 0, 10, 10, 10); id2 <- rep(c("a", "b"), each = 3); t2 <- rep(1:3, 2)
  sm  <- radiatR:::.smooth_profile(v2, id2, t2, 3)
  expect_equal(sm[id2 == "a"], c(0, 0, 0))
  expect_equal(sm[id2 == "b"], c(10, 10, 10))
})

test_that("plot_profile(smooth=) smooths the line; default is a no-op", {
  data(cpunctatus, package = "radiatR", envir = environment())
  ts <- set_frame_rate(cpunctatus, 30)
  line_y <- function(p) {
    b <- ggplot2::ggplot_build(p)
    i <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomLine"), logical(1)))[1]
    b$data[[i]]$y
  }
  expect_equal(line_y(plot_profile(ts, metric = "speed", smooth = 1)),
               line_y(plot_profile(ts, metric = "speed")))
  raw <- line_y(plot_profile(ts, metric = "speed"))
  sm  <- line_y(plot_profile(ts, metric = "speed", smooth = 9))
  expect_lt(stats::sd(sm, na.rm = TRUE), stats::sd(raw, na.rm = TRUE))   # less spread
})

test_that("plot_profile(show_raw=) adds a faint raw line only when smoothing", {
  data(cpunctatus, package = "radiatR", envir = environment())
  ts <- set_frame_rate(cpunctatus, 30)
  n_line <- function(p) sum(vapply(p$layers,
    function(l) inherits(l$geom, "GeomLine"), logical(1)))
  expect_equal(n_line(plot_profile(ts, metric = "speed", smooth = 5, show_raw = TRUE)),
               n_line(plot_profile(ts, metric = "speed", smooth = 5)) + 1L)
  expect_equal(n_line(plot_profile(ts, metric = "speed", smooth = 1, show_raw = TRUE)),
               n_line(plot_profile(ts, metric = "speed", smooth = 1)))   # no-op w/o smoothing
})

test_that("plot_profile(direction) ignores smoothing; smooth validates", {
  data(cpunctatus, package = "radiatR", envir = environment())
  ts <- set_frame_rate(cpunctatus, 30)
  pt_y <- function(p) {
    b <- ggplot2::ggplot_build(p)
    i <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1)))[1]
    b$data[[i]]$y
  }
  expect_equal(pt_y(plot_profile(ts, metric = "direction", smooth = 5)),
               pt_y(plot_profile(ts, metric = "direction")))
  expect_error(plot_profile(ts, metric = "speed", smooth = 0), "smooth")
  expect_error(plot_profile(ts, metric = "speed", smooth = 2.5), "smooth")
})

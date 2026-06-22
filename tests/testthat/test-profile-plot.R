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
  expect_error(plot_profile(cpunctatus, metric = "speed"), "frame rate")
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

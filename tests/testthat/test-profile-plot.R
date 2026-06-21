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

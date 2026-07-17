test_that(".calibrate_to_unit_circle translates and scales uniformly", {
  x <- c(512, 812, 212)
  y <- c(384, 384, 384)
  out <- radiatR:::.calibrate_to_unit_circle(x, y, origin = c(512, 384), radius = 300)
  expect_equal(out$x, c(0, 1, -1))
  expect_equal(out$y, c(0, 0, 0))
})

test_that(".calibrate_to_unit_circle preserves bearing of a point from the origin", {
  # A point due-north of origin stays due-north (positive y, zero x) after mapping.
  out <- radiatR:::.calibrate_to_unit_circle(512, 684, origin = c(512, 384), radius = 300)
  expect_equal(out$x, 0)
  expect_equal(out$y, 1)
})

test_that(".calibrate_to_unit_circle leaves non-finite coordinates non-finite", {
  out <- radiatR:::.calibrate_to_unit_circle(c(512, NA), c(384, 384),
                                             origin = c(512, 384), radius = 300)
  expect_equal(out$x[1], 0)
  expect_true(is.na(out$x[2]))
})

# Helper: a single eastward trajectory in raw pixel space, plus a second
# trajectory offset to the north-east, sharing one origin/radius.
.calib_df <- function() {
  data.frame(
    id   = rep(c("a", "b"), each = 3),
    time = rep(1:3, 2),
    # a: due east of origin (512,384) at increasing radius
    # b: due north of origin at increasing radius (offset from a)
    x = c(562, 662, 812,  512, 512, 512),
    y = c(384, 384, 384,  434, 534, 684)
  )
}

test_that("calibration preserves an eastward bearing (the #12 failure in reverse)", {
  ts <- tracks(.calib_df(), id = "id", time = "time", x = "x", y = "y",
               origin = c(512, 384), radius = 300)
  d <- as.data.frame(ts)
  a <- d[d$id == "a", ]
  # East = angle 0 in the package's [0, 2pi) convention. Distal point is the
  # farthest-from-centre row; its bearing must read east, not west.
  ang <- a$angle[which.max(sqrt(a$x^2 + a$y^2))]
  expect_equal(as.numeric(ang) %% (2 * pi), 0, tolerance = 1e-8)
})

test_that("calibration preserves relative geometry between trajectories", {
  ts <- tracks(.calib_df(), id = "id", time = "time", x = "x", y = "y",
               origin = c(512, 384), radius = 300)
  d <- as.data.frame(ts)
  a <- d[d$id == "a", ]; b <- d[d$id == "b", ]
  ea <- a$angle[which.max(sqrt(a$x^2 + a$y^2))]   # east  -> 0
  nb <- b$angle[which.max(sqrt(b$x^2 + b$y^2))]   # north -> pi/2
  # a is east, b is north: 90-degree separation survives (normalize_xy would
  # collapse both onto their own centres and destroy this).
  expect_equal(as.numeric(nb - ea) %% (2 * pi), pi / 2, tolerance = 1e-8)
})

test_that("calibration retains raw coordinates and is invertible", {
  ts <- tracks(.calib_df(), id = "id", time = "time", x = "x", y = "y",
               origin = c(512, 384), radius = 300)
  d <- as.data.frame(ts)
  expect_true(all(c("x_raw", "y_raw") %in% names(d)))
  expect_equal((d$x_raw - 512) / 300, d$x, tolerance = 1e-12)
  expect_equal((d$y_raw - 384) / 300, d$y, tolerance = 1e-12)
})

test_that("calibration records provenance in metadata and transform_history", {
  ts <- tracks(.calib_df(), id = "id", time = "time", x = "x", y = "y",
               origin = c(512, 384), radius = 300)
  expect_equal(ts@meta$calibration, list(origin = c(512, 384), radius = 300))
  expect_false(isTRUE(ts@meta$normalize_xy))
  th <- transform_history(ts)
  expect_true("calibrate_unit_circle" %in% th$step)
  expect_setequal(th$id[th$step == "calibrate_unit_circle"], c("a", "b"))
  cal_rows <- th[th$step == "calibrate_unit_circle", ]
  expect_equal(cal_rows$params[[1]], list(origin = c(512, 384), radius = 300))
})

test_that("calibration works for a non-2-id trajectory count (3 ids)", {
  df3 <- data.frame(
    id   = rep(c("a", "b", "c"), each = 3),
    time = rep(1:3, 3),
    x = c(562, 662, 812,  512, 512, 512,  462, 362, 212),
    y = c(384, 384, 384,  434, 534, 684,  434, 534, 684)
  )
  ts3 <- tracks(df3, id = "id", time = "time", x = "x", y = "y",
                origin = c(512, 384), radius = 300)
  th3 <- transform_history(ts3)
  cal3 <- th3[th3$step == "calibrate_unit_circle", ]
  expect_setequal(cal3$id, c("a", "b", "c"))
  expect_true(all(vapply(cal3$params, identical, logical(1),
                         list(origin = c(512, 384), radius = 300))))
})

test_that("calibration rejects invalid inputs", {
  df <- .calib_df()
  expect_error(
    tracks(df, id = "id", time = "time", x = "x", y = "y",
           origin = c(512, 384), radius = 0),
    "positive finite")
  expect_error(
    tracks(df, id = "id", time = "time", x = "x", y = "y",
           origin = c(512, 384), radius = -1),
    "positive finite")
  expect_error(
    tracks(df, id = "id", time = "time", x = "x", y = "y", origin = c(512, 384)),
    "required together")
  expect_error(
    tracks(df, id = "id", time = "time", x = "x", y = "y",
           origin = c(512, 384), radius = 300, normalize_xy = TRUE),
    "mutually exclusive")
})

test_that("calibration requires x/y (not angle-only) input", {
  ang_df <- data.frame(id = "a", time = 1:3, theta = c(0, 1, 2))
  expect_error(
    tracks(ang_df, id = "id", time = "time", angle = "theta", angle_unit = "radians",
           origin = c(512, 384), radius = 300),
    "requires x and y")
})

test_that("add_circ_mean(axial=TRUE) draws a segment through the origin, both arrowheads", {
  sm <- data.frame(mean_dir = pi / 6, resultant_R = 0.8)
  l  <- add_circ_mean(sm, axial = TRUE)
  d  <- l$data
  # axis segment: endpoints straddle the origin
  expect_equal(d$.x, -d$.xend)
  expect_equal(d$.y, -d$.yend)
  # both ends carry an arrowhead (grid: both = 3)
  expect_equal(as.integer(l$geom_params$arrow$ends), 3L)
})

test_that("add_circ_mean(axial=FALSE) is unchanged (origin -> tip, single head)", {
  sm <- data.frame(mean_dir = pi / 6, resultant_R = 0.8)
  l  <- add_circ_mean(sm, axial = FALSE)
  expect_equal(add_circ_mean(sm)$data, l$data)     # default == FALSE
  expect_equal(l$data$.x, 0)                        # starts at origin
  expect_equal(as.integer(l$geom_params$arrow$ends), 2L)   # last only
})

test_that("add_heading_arrow(axial=TRUE) routes the axial mean through (axis segment)", {
  set.seed(1)
  a  <- (c(rnorm(40, 30, 5), rnorm(40, 210, 5)) * pi/180) %% (2*pi)
  hf <- headings_frame(data.frame(heading = a), heading, units = "radians")
  l  <- add_heading_arrow(hf, axial = TRUE)
  # tail = -tip (segment through the origin), robust to display rotation
  expect_equal(l$data$.x, -l$data$.xend)
  expect_equal(l$data$.y, -l$data$.yend)
  expect_equal(as.integer(l$geom_params$arrow$ends), 3L)
})

test_that("add_circ_interval(axial=TRUE) draws the arc at both ends", {
  iv <- data.frame(mean_dir = 0.5, lower = 0.4, upper = 0.6, wraps = FALSE)
  l1 <- add_circ_interval(iv, axial = FALSE)
  l2 <- add_circ_interval(iv, axial = TRUE)
  # twice the arc points (two groups), and two distinct .group_id values
  expect_equal(nrow(l2$data), 2L * nrow(l1$data))
  expect_length(unique(l2$data$.group_id), 2L)
})

test_that("add_heading_interval(axial=TRUE) routes the axial interval through", {
  set.seed(2)
  a  <- (c(rnorm(60, 35, 6), rnorm(60, 215, 6)) * pi/180) %% (2*pi)
  hf <- headings_frame(data.frame(heading = a), heading, units = "radians")
  l  <- add_heading_interval(hf, axial = TRUE, stat = "sd")
  expect_length(unique(l$data$.group_id), 2L)   # both ends
})

test_that("add_critical_v_line(axial=TRUE) mirrors the chord to both poles", {
  hd <- data.frame(heading = runif(40, 0, 2*pi))
  l0 <- add_critical_v_line(hd, mu0 = pi/2, axial = FALSE)   # list of layers
  l1 <- add_critical_v_line(hd, mu0 = pi/2, axial = TRUE)
  # the chord geom_segment carries twice as many rows (a chord at each pole)
  seg0 <- l0[[length(l0)]]$data
  seg1 <- l1[[length(l1)]]$data
  expect_equal(nrow(seg1), 2L * nrow(seg0))
})

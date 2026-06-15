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

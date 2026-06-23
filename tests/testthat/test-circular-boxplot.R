test_that("circ_boxplot_stats recovers the median and brackets ~50% in the box", {
  set.seed(1)
  ang <- as.numeric(circular::rvonmises(200, mu = circular::circular(pi/2), kappa = 4))
  s <- circ_boxplot_stats(data.frame(heading = ang))
  expect_true(s$drawable)
  dmed <- abs(((s$median - pi/2 + pi) %% (2*pi)) - pi)
  expect_lt(dmed, 0.25)
  rel <- ((ang - s$median + pi) %% (2*pi)) - pi
  h   <- ((s$hinges - s$median + pi) %% (2*pi)) - pi
  inside <- mean(rel >= min(h) & rel <= max(h))
  expect_gt(inside, 0.40); expect_lt(inside, 0.60)
})

test_that("the fence multiplier is ~1.5 when concentrated and ~0.5 near uniform", {
  set.seed(2)
  conc <- as.numeric(circular::rvonmises(300, mu = circular::circular(0), kappa = 12))
  near_u <- as.numeric(circular::rvonmises(300, mu = circular::circular(0), kappa = 0.1))
  cc <- circ_boxplot_stats(data.frame(heading = conc))$constant
  cu <- circ_boxplot_stats(data.frame(heading = near_u))$constant
  expect_gt(cc, 1.3); expect_lt(cc, 1.9)
  expect_gt(cu, 0.45); expect_lt(cu, 0.70)
})

test_that("far-out values fall outside the whiskers", {
  set.seed(3)
  ang <- c(as.numeric(circular::rvonmises(120, mu = circular::circular(0), kappa = 20)), pi)
  s <- circ_boxplot_stats(data.frame(heading = ang))
  expect_true(s$drawable)
  expect_true(length(s$far_out) >= 1)
  rel_fo <- ((s$far_out - s$median + pi) %% (2*pi)) - pi
  f <- ((s$fences - s$median + pi) %% (2*pi)) - pi
  expect_true(all(rel_fo > max(f) | rel_fo < min(f)))
})

test_that("not-drawable cases are flagged (uniform median, too few points)", {
  set.seed(4)
  uni <- runif(200, 0, 2*pi)
  su  <- circ_boxplot_stats(data.frame(heading = uni))
  expect_true(!is.na(su$reason))
  few <- circ_boxplot_stats(data.frame(heading = c(0.1, 0.2, 0.3)))
  expect_false(few$drawable)
  expect_match(few$reason, "4")
})

test_that("highly concentrated / near-identical data does not crash and gives ~1.5 constant", {
  s8 <- circ_boxplot_stats(data.frame(heading = rep(1, 8)))
  expect_true(s8$drawable)
  expect_true(is.finite(s8$constant))
  expect_gt(s8$constant, 1.3); expect_lt(s8$constant, 1.8)
  expect_true(all(is.finite(s8$fences)))
  expect_true(is.na(s8$reason))                # not flagged near-uniform
  tight <- 1 + c(-1e-3, 1e-3, rep(0, 6))
  expect_silent(circ_boxplot_stats(data.frame(heading = tight)))
})

test_that("circ_boxplot_stats matches bpDir::CircularBoxplot as an oracle", {
  skip_if_not_installed("bpDir")
  set.seed(5)
  ang <- as.numeric(circular::rvonmises(150, mu = circular::circular(1), kappa = 3))
  s <- circ_boxplot_stats(data.frame(heading = ang))
  grDevices::pdf(tempfile(fileext = ".pdf")); on.exit(grDevices::dev.off())
  ref <- bpDir::CircularBoxplot(circular::circular(ang, units = "radians", modulo = "2pi"),
                                units = "radians", constant = "optimal")
  expect_equal(s$constant, as.numeric(ref$constant), tolerance = 1e-3)
})

test_that("axial boxplot recovers the median axis in [0, pi) from antipodal data", {
  set.seed(6)
  axis <- pi/3
  half <- as.numeric(circular::rvonmises(150, mu = circular::circular(2*axis), kappa = 8)) / 2
  ang  <- c(half, (half + pi) %% (2*pi))
  s <- circ_boxplot_stats(data.frame(heading = ang), axial = TRUE)
  expect_true(s$axial); expect_true(s$drawable)
  expect_true(s$median >= 0 && s$median < pi)
  daxis <- abs(((s$median - axis + pi/2) %% pi) - pi/2)
  expect_lt(daxis, 0.2)
  expect_true(all(s$far_out >= 0 & s$far_out < pi))
})

# Oracle for the axial fence multiplier. Per Buttarazzi et al. (2018) section
# 4.4 the axial multiplier is the concentration measured on the DOUBLED
# (transformed) data -- "spread is NOT back transformed" -- where the axial
# distribution is unimodal von Mises. So radiatR's axial constant must equal
# what bpDir's *directional* CircularBoxplot computes on the doubled angles.
#
# We deliberately do NOT compare against bpDir::AxialBoxplot: its code computes
# the concentration as A1inv(rho.circular(A)) on the UNDOUBLED data (contrary to
# its own paper's 4.4), which is internally inconsistent (it takes the median on
# the doubled data) and representation-sensitive (degenerates to ~0.49 on
# full-circle antipodal input). radiatR follows the paper; this oracle pins it.
test_that("axial constant matches bpDir's directional boxplot on the doubled data (paper 4.4)", {
  skip_if_not_installed("bpDir")
  set.seed(7)
  half <- as.numeric(circular::rvonmises(120, mu = circular::circular(2), kappa = 5)) / 2
  s <- circ_boxplot_stats(data.frame(heading = half), axial = TRUE)
  doubled <- (2 * half) %% (2 * pi)
  grDevices::pdf(tempfile(fileext = ".pdf")); on.exit(grDevices::dev.off())
  ref <- bpDir::CircularBoxplot(circular::circular(doubled, units = "radians", modulo = "2pi"),
                                units = "radians", constant = "optimal")
  expect_equal(s$constant, as.numeric(ref$constant), tolerance = 1e-3)
  # and it is the meaningful Tukey-like value, not the degenerate near-uniform one
  expect_gt(s$constant, 1.3)
})

test_that("add_circular_boxplot returns ggplot layers that compose with radiate", {
  set.seed(8)
  hd <- data.frame(heading = as.numeric(circular::rvonmises(150, mu = circular::circular(1), kappa = 5)))
  lyr <- add_circular_boxplot(hd)
  expect_true(is.list(lyr))
  expect_true(all(vapply(lyr, function(x) inherits(x, "Layer"), logical(1))))
  p <- ggplot2::ggplot() + ggplot2::coord_fixed() + lyr
  expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
})

test_that("axial add_circular_boxplot mirrors elements at theta and theta+pi", {
  set.seed(9)
  half <- as.numeric(circular::rvonmises(150, mu = circular::circular(2), kappa = 8)) / 2
  hd <- data.frame(heading = c(half, (half + pi) %% (2*pi)))
  # the antipodal data has a non-unique median directionally (expected warning)
  ld <- suppressWarnings(add_circular_boxplot(hd, axial = FALSE))
  la <- add_circular_boxplot(hd, axial = TRUE)
  poly_rows <- function(lyrs) sum(vapply(lyrs, function(L)
    if (inherits(L, "Layer") && inherits(L$geom, "GeomPolygon")) nrow(L$data) else 0L, integer(1)))
  expect_gt(poly_rows(la), poly_rows(ld))
})

test_that("not-drawable input warns and adds no boxplot geoms", {
  hd <- data.frame(heading = c(0.1, 0.2, 0.3))   # n < 4
  expect_warning(out <- add_circular_boxplot(hd), "not drawn|4")
  expect_null(out)
  p <- ggplot2::ggplot() + out
  expect_s3_class(p, "ggplot")
})

test_that("axial boxplot draws correctly when the axis sits on the 0/pi seam", {
  set.seed(42)
  half <- (as.numeric(circular::rvonmises(200, mu = circular::circular(2*0.05), kappa = 10))/2) %% pi
  la <- add_circular_boxplot(data.frame(heading = half), axial = TRUE)
  poly <- la[[which(vapply(la, function(L) inherits(L$geom, "GeomPolygon"), logical(1)))[1]]]$data
  ang <- atan2(poly$.y - 0, poly$.x - 0)
  expect_true(is.data.frame(poly) && nrow(poly) > 0)
  p <- ggplot2::ggplot() + ggplot2::coord_fixed() + la
  expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
})

test_that("add_circular_boxplot: arrow off by default, thin band outside the circle", {
  set.seed(8)
  hd <- data.frame(heading = as.numeric(
    circular::rvonmises(150, mu = circular::circular(1), kappa = 5)))
  lyr <- add_circular_boxplot(hd)
  has_arrow <- function(L) inherits(L, "Layer") && !is.null(L$geom_params$arrow)
  expect_false(any(vapply(lyr, has_arrow, logical(1))))          # no arrow by default
  poly <- lyr[[which(vapply(lyr, function(L) inherits(L$geom, "GeomPolygon"),
                            logical(1)))[1]]]$data
  r <- sqrt(poly$.x^2 + poly$.y^2)
  expect_true(min(r) > 1.0)                                      # entirely outside the circle
  expect_true(max(r) > 1.05 && max(r) < 1.2)                     # ~1.1 band
  expect_true((max(r) - min(r)) < 0.1)                           # thin (width 0.06)
  lyr2 <- add_circular_boxplot(hd, show_median_arrow = TRUE)
  expect_true(any(vapply(lyr2, has_arrow, logical(1))))          # opt back in
})

test_that("add_circular_boxplot: theme sets the chrome colour", {
  set.seed(8)
  hd <- data.frame(heading = as.numeric(
    circular::rvonmises(150, mu = circular::circular(1), kappa = 5)))
  poly_col <- function(lyr) {
    L <- lyr[[which(vapply(lyr, function(z) inherits(z$geom, "GeomPolygon"),
                           logical(1)))[1]]]
    L$aes_params$colour
  }
  expect_equal(poly_col(add_circular_boxplot(hd, theme = "void")), "black")
  dk <- radiatR:::.radial_style("dark")
  expect_equal(poly_col(add_circular_boxplot(hd, theme = "dark")),
               dk$col_of(dk$ax$line))
  expect_equal(poly_col(add_circular_boxplot(hd)), "black")       # NULL theme -> colour default
})

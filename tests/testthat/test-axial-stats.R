test_that(".fold_angles doubles only when axial; .unfold_mean halves to [0, pi)", {
  th <- c(0, pi/4, pi/2, 3*pi/2, 2*pi - 0.1)
  expect_equal(radiatR:::.fold_angles(th, FALSE), th)
  expect_equal(radiatR:::.fold_angles(th, TRUE), (2 * th) %% (2*pi))

  # directional: unfold is just wrap-to-2pi
  expect_equal(radiatR:::.unfold_mean(3.0, FALSE), 3.0 %% (2*pi))
  # axial: halve into [0, pi)
  expect_equal(radiatR:::.unfold_mean(pi,   TRUE), (pi/2) %% pi)
  expect_equal(radiatR:::.unfold_mean(3*pi, TRUE), ((3*pi)/2) %% pi)
})

test_that("circ_summarise(axial=TRUE) equals directional summary of doubled angles", {
  set.seed(1)
  a  <- runif(120, 0, 2*pi)
  ax <- circ_summarise(data.frame(heading = a), heading, units = "radians", axial = TRUE)
  d2 <- circ_summarise(data.frame(heading = (2*a) %% (2*pi)), heading, units = "radians")
  expect_equal(ax$resultant_R, d2$resultant_R)
  expect_equal(ax$kappa,       d2$kappa)
  expect_equal(ax$mean_dir,    (d2$mean_dir / 2) %% pi)
})

test_that("circ_summarise(axial=TRUE) recovers an axis where directional R cancels", {
  set.seed(2)
  a  <- (c(rnorm(60, 30, 4), rnorm(60, 210, 4)) * pi/180) %% (2*pi)
  df <- data.frame(heading = a)
  dir <- circ_summarise(df, heading, units = "radians")
  ax  <- circ_summarise(df, heading, units = "radians", axial = TRUE)
  expect_lt(dir$resultant_R, 0.2)             # opposite lobes cancel
  expect_gt(ax$resultant_R,  0.9)             # axis is concentrated
  expect_equal(as.numeric(ax$mean_dir), 30 * pi/180, tolerance = 0.05)
})

test_that("circ_summarise axial mean_dir_deg comes from the halved mean, per display", {
  set.seed(3)
  a  <- (c(rnorm(60, 40, 3), rnorm(60, 220, 3)) * pi/180) %% (2*pi)
  ax <- circ_summarise(data.frame(heading = a), heading, units = "radians",
                       axial = TRUE)
  expect_equal(ax$mean_dir_deg,
               radiatR:::.uc_angle_to_display(ax$mean_dir, circ_display()))
})

test_that("circ_summarise(axial=FALSE) is unchanged", {
  set.seed(4)
  df <- data.frame(heading = runif(50, 0, 2*pi), g = rep(c("x","y"), 25))
  expect_equal(circ_summarise(df, heading, units = "radians", .by = "g"),
               circ_summarise(df, heading, units = "radians", .by = "g",
                              axial = FALSE))
})

test_that("circ_summary(TrajSet, axial=TRUE) equals doubled-angle summary, mean halved", {
  ts  <- simulate_tracks(n_points = 30, seed = 8, output = "trajset")
  dir <- circ_summary(ts, by = "global")
  ax  <- circ_summary(ts, by = "global", axial = TRUE)
  d   <- as.data.frame(ts)
  th  <- d[[ts@cols$angle]]; th <- th[is.finite(th)]
  R2  <- sqrt(mean(cos(2*th))^2 + mean(sin(2*th))^2)
  expect_equal(ax$resultant_R, R2)
  expect_equal(ax$mean_dir, (Arg(mean(exp(1i*2*th))) %% (2*pi) / 2) %% pi)
  # directional path unchanged
  expect_equal(circ_summary(ts, by = "global", axial = FALSE), dir)
})

test_that("circ_dispersion(axial=TRUE) gives the axial mean, R and circ_sd", {
  set.seed(5)
  a  <- (c(rnorm(50, 50, 6), rnorm(50, 230, 6)) * pi/180) %% (2*pi)
  hd <- data.frame(heading = a)
  ax <- circ_dispersion(hd, angle_col = "heading", axial = TRUE)
  expect_gt(ax$resultant_R, 0.85)
  expect_equal(ax$mean_dir, (Arg(mean(exp(1i*2*a)))/2) %% pi, tolerance = 1e-8)
  expect_equal(ax$circ_sd, sqrt(-2*log(ax$resultant_R)), tolerance = 1e-8)
  # directional default unchanged
  expect_equal(circ_dispersion(hd, axial = FALSE), circ_dispersion(hd))
})

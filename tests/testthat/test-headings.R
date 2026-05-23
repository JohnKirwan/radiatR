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

test_that("derive_headings crossing rule with return_coords adds x_inner/y_inner", {
  theta <- pi / 4
  n <- 20
  r <- seq(0, 0.8, length.out = n)
  df <- data.frame(id = "A", time = seq_len(n),
                   x = r * cos(theta), y = r * sin(theta))
  ts <- TrajSet(df, id = "id", time = "time", x = "x", y = "y",
                normalize_xy = FALSE)
  hd <- derive_headings(ts, rule = "crossing", circ0 = 0.2, circ1 = 0.4,
                        return_coords = TRUE)
  expect_named(hd, c("id", "time", "heading", "x_inner", "y_inner"))
  r_inner <- sqrt(hd$x_inner^2 + hd$y_inner^2)
  expect_equal(r_inner, 0.2, tolerance = 1e-6)
  expect_equal(atan2(hd$y_inner, hd$x_inner), theta, tolerance = 1e-6)
})

test_that("derive_headings crossing without crossing returns NA row", {
  # Trajectory that never leaves circ0=0.2; normalize_xy=FALSE keeps raw coords.
  df <- data.frame(id = "A", time = 1:5,
                   x = c(0, 0.05, 0.1, 0.05, 0), y = rep(0, 5))
  ts <- TrajSet(df, id = "id", time = "time", x = "x", y = "y",
                normalize_xy = FALSE)
  hd <- derive_headings(ts, rule = "crossing", circ0 = 0.2, circ1 = 0.4)
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
  ts <- TrajSet(df, id = "id", time = "time", x = "x", y = "y", angle = "time")
  headings <- derive_headings(ts, rule = "net")
  expect_equal(nrow(headings), 1)
  expect_equal(headings$heading, atan2(1, 1), tolerance = 1e-8)
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
  ts <- TrajSet(df, id = "id", time = "time", x = "x", y = "y", angle = "time")
  res <- derive_headings(ts, rule = "zero_heading")
  expect_equal(res$heading, 0)
  expect_true("zero_heading" %in% list_heading_rules())
})

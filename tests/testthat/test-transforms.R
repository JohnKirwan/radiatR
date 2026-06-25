# A trivial per-trajectory transform: add a constant to the angle column.
.add_const <- function(df, cols, k) {
  df[[cols$angle]] <- (df[[cols$angle]] + k) %% (2 * pi)
  df
}

test_that("apply_transform by='trajectory' modifies the angle and keeps structure", {
  ts  <- cpunctatus
  out <- apply_transform(ts, .add_const, k = 0.3, by = "trajectory",
                         step = "add_const")
  expect_s4_class(out, "Tracks")
  expect_equal(nrow(out@data), nrow(ts@data))
  expect_identical(out@data[[ts@cols$id]], ts@data[[ts@cols$id]])      # ids unchanged
  expect_identical(out@data[[ts@cols$time]], ts@data[[ts@cols$time]])  # row order/time unchanged
  expect_equal(out@data[[ts@cols$angle]],
               (ts@data[[ts@cols$angle]] + 0.3) %% (2 * pi))
})

test_that("apply_transform by='all' runs once on the whole frame", {
  ts  <- cpunctatus
  out <- apply_transform(ts, .add_const, k = 0.1, by = "all", step = "add_const")
  expect_equal(out@data[[ts@cols$angle]],
               (ts@data[[ts@cols$angle]] + 0.1) %% (2 * pi))
})

test_that("apply_transform appends one transform_history step across all trajectories", {
  ts  <- cpunctatus
  h0  <- transform_history(ts)
  out <- apply_transform(ts, .add_const, k = 0.2, step = "my_step")
  h   <- transform_history(out)
  new <- h[h$step == "my_step", ]
  expect_equal(nrow(new), length(ids(ts)))            # one row per trajectory
  expect_equal(length(unique(new$order)), 1L)         # a single new step
  expect_true(all(new$order > max(c(h0$order, 0L))))  # appended after existing steps
})

test_that("apply_transform handles a transform with no extra arguments", {
  ts  <- cpunctatus
  flip <- function(df, cols) { df[[cols$angle]] <- (df[[cols$angle]] + pi) %% (2 * pi); df }
  expect_s4_class(apply_transform(ts, flip, by = "all"), "Tracks")
  expect_s4_class(apply_transform(ts, flip, by = "trajectory"), "Tracks")
})

test_that("apply_transform errors when fn changes the row count", {
  ts <- cpunctatus
  bad <- function(df, cols) df[-1, , drop = FALSE]   # drops a row
  expect_error(apply_transform(ts, bad), "same number of rows")
})

test_that("apply_transform errors when fn returns a non-data-frame", {
  ts <- cpunctatus
  expect_error(apply_transform(ts, function(df, cols) 42), "data frame")
})

# Recipe A: edge-referenced -> centre-referenced reference offset (per-trial,
# read from a stimulus-width metadata column).
edge_to_centre <- function(df, cols, width_col, units = "degrees") {
  half <- (df[[width_col]] / 2) * (if (units == "degrees") pi / 180 else 1)
  ang  <- (df[[cols$angle]] + half) %% (2 * pi)
  df[[cols$angle]] <- ang
  rho <- df[[cols$rho]]
  df[[cols$rel_x]] <- rho * cos(ang)
  df[[cols$rel_y]] <- rho * sin(ang)
  df
}

# Recipe B: polarization direction -> axis (angle doubling).
direction_to_axis <- function(df, cols) {
  df[[cols$angle]] <- (2 * df[[cols$angle]]) %% (2 * pi)
  df
}

test_that("edge_to_centre offsets the heading by half the stimulus width", {
  ts <- cpunctatus
  ts@data$width <- 30                       # degrees, constant for the test
  out  <- apply_transform(ts, edge_to_centre, width_col = "width",
                          units = "degrees", step = "edge_to_centre")
  half <- 15 * pi / 180
  exp_ang <- (cpunctatus@data[[cpunctatus@cols$angle]] + half) %% (2 * pi)
  expect_equal(out@data[[ts@cols$angle]], exp_ang)
  expect_equal(out@data[[ts@cols$rel_x]], cpunctatus@data[[cpunctatus@cols$rho]] * cos(exp_ang))
  expect_equal(out@data[[ts@cols$rel_y]], cpunctatus@data[[cpunctatus@cols$rho]] * sin(exp_ang))
})

test_that("direction_to_axis doubles the angle modulo 2*pi", {
  ts  <- cpunctatus
  out <- apply_transform(ts, direction_to_axis, by = "all",
                         step = "direction_to_axis")
  expect_equal(out@data[[ts@cols$angle]],
               (2 * cpunctatus@data[[cpunctatus@cols$angle]]) %% (2 * pi))
})

# Build a one-track Tracks with explicit relative coords (rho = |rel_x| here).
# normalize_xy = FALSE keeps the supplied rel_x/rel_y as-is.
mk_track <- function(rx) {
  n  <- length(rx)
  df <- data.frame(id = "A", t = seq_len(n), x = rx, y = 0, rx = rx, ry = 0)
  tracks(df, id = "id", time = "t", x = "x", y = "y",
         rel_x = "rx", rel_y = "ry", normalize_xy = FALSE)
}

test_that("restrict_to_circumference truncate keeps the pre-excursion prefix", {
  ts <- mk_track(c(0, 0.3, 1.4, 0.3, 0))            # first rho>1 at index 3
  out <- suppressMessages(restrict_to_circumference(ts, "truncate"))
  d   <- as.data.frame(out)
  expect_equal(nrow(d), 2L)
  expect_true(all(sqrt(d$rx^2 + d$ry^2) <= 1 + 1e-9))
})

test_that("restrict_to_circumference drop removes only beyond-circumference rows", {
  ts <- mk_track(c(0, 0.3, 1.4, 0.3, 0))
  out <- suppressMessages(restrict_to_circumference(ts, "drop"))
  d   <- as.data.frame(out)
  expect_equal(nrow(d), 4L)                          # only the rho=1.4 row removed
  expect_true(all(sqrt(d$rx^2 + d$ry^2) <= 1 + 1e-9))
})

test_that("restrict_to_circumference keeps points exactly on the circumference", {
  ts <- mk_track(c(0, 0.5, 1, 0.5, 0))               # rho = 1 is on the circle
  out <- suppressMessages(restrict_to_circumference(ts, "truncate"))
  expect_equal(nrow(as.data.frame(out)), 5L)         # nothing is "beyond"
})

test_that("restrict_to_circumference errors without relative coordinates", {
  df <- data.frame(id = "A", t = 1:3, x = c(0, 0.5, 0.9), y = 0)
  ts <- tracks(df, id = "id", time = "t", x = "x", y = "y", normalize_xy = FALSE)
  expect_error(restrict_to_circumference(ts), "relative", ignore.case = TRUE)
})

test_that("restrict_to_circumference drops a fully-beyond track and messages", {
  ts <- mk_track(c(1.2, 1.3, 1.4))                   # entire track beyond
  expect_message(out <- restrict_to_circumference(ts, "truncate"), "removed")
  expect_equal(nrow(as.data.frame(out)), 0L)
})

test_that("restrict_to_circumference logs a transform-history entry", {
  ts  <- mk_track(c(0, 0.3, 1.4, 0.3, 0))
  out <- suppressMessages(restrict_to_circumference(ts, "drop"))
  expect_true("restrict_to_circumference" %in% transform_history(out)$step)
})

test_that("restrict_to_circumference shortens cpunctatus tracks and removes the glitch", {
  data(cpunctatus, package = "radiatR")
  tr <- suppressMessages(restrict_to_circumference(cpunctatus, "truncate"))
  l0 <- track_length(cpunctatus); l1 <- track_length(tr)
  m  <- merge(l0, l1, by = "trial_id", suffixes = c("_0", "_1"))
  expect_true(all(m$length_1 <= m$length_0 + 1e-9))
  d  <- as.data.frame(tr)
  expect_true(all(sqrt(d$rel_x^2 + d$rel_y^2) <= 1 + 1e-6))   # the 1.347 glitch gone
})

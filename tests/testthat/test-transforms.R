# A trivial per-trajectory transform: add a constant to the angle column.
.add_const <- function(df, cols, k) {
  df[[cols$angle]] <- (df[[cols$angle]] + k) %% (2 * pi)
  df
}

test_that("apply_transform by='trajectory' modifies the angle and keeps structure", {
  ts  <- cpunctatus
  out <- apply_transform(ts, .add_const, k = 0.3, by = "trajectory",
                         step = "add_const")
  expect_s4_class(out, "TrajSet")
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
  expect_s4_class(apply_transform(ts, flip, by = "all"), "TrajSet")
  expect_s4_class(apply_transform(ts, flip, by = "trajectory"), "TrajSet")
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

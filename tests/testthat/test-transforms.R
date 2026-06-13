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

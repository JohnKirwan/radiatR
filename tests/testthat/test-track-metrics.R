test_that("path_straightness: straight path is 1, return-to-start is 0", {
  expect_equal(path_straightness(c(0, 1, 2, 3), c(0, 0, 0, 0)), 1)
  expect_equal(path_straightness(c(0, 1, 0), c(0, 1, 0)), 0)
})

test_that("path_straightness matches the analytic value for an L-shaped path", {
  # (0,0) -> (0,1) -> (1,1): length 2, net sqrt(2) => sqrt(2)/2
  expect_equal(path_straightness(c(0, 0, 1), c(0, 1, 1)), sqrt(2) / 2,
               tolerance = 1e-12)
})

test_that("path_straightness is scale-invariant", {
  x <- c(0, 1, 1, 2); y <- c(0, 0.5, 1.5, 1)
  expect_equal(path_straightness(x, y), path_straightness(10 * x, 10 * y),
               tolerance = 1e-12)
})

test_that("path_straightness handles degenerate input", {
  expect_true(is.na(path_straightness(numeric(0), numeric(0))))
  expect_true(is.na(path_straightness(1, 1)))              # single point
  expect_true(is.na(path_straightness(c(2, 2, 2), c(3, 3, 3))))  # zero length
  # NA coordinates are dropped before measuring
  expect_equal(path_straightness(c(0, NA, 2), c(0, NA, 0)), 1)
})

test_that("straightness_index returns one bounded value per trajectory", {
  ts <- simulate_tracks(conditions = data.frame(n_trials = 5L),
                        n_points = 40, seed = 4, output = "trajset")
  si <- straightness_index(ts)
  idc <- ts@cols$id

  expect_s3_class(si, "data.frame")
  expect_named(si, c(idc, "straightness"))
  expect_equal(nrow(si), length(unique(ts@data[[idc]])))
  vals <- si$straightness[is.finite(si$straightness)]
  expect_true(all(vals >= 0 & vals <= 1))
})

test_that("straightness_index orders points by time before measuring", {
  ts <- simulate_tracks(conditions = data.frame(n_trials = 2L),
                        n_points = 30, seed = 9, output = "trajset")
  idc <- ts@cols$id
  ordered <- straightness_index(ts)

  # Shuffling the row order must not change the result: the function reorders
  # each trajectory by its time column.
  shuffled <- ts
  set.seed(1)
  shuffled@data <- shuffled@data[sample(nrow(shuffled@data)), , drop = FALSE]
  expect_equal(
    straightness_index(shuffled)[order(straightness_index(shuffled)[[idc]]), ],
    ordered[order(ordered[[idc]]), ],
    ignore_attr = TRUE
  )
})

test_that("straightness_index rejects non-TrajSet input", {
  expect_error(straightness_index(data.frame(x = 1, y = 1)), "TrajSet")
})

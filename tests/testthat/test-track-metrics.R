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

test_that("path_tortuosity is the reciprocal of straightness; straight = 1", {
  expect_equal(path_tortuosity(c(0, 1, 2, 3), c(0, 0, 0, 0)), 1)
  expect_equal(path_tortuosity(c(0, 0, 1), c(0, 1, 1)), sqrt(2),
               tolerance = 1e-12)
  x <- c(0, 1, 1, 2); y <- c(0, 0.5, 1.5, 1)
  expect_equal(path_tortuosity(x, y), 1 / path_straightness(x, y),
               tolerance = 1e-12)
})

test_that("path_tortuosity is Inf when the path returns to its start", {
  expect_identical(path_tortuosity(c(0, 1, 0), c(0, 1, 0)), Inf)
})

test_that("path_tortuosity handles degenerate input", {
  expect_true(is.na(path_tortuosity(numeric(0), numeric(0))))
  expect_true(is.na(path_tortuosity(1, 1)))                     # single point
  expect_true(is.na(path_tortuosity(c(2, 2), c(3, 3))))         # zero length
})

test_that("path_tortuosity is scale-invariant", {
  x <- c(0, 1, 2); y <- c(0, 1, 0)
  expect_equal(path_tortuosity(x, y), path_tortuosity(5 * x, 5 * y),
               tolerance = 1e-12)
})

test_that("tortuosity_ratio returns one value >= 1 per trajectory", {
  ts <- simulate_tracks(conditions = data.frame(n_trials = 5L),
                        n_points = 40, seed = 4, output = "trajset")
  tr <- tortuosity_ratio(ts)
  idc <- ts@cols$id

  expect_named(tr, c(idc, "tortuosity"))
  expect_equal(nrow(tr), length(unique(ts@data[[idc]])))
  vals <- tr$tortuosity[is.finite(tr$tortuosity)]
  expect_true(all(vals >= 1 - 1e-9))

  # Reciprocal of the straightness index, trajectory by trajectory.
  si <- straightness_index(ts)
  m  <- match(tr[[idc]], si[[idc]])
  ok <- is.finite(tr$tortuosity) & si$straightness[m] > 0
  expect_equal(tr$tortuosity[ok], 1 / si$straightness[m][ok],
               tolerance = 1e-9)
})

test_that("tortuosity_ratio rejects non-TrajSet input", {
  expect_error(tortuosity_ratio(data.frame(x = 1, y = 1)), "TrajSet")
})

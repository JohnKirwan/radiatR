mk_ts <- function(ids, time, fps = NULL, distance_scale = NULL, distance_unit = NULL,
                   normalize_xy = FALSE) {
  df <- data.frame(id = ids, time = time, angle = rep(0, length(ids)))
  ts <- methods::new("Tracks", data = df,
                      cols = list(id = "id", time = "time", angle = "angle"),
                      angle_unit = "radians",
                      meta = list(normalize_xy = normalize_xy))
  if (!is.null(fps)) ts <- set_frame_rate(ts, fps)
  if (!is.null(distance_scale)) ts <- set_distance_scale(ts, distance_scale, distance_unit)
  ts
}

test_that("c() on Tracks errors when a trajectory id collides across inputs", {
  t1 <- mk_ts(ids = c("a", "a"), time = c(1, 2))
  t2 <- mk_ts(ids = c("a", "b"), time = c(1, 1))

  expect_error(c(t1, t2), "'a'")
})

test_that("c() on Tracks errors on conflicting frame_rate metadata", {
  t1 <- mk_ts(ids = c("a", "a"), time = c(1, 2), fps = 30)
  t2 <- mk_ts(ids = c("b", "b"), time = c(1, 2), fps = 60)

  expect_error(c(t1, t2), "frame_rate")
})

test_that("c() on Tracks errors on conflicting distance_scale/unit metadata", {
  t1 <- mk_ts(ids = c("a", "a"), time = c(1, 2), distance_scale = 1, distance_unit = "cm")
  t2 <- mk_ts(ids = c("b", "b"), time = c(1, 2), distance_scale = 2, distance_unit = "cm")
  expect_error(c(t1, t2), "distance_scale")

  t3 <- mk_ts(ids = c("c", "c"), time = c(1, 2), distance_scale = 1, distance_unit = "mm")
  expect_error(c(t1, t3), "distance_unit")
})

test_that("c() on Tracks errors on conflicting normalize_xy metadata", {
  t1 <- mk_ts(ids = c("a", "a"), time = c(1, 2), normalize_xy = TRUE)
  t2 <- mk_ts(ids = c("b", "b"), time = c(1, 2), normalize_xy = FALSE)

  expect_error(c(t1, t2), "normalize_xy")
})

test_that("c() on Tracks errors when calibration is set in one input but not another", {
  t1 <- mk_ts(ids = c("a", "a"), time = c(1, 2), fps = 30)
  t2 <- mk_ts(ids = c("b", "b"), time = c(1, 2))

  expect_error(c(t1, t2), "frame_rate")
})

test_that("c() on Tracks succeeds and preserves both inputs' calibration when compatible", {
  t1 <- mk_ts(ids = c("a", "a"), time = c(1, 2), fps = 30, distance_scale = 1, distance_unit = "cm")
  t2 <- mk_ts(ids = c("b", "b"), time = c(1, 2), fps = 30, distance_scale = 1, distance_unit = "cm")

  combined <- c(t1, t2)
  expect_equal(frame_rate(combined), 30)
  expect_equal(distance_scale(combined), 1)
  expect_equal(distance_unit(combined), "cm")
  expect_setequal(unique(combined@data$id), c("a", "b"))
})

test_that("c() on Tracks merges trial_limits and sums oob_points/oob_trials from every input", {
  t1 <- mk_ts(ids = c("a", "a"), time = c(1, 2))
  t1@meta$trial_limits <- tibble::tibble(vid_ord = "a")
  t1@meta$oob_points <- 3L
  t1@meta$oob_trials <- 1L
  t2 <- mk_ts(ids = c("b", "b"), time = c(1, 2))
  t2@meta$trial_limits <- tibble::tibble(vid_ord = "b")
  t2@meta$oob_points <- 2L
  t2@meta$oob_trials <- 1L

  combined <- c(t1, t2)
  expect_setequal(combined@meta$trial_limits$vid_ord, c("a", "b"))
  expect_equal(combined@meta$oob_points, 5L)
  expect_equal(combined@meta$oob_trials, 2L)
})

test_that("c() on Tracks merges per-id reference metadata from every input", {
  t1 <- mk_ts(ids = c("a", "a"), time = c(1, 2))
  t1@meta$reference <- tibble::tibble(id = "a", ref_theta = 0.5)
  t2 <- mk_ts(ids = c("b", "b"), time = c(1, 2))
  t2@meta$reference <- tibble::tibble(id = "b", ref_theta = 1.5)

  combined <- c(t1, t2)
  ref <- reference(combined)
  expect_setequal(ref$id, c("a", "b"))
  expect_equal(ref$ref_theta[ref$id == "a"], 0.5)
  expect_equal(ref$ref_theta[ref$id == "b"], 1.5)
})

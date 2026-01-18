test_that("simulate_tracks returns tibble by default", {
  set.seed(1)
  sim <- simulate_tracks(n_points = 50, seed = 1)
  expect_s3_class(sim, "tbl_df")
  expect_true(all(c("condition", "trial_id", "predictor") %in% names(sim)))
})

test_that("simulate_tracks can return TrajSet and write file", {
  tmp <- tempfile(fileext = ".csv")
  res <- simulate_tracks(n_points = 40, output = "both", write_path = tmp, seed = 2)
  expect_true(file.exists(tmp))
  expect_s3_class(res$tibble, "tbl_df")
  expect_s4_class(res$trajset, "TrajSet")
  expect_equal(nrow(as.data.frame(res$trajset)), nrow(res$tibble))
})

test_that("simulate_tracks respects condition effects", {
  conds <- tibble::tibble(
    condition = c("low", "high"),
    n_trials = c(50L, 50L),
    stim_mean = 0,
    concentration_base = c(2, 8),
    concentration_slope = c(0, 0),
    tortuosity_base = c(0.12, 0.04),
    tortuosity_slope = c(0, 0),
    tortuosity_sd = c(0.02, 0.01)
  )
  sim <- simulate_tracks(conditions = conds, n_points = 60, seed = 3)
  final_frame <- sim[sim$frame == 60, ]
  summary <- stats::aggregate(final_frame[, c("concentration", "tortuosity")],
                              by = list(condition = final_frame$condition),
                              FUN = mean)
  high <- summary[summary$condition == "high", ]
  low <- summary[summary$condition == "low", ]
  expect_gt(high$concentration, low$concentration)
  expect_lt(high$tortuosity, low$tortuosity)
})

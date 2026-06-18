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
    ref_mean = 0,
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

test_that("default simulate_tracks output is unchanged (backward compat, pinned seed)", {
  s  <- simulate_tracks(n_points = 50, seed = 99)
  fh <- s$final_heading[!duplicated(s$trial_id)]
  expect_equal(head(fh, 3),
               c(5.9589157262, 0.0305034215, 5.5213237187), tolerance = 1e-8)
  expect_equal(head(s$abs_x, 3),
               c(0.0311209063, 0.0063145798, 0.0586878370), tolerance = 1e-8)
  expect_true(all(c("modality", "n_modes", "mode_id", "mode_mean") %in% names(s)))
  expect_true(all(s$modality == "unimodal"))
})

test_that(".sim_principal_angle dispatches per modality with ground truth", {
  set.seed(1)
  u <- radiatR:::.sim_principal_angle("uniform", ref_mean = 0, kappa = 5, n_modes = 1)
  expect_true(u$angle >= 0 && u$angle < 2 * pi); expect_true(is.na(u$mode_mean))
  a <- radiatR:::.sim_principal_angle("axial", ref_mean = 0, kappa = 50, n_modes = 1)
  expect_true(a$mode_id %in% c(1L, 2L))
  expect_lt(min(abs(a$mode_mean - c(0, pi))), 1e-9)
  m <- radiatR:::.sim_principal_angle("multimodal", ref_mean = 0, kappa = 50, n_modes = 3)
  expect_true(m$mode_id %in% 1:3)
})

test_that("modality drives recovered sample structure (directed tracks)", {
  mk <- function(mod, nmodes = 1L, kappa = 6, ntr = 90L) {
    cond <- tibble::tibble(condition = mod, n_trials = ntr, ref_mean = 0.4,
                           concentration_base = kappa, modality = mod, n_modes = nmodes)
    ts <- simulate_tracks(n_points = 60, conditions = cond, output = "trajset", seed = 7)
    derive_headings(ts, rule = "net")
  }
  expect_equal(circ_model_select(mk("uniform"))$model[1],  "uniform")
  expect_equal(circ_model_select(mk("unimodal"))$model[1], "unimodal")
  expect_equal(circ_model_select(mk("axial"))$model[1],    "axial")
  set.seed(1)
  hr <- test_uniformity(mk("multimodal", nmodes = 3L, kappa = 10), test = "hermans_rasson", n_sim = 499)
  expect_lt(hr$p_value, 0.05)
})

test_that("simulate_tracks stores the generating conditions in TrajSet meta", {
  ts <- simulate_tracks(n_points = 30, output = "trajset", seed = 3)
  expect_true(!is.null(ts@meta$sim_conditions))
  expect_true("modality" %in% names(ts@meta$sim_conditions))
})

test_that(".sim_triangle_wave oscillates in [-1,1] with the requested reversals", {
  w <- radiatR:::.sim_triangle_wave(101, n_reversals = 4)
  expect_length(w, 101)
  expect_true(max(w) <= 1 + 1e-9 && min(w) >= -1 - 1e-9)
  turns <- sum(diff(sign(diff(w))) != 0)
  expect_gte(turns, 3L)
})

test_that("oscillatory tracks are axial: velocity_axis recovers the axis, net cancels", {
  # The oscillatory line-width is decoupled from tortuosity (a small fixed
  # fraction of amplitude), so the step-wise velocity-axis estimator recovers
  # the axis at the DEFAULT tortuosity -- no SNR hack required.
  cond <- tibble::tibble(condition = "osc", n_trials = 40L, ref_mean = 0.6,
                         concentration_base = 50, modality = "unimodal",
                         track_shape = "oscillatory", n_reversals = 4L, amplitude = 0.9)
  s_df <- simulate_tracks(n_points = 120, conditions = cond, seed = 11)
  expect_true("line_width" %in% names(s_df))
  ts <- simulate_tracks(n_points = 120, conditions = cond, output = "trajset", seed = 11)

  ax <- derive_headings(ts, rule = "velocity_axis")          # axis in [0, pi)
  # Compare in radians via mean_dir (mean_dir_deg applies the display convention,
  # which is not a raw degree conversion of the axis). Axis ~ 0.6 rad (mod pi).
  ax_mean <- circ_summarise(ax, "heading", units = "radians", axial = TRUE,
                            stats = "mean_dir")$mean_dir
  d <- (ax_mean %% pi - 0.6) %% pi
  expect_lt(min(d, pi - d), 20 * pi / 180)

  net <- derive_headings(ts, rule = "net")
  R <- circ_summarise(net, "heading", units = "radians", stats = "resultant_R")$resultant_R
  expect_lt(R, 0.4)
})

test_that("directed default is unchanged by adding the track_shape dimension", {
  s  <- simulate_tracks(n_points = 50, seed = 99)
  fh <- s$final_heading[!duplicated(s$trial_id)]
  expect_equal(head(fh, 3),
               c(5.9589157262, 0.0305034215, 5.5213237187), tolerance = 1e-8)
  expect_true(all(s$track_shape == "directed"))
})

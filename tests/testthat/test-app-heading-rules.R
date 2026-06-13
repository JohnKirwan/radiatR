test_that("every heading rule exposed in the app runs on the example data", {
  data(cpunctatus, package = "radiatR", envir = environment())

  # The 11 parameter-free rules offered in the app's dropdown.
  rules <- c("distal", "net", "straight", "window_net", "origin_mean",
             "velocity_mean", "maxspeed_window", "vm_fit", "pca_axis",
             "ransac_straight", "goal_bias")
  for (r in rules) {
    hd <- derive_headings(cpunctatus, rule = r, on_missing = "quiet")
    expect_s3_class(hd, "data.frame")
    expect_true("heading" %in% names(hd),
                info = paste("rule", r, "missing heading column"))
  }

  # crossing additionally needs ring radii (the app supplies these).
  hd <- derive_headings(cpunctatus, rule = "crossing",
                        circ0 = 0.3, circ1 = 0.6, on_missing = "quiet")
  expect_s3_class(hd, "data.frame")
  expect_true("heading" %in% names(hd))
})

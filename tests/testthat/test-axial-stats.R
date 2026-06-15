test_that(".fold_angles doubles only when axial; .unfold_mean halves to [0, pi)", {
  th <- c(0, pi/4, pi/2, 3*pi/2, 2*pi - 0.1)
  expect_equal(radiatR:::.fold_angles(th, FALSE), th)
  expect_equal(radiatR:::.fold_angles(th, TRUE), (2 * th) %% (2*pi))

  # directional: unfold is just wrap-to-2pi
  expect_equal(radiatR:::.unfold_mean(3.0, FALSE), 3.0 %% (2*pi))
  # axial: halve into [0, pi)
  expect_equal(radiatR:::.unfold_mean(pi,   TRUE), (pi/2) %% pi)
  expect_equal(radiatR:::.unfold_mean(3*pi, TRUE), ((3*pi)/2) %% pi)
})

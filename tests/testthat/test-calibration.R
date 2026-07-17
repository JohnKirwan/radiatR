test_that(".calibrate_to_unit_circle translates and scales uniformly", {
  x <- c(512, 812, 212)
  y <- c(384, 384, 384)
  out <- radiatR:::.calibrate_to_unit_circle(x, y, origin = c(512, 384), radius = 300)
  expect_equal(out$x, c(0, 1, -1))
  expect_equal(out$y, c(0, 0, 0))
})

test_that(".calibrate_to_unit_circle preserves bearing of a point from the origin", {
  # A point due-north of origin stays due-north (positive y, zero x) after mapping.
  out <- radiatR:::.calibrate_to_unit_circle(512, 684, origin = c(512, 384), radius = 300)
  expect_equal(out$x, 0)
  expect_equal(out$y, 1)
})

test_that(".calibrate_to_unit_circle leaves non-finite coordinates non-finite", {
  out <- radiatR:::.calibrate_to_unit_circle(c(512, NA), c(384, 384),
                                             origin = c(512, 384), radius = 300)
  expect_equal(out$x[1], 0)
  expect_true(is.na(out$x[2]))
})

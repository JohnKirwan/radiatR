test_that("distal return_coords adds the furthest point matching the heading", {
  hd <- derive_headings(cpunctatus, rule = "distal", return_coords = TRUE)
  expect_true(all(c("x_distal", "y_distal") %in% names(hd)))

  ok  <- is.finite(hd$heading)
  ang <- atan2(hd$y_distal[ok], hd$x_distal[ok]) %% (2 * pi)
  expect_equal(ang, hd$heading[ok], tolerance = 1e-8)

  # the returned point is the maximum-radius point of its track
  idc <- cpunctatus@cols$id; xc <- cpunctatus@cols$x; yc <- cpunctatus@cols$y
  dat <- cpunctatus@data
  i   <- which(ok)[1]
  trk <- dat[dat[[idc]] == hd$id[i], ]
  rmax <- which.max(sqrt(trk[[xc]]^2 + trk[[yc]]^2))
  expect_equal(hd$x_distal[i], trk[[xc]][rmax])
  expect_equal(hd$y_distal[i], trk[[yc]][rmax])

  # default output is unchanged (no extra columns)
  hd0 <- derive_headings(cpunctatus, rule = "distal")
  expect_false(any(c("x_distal", "y_distal") %in% names(hd0)))
})

test_that("net return_coords adds start/end matching the heading", {
  hd <- derive_headings(cpunctatus, rule = "net", return_coords = TRUE)
  expect_true(all(c("x_start", "y_start", "x_end", "y_end") %in% names(hd)))

  ok  <- is.finite(hd$heading)
  ang <- atan2(hd$y_end[ok] - hd$y_start[ok], hd$x_end[ok] - hd$x_start[ok]) %% (2 * pi)
  expect_equal(ang, hd$heading[ok], tolerance = 1e-8)

  hd0 <- derive_headings(cpunctatus, rule = "net")
  expect_false(any(c("x_start", "x_end") %in% names(hd0)))
})

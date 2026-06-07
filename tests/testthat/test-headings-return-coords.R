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

test_that("straight return_coords adds run endpoints that lie on the track", {
  hd <- derive_headings(cpunctatus, rule = "straight", return_coords = TRUE)
  expect_true(all(c("x_seg0", "y_seg0", "x_seg1", "y_seg1") %in% names(hd)))

  idc <- cpunctatus@cols$id; xc <- cpunctatus@cols$x; yc <- cpunctatus@cols$y
  dat <- cpunctatus@data
  ok  <- which(is.finite(hd$heading) & is.finite(hd$x_seg0))
  expect_gt(length(ok), 0)

  for (i in ok) {
    trk <- dat[dat[[idc]] == hd$id[i], ]
    on0 <- any(abs(trk[[xc]] - hd$x_seg0[i]) < 1e-9 & abs(trk[[yc]] - hd$y_seg0[i]) < 1e-9)
    on1 <- any(abs(trk[[xc]] - hd$x_seg1[i]) < 1e-9 & abs(trk[[yc]] - hd$y_seg1[i]) < 1e-9)
    # endpoints are real points of the track; they may coincide for a track whose
    # straightest run is stationary (the rule treats a zero-displacement run as
    # turn 0), so the columns faithfully report whatever segment the rule chose
    expect_true(on0 && on1)
  }

  hd0 <- derive_headings(cpunctatus, rule = "straight")
  expect_false(any(c("x_seg0", "x_seg1") %in% names(hd0)))
})

test_that("pca_axis return_coords adds a unit axis and centroid matching the heading", {
  hd <- derive_headings(cpunctatus, rule = "pca_axis", return_coords = TRUE)
  expect_true(all(c("x_centroid", "y_centroid", "axis_x", "axis_y") %in% names(hd)))

  ok <- is.finite(hd$heading)
  expect_equal(sqrt(hd$axis_x[ok]^2 + hd$axis_y[ok]^2), rep(1, sum(ok)), tolerance = 1e-8)

  ang <- atan2(hd$axis_y[ok], hd$axis_x[ok]) %% (2 * pi)
  expect_equal(ang, hd$heading[ok], tolerance = 1e-8)

  # centroid is the per-track column mean of the positions
  idc <- cpunctatus@cols$id; xc <- cpunctatus@cols$x; yc <- cpunctatus@cols$y
  dat <- cpunctatus@data
  cen_x <- tapply(dat[[xc]], dat[[idc]], mean)
  cen_y <- tapply(dat[[yc]], dat[[idc]], mean)
  expect_equal(unname(hd$x_centroid[ok]), as.vector(cen_x[as.character(hd$id[ok])]), tolerance = 1e-8)
  expect_equal(unname(hd$y_centroid[ok]), as.vector(cen_y[as.character(hd$id[ok])]), tolerance = 1e-8)

  hd0 <- derive_headings(cpunctatus, rule = "pca_axis")
  expect_false(any(c("axis_x", "x_centroid") %in% names(hd0)))
})

test_that("trajectory mode cycles a key on a data frame, capped at n", {
  hd <- data.frame(id = c("t1", "t2", "t3", "t1"), heading = 0)
  out <- assign_colour_key(hd, by = "trajectory", n = 20)
  expect_true(".colour" %in% names(out))
  expect_s3_class(out$.colour, "factor")
  expect_equal(as.integer(out$.colour), c(1L, 2L, 3L, 1L))
  expect_identical(levels(out$.colour), as.character(1:20))
})

test_that("reference borrows the level order so two frames share a key", {
  ts <- simulate_tracks(n_points = 20, seed = 1, output = "trajset")
  id_col <- ts@cols$id
  ts2 <- assign_colour_key(ts, by = "trajectory", n = 20)
  hd  <- derive_headings(ts, rule = "distal")
  hd2 <- assign_colour_key(hd, by = "trajectory", n = 20, reference = ts)
  trk <- setNames(as.integer(ts2@data$.colour), ts2@data[[id_col]])
  expect_equal(as.integer(hd2$.colour), unname(trk[hd2$id]))
})

test_that("low-cardinality column mode keeps raw values + factor levels (legend)", {
  hd <- data.frame(id = 1:4, grp = factor(c("a", "b", "a", "c")), heading = 0)
  out <- assign_colour_key(hd, by = "grp", n = 20)
  expect_identical(as.character(out$.colour), c("a", "b", "a", "c"))
  expect_identical(levels(out$.colour), c("a", "b", "c"))
})

test_that("high-cardinality column mode cycles (caps at n)", {
  hd <- data.frame(id = 1:25, grp = paste0("g", 1:25), heading = 0)
  out <- assign_colour_key(hd, by = "grp", n = 20)
  expect_lte(length(unique(out$.colour)), 20L)
  expect_identical(levels(out$.colour), as.character(1:20))
})

test_that("custom `into` name and NA handling", {
  hd <- data.frame(id = c("t1", NA, "t2"), heading = 0)
  out <- assign_colour_key(hd, by = "trajectory", n = 20, into = ".cc")
  expect_true(".cc" %in% names(out))
  expect_true(is.na(out$.cc[2]))
})

test_that("borrows a colour column from the reference by trajectory id", {
  ts <- simulate_tracks(n_points = 20, seed = 1, output = "trajset")
  hd <- derive_headings(ts, rule = "distal")            # has no "condition" column
  out <- assign_colour_key(hd, by = "condition", reference = ts)
  df  <- as.data.frame(ts)
  expect_identical(as.character(out$.colour),
                   as.character(df$condition[match(hd$id, df[[ts@cols$id]])]))
})

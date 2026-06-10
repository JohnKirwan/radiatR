# cycle_colours(): the order-stable colour-cycling primitive shared by
# assign_cycle_colours() (and radiate()) and the Shiny app's track/marker colour
# keys. A single source of truth for "cycle n colours over the values of a key".

test_that("cycle_colours numbers values by first appearance, wrapping at n", {
  x <- c("a", "b", "c", "a", "b")
  out <- cycle_colours(x, n = 20L)
  expect_s3_class(out, "factor")
  expect_identical(levels(out), as.character(1:20))
  expect_equal(as.integer(out), c(1L, 2L, 3L, 1L, 2L))
})

test_that("cycle_colours wraps a high-cardinality key into n colours", {
  x <- paste0("t", 1:25)
  out <- cycle_colours(x, n = 20L)
  expect_lte(length(unique(out)), 20L)
  # t21 -> 1, t25 -> 5
  expect_equal(as.integer(out)[c(21, 25)], c(1L, 5L))
})

test_that("an explicit `levels` order makes two vectors share the same key", {
  # two frames that mention the trajectories in different orders still map a
  # given trajectory to the same colour when given a common level order.
  lev <- c("t3", "t1", "t2")
  a <- cycle_colours(c("t1", "t2", "t3"), n = 20L, levels = lev)
  b <- cycle_colours(c("t3", "t3", "t1"), n = 20L, levels = lev)
  expect_equal(as.integer(a), c(2L, 3L, 1L))   # t1->2, t2->3, t3->1
  expect_equal(as.integer(b), c(1L, 1L, 2L))   # t3->1, t3->1, t1->2
})

test_that("cycle_colours preserves NA", {
  out <- cycle_colours(c("a", NA, "b", NA), n = 20L)
  expect_true(all(is.na(out[c(2, 4)])))
  expect_equal(as.integer(out)[c(1, 3)], c(1L, 2L))
})

test_that("cycle_colours validates n", {
  expect_error(cycle_colours(letters, n = 0L), "positive")
  expect_error(cycle_colours(letters, n = -1L), "positive")
})

test_that("assign_cycle_colours is consistent with cycle_colours (no panels)", {
  df  <- data.frame(id = c("a", "b", "c", "a"))
  got <- assign_cycle_colours(df, id_col = "id", n = 4L)$cycle_colour
  expect_equal(got, cycle_colours(df$id, n = 4L))
})

test_that("derive_coords reproduces the loader mapping's derived fields", {
  set.seed(1)
  origin <- c(100, 100); reference <- c(160, 100)   # radius 60, ref at East
  m  <- build_unit_circle_mapping(origin, reference, flip_y = TRUE)
  x  <- runif(20, 40, 160); y <- runif(20, 40, 160)
  mp <- m$map(x, y)

  dc <- derive_coords(mp$trans_x, mp$trans_y, reference = m$ref_theta_unit)
  expect_equal(dc$trans_rho,       mp$trans_rho)
  expect_equal(dc$abs_theta_clock, mp$abs_theta_clock)
  expect_equal(dc$abs_theta_unit,  mp$abs_theta_unit)
  expect_equal(dc$rel_theta_unit,  mp$rel_theta_unit)
  expect_equal(dc$rel_x,           mp$rel_x)
  expect_equal(dc$rel_y,           mp$rel_y)
})

test_that("derive_coords with reference 0 gives relative == absolute", {
  dc <- derive_coords(c(0.5, -0.3), c(0.2, 0.4), reference = 0)
  expect_equal(dc$rel_theta_unit, dc$abs_theta_unit)
  expect_equal(dc$rel_x, c(0.5, -0.3))
  expect_equal(dc$rel_y, c(0.2, 0.4))
})

test_that("reference() is empty by default and .reference_lookup defaults to 0", {
  ts <- simulate_tracks(n_points = 20, seed = 3, output = "trajset")
  expect_true(is.null(reference(ts)) || nrow(reference(ts)) == 0L)
  lut <- radiatR:::.reference_lookup(ts)
  expect_true(all(lut == 0))
  expect_equal(length(lut), length(ids(ts)))
})

test_that("the object-position pipeline records a per-trajectory reference", {
  tmp_dir <- tempfile("radiatr_ref"); dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))
  landmarks <- data.frame(frame = c(1, 1, 101, 101),
                          x = c(0, 45, 0, -42), y = c(0, 0, 0, 0))
  tracks <- data.frame(
    frame = 1:200,
    x = c(seq(0, 40, length.out = 100), seq(0, -38, length.out = 100)),
    y = c(seq(0, 8,  length.out = 100), seq(0, -6,  length.out = 100)))
  write.table(landmarks, file.path(tmp_dir, "video_demo_point01.txt"),
              sep = "\t", col.names = FALSE, row.names = FALSE)
  write.table(tracks, file.path(tmp_dir, "video_demo_point02.txt"),
              sep = "\t", col.names = FALSE, row.names = FALSE)
  landmarks_df <- read.delim(file.path(tmp_dir, "video_demo_point01.txt"), header = FALSE)[, 1:3]
  names(landmarks_df) <- c("frame", "x", "y")
  tracks_df <- read.delim(file.path(tmp_dir, "video_demo_point02.txt"), header = FALSE)[, 1:3]
  names(tracks_df) <- c("frame", "x", "y")
  file_tbl <- import_tracks(tmp_dir)

  ts <- suppressWarnings(get_all_object_pos(landmarks_df, tracks_df, file_tbl, tmp_dir))
  ref <- reference(ts)
  expect_true(is.data.frame(ref))
  expect_true(all(c("id", "ref_theta") %in% names(ref)))
  expect_setequal(ref$id, ids(ts))
})

test_that("set_reference re-derives the relative frame consistently", {
  ts  <- cpunctatus
  off <- 0.4
  out <- set_reference(ts, off)             # scalar reference for all trajectories

  cols <- out@cols
  d <- derive_coords(out@data[[cols$x]], out@data[[cols$y]], reference = off)
  expect_equal(out@data[[cols$angle]], d$rel_theta_unit)
  expect_equal(out@data[[cols$rel_x]], d$rel_x)
  expect_equal(out@data[[cols$rel_y]], d$rel_y)
  expect_true(all(reference(out)$ref_theta == off))
  expect_equal(nrow(transform_history(out)),
               nrow(transform_history(ts)) + length(ids(ts)))
  expect_true("set_reference" %in% transform_history(out)$step)
})

test_that("set_reference with reference 0 makes relative == absolute", {
  out  <- set_reference(cpunctatus, 0)
  cols <- out@cols
  d <- derive_coords(out@data[[cols$x]], out@data[[cols$y]], reference = 0)
  expect_equal(out@data[[cols$angle]], d$abs_theta_unit)
  expect_equal(out@data[[cols$rel_x]], out@data[[cols$x]])
  expect_equal(out@data[[cols$rel_y]], out@data[[cols$y]])
})

test_that("set_reference accepts per-trajectory values", {
  ts   <- cpunctatus
  ids0 <- ids(ts)
  vals <- stats::setNames(seq(0, 1, length.out = length(ids0)), ids0)
  out  <- set_reference(ts, vals)
  expect_equal(reference(out)$ref_theta[match(ids0, reference(out)$id)],
               unname(vals))
})

# cpunctatus predates Phase 1, so it has no @meta$reference even though its
# stored relative frame was built with per-trial references. Reconstruct that
# reference (ref = abs_theta - rel_theta per trajectory) so the materialiser
# re-derives the same values it would have stored.
.with_reconstructed_reference <- function(ts) {
  d <- ts@data
  ref_id <- tapply(seq_len(nrow(d)), d[[ts@cols$id]],
                   function(i) (d$abs_theta[i] - d$rel_theta[i])[1] %% (2 * pi))
  set_reference(ts, stats::setNames(as.numeric(ref_id), names(ref_id)))
}

test_that("as.data.frame restores derived columns dropped from @data", {
  ts   <- .with_reconstructed_reference(cpunctatus)
  full <- ts@data
  drop <- c("rel_x", "rel_y", "radius", "trans_rho", "abs_theta")  # keep cols$angle present

  ts2 <- ts
  for (cc in drop) ts2@data[[cc]] <- NULL
  expect_false(any(drop %in% names(ts2@data)))

  d <- as.data.frame(ts2)
  expect_true(all(drop %in% names(d)))
  for (cc in c("rel_x", "rel_y", "trans_rho", "abs_theta")) {
    expect_equal(d[[cc]], full[[cc]], info = cc)
  }
  expect_equal(d[["radius"]], full[["trans_rho"]])
})

test_that("as.data.frame is a no-op when derived columns are present", {
  ts <- cpunctatus
  expect_identical(as.data.frame(ts), ts@data)
})

test_that("consumers produce identical output without stored derived columns", {
  ts      <- .with_reconstructed_reference(cpunctatus)   # helper defined above
  ts_drop <- ts
  for (cc in c("rel_x", "rel_y", "radius", "trans_rho", "abs_theta"))
    ts_drop@data[[cc]] <- NULL
  # keep cols$angle (rel_theta) present (the registered angle role); the test
  # exercises the plotting consumer that reads rel_x/rel_y for track geometry.

  set.seed(1)
  p_full <- radiate(ts,      group_col = "trial_id", show_arrow = FALSE,
                    show_labels = FALSE)
  set.seed(1)
  p_drop <- radiate(ts_drop, group_col = "trial_id", show_arrow = FALSE,
                    show_labels = FALSE)
  fp <- function(p) {
    b <- ggplot2::ggplot_build(p)
    lapply(b$data, function(z) {
      cc <- intersect(c("x", "y"), names(z))
      round(as.matrix(z[, cc, drop = FALSE]), 6)
    })
  }
  expect_equal(fp(p_drop), fp(p_full))
})

test_that("a TrajSet may register rel_x/rel_y/rho roles whose columns are absent", {
  df <- data.frame(
    id    = rep("a", 4),
    time  = 1:4,
    trans_x = c(0.5, -0.3, 0.1, -0.2),
    trans_y = c(0.2,  0.4, -0.1, 0.3),
    rel_theta = c(0.1, 0.2, 0.3, 0.4)
  )
  ts <- TrajSet(df, id = "id", time = "time", angle = "rel_theta",
                x = "trans_x", y = "trans_y",
                rel_x = "rel_x", rel_y = "rel_y",   # roles registered, columns absent
                angle_unit = "radians", normalize_xy = FALSE)
  expect_false("rel_x" %in% names(ts@data))
  expect_false("rel_y" %in% names(ts@data))
  expect_silent(methods::validObject(ts))
  # the materializer fills them on demand
  d <- as.data.frame(ts)
  expect_true(all(c("rel_x", "rel_y") %in% names(d)))
  expect_true(is.numeric(d$rel_x))
})

test_that("a constructed TrajSet does not store a rho/radius column but materializes it", {
  ts <- simulate_tracks(n_points = 20, seed = 3, output = "trajset")
  rho_role <- ts@cols$rho
  expect_false(is.null(rho_role))            # the role is still registered
  expect_false(rho_role %in% names(ts@data)) # but the column is not stored
  expect_silent(methods::validObject(ts))

  d <- as.data.frame(ts)
  expect_true(rho_role %in% names(d))        # materialized on demand
  expect_equal(d[[rho_role]],
               sqrt(d[[ts@cols$x]]^2 + d[[ts@cols$y]]^2))
})

test_that("trial utilities summarise and transform tracks", {
  landmarks <- data.frame(
    frame = c(1, 2, 6, 7),
    x = c(0, 1, 0, -1),
    y = c(0, 0, 0, 0)
  )
  track <- data.frame(
    frame = 1:10,
    x = seq(0, -1, length.out = 10),
    y = seq(0, 0.2, length.out = 10)
  )
  file_tbl <- tibble::tibble(
    basename = "video1",
    arc = 0,
    type = "stimulus",
    obstacle = "none",
    id = "animal"
  )

  limits <- suppressWarnings(get_trial_limits(landmarks, track, file_tbl, vid_num = 1))
  expect_equal(nrow(limits), 2)
  expect_equal(limits$video[1], "video1")
  expect_true(all(c("ref_theta", "r_px") %in% names(limits)))

  track_ts <- suppressWarnings(get_tracked_object_pos(limits, track))
  expect_s4_class(track_ts, "Tracks")
  expect_true(nrow(as.data.frame(track_ts)) > 0)
  expect_s3_class(track_ts@meta$trial_limits, "data.frame")

  tmp <- withr::local_tempdir()
  track_path <- file.path(tmp, "video1_point02.txt")
  landmark_path <- file.path(tmp, "video1_point01.txt")
  utils::write.table(landmarks, landmark_path, sep = "\t", col.names = FALSE, row.names = FALSE)
  utils::write.table(track, track_path, sep = "\t", col.names = FALSE, row.names = FALSE)

  file_tbl_disk <- tibble::tibble(
    basename = "video1",
    landmark = basename(landmark_path),
    track = basename(track_path)
  )

  agg <- suppressWarnings(get_all_object_pos(file_tbl = file_tbl_disk, track_dir = tmp))
  expect_s4_class(agg, "Tracks")
  df <- as.data.frame(agg)
  expect_true(all(c("rel_x", "rel_y") %in% names(df)))
  expect_s3_class(agg@meta$trial_limits, "data.frame")
})

test_that("get_tracked_object_pos sets display convention meta keys", {
  landmarks <- data.frame(
    frame = c(1, 2, 6, 7),
    x = c(0, 1, 0, -1),
    y = c(0, 0, 0, 0)
  )
  track <- data.frame(
    frame = 1:10,
    x = seq(0, -1, length.out = 10),
    y = seq(0, 0.2, length.out = 10)
  )
  file_tbl <- tibble::tibble(
    basename = "video1", arc = 0, type = "stimulus", obstacle = "none", id = "animal"
  )
  limits <- suppressWarnings(get_trial_limits(landmarks, track, file_tbl, vid_num = 1))
  ts     <- suppressWarnings(get_tracked_object_pos(limits, track))
  expect_equal(ts@meta$display_convention, "clock")
  expect_equal(ts@meta$plot_x_col,         "rel_x")
  expect_equal(ts@meta$plot_y_col,         "rel_y")
})

test_that("get_tracked_object_pos reports out-of-bounds points once, aggregated over trials", {
  # unit radius = 1 px (origin (0,0) -> reference (1,0)); two trials.
  landmarks <- data.frame(frame = c(1, 2, 6, 7),
                          x = c(0, 1, 0, 1), y = c(0, 0, 0, 0))
  # both trials contain points well beyond radius 1 (unit-circle-mapped coords kept as-is).
  track <- data.frame(frame = 1:10,
                             x = c(0, 0.5, 1.2, 1.5, 0.3, 0, 0.5, 1.3, 1.6, 0.2),
                             y = 0)
  file_tbl <- tibble::tibble(basename = "video1", arc = 0, type = "stimulus",
                             obstacle = "none", id = "animal")
  limits <- suppressWarnings(get_trial_limits(landmarks, track, file_tbl, vid_num = 1))

  msgs <- testthat::capture_messages(
    suppressWarnings(get_tracked_object_pos(limits, track)))
  oob <- grep("exceeded the unit circle boundary", msgs, value = TRUE)
  expect_length(oob, 1L)                 # one message, not one per trial
  expect_match(oob, "across 2 trials")   # aggregated across both trials
})

test_that("get_all_object_pos reports out-of-bounds points once, aggregated over files", {
  # unit radius = 1 px; each video has two trials with points beyond radius 1.
  landmarks <- data.frame(frame = c(1, 2, 6, 7),
                          x = c(0, 1, 0, 1), y = c(0, 0, 0, 0))
  track <- data.frame(frame = 1:10,
                       x = c(0, 0.5, 1.2, 1.5, 0.3, 0, 0.5, 1.3, 1.6, 0.2),
                       y = 0)

  tmp <- withr::local_tempdir()
  rows <- lapply(1:3, function(k) {
    base <- sprintf("video%d", k)
    lm_path <- file.path(tmp, sprintf("%s_point01.txt", base))
    tr_path <- file.path(tmp, sprintf("%s_point02.txt", base))
    utils::write.table(landmarks, lm_path, sep = "\t", col.names = FALSE, row.names = FALSE)
    utils::write.table(track, tr_path, sep = "\t", col.names = FALSE, row.names = FALSE)
    tibble::tibble(basename = base,
                   landmark = basename(lm_path),
                   track = basename(tr_path))
  })
  file_tbl_disk <- do.call(rbind, rows)

  msgs <- testthat::capture_messages(
    suppressWarnings(get_all_object_pos(file_tbl = file_tbl_disk, track_dir = tmp)))
  oob <- grep("exceeded the unit circle boundary", msgs, value = TRUE)
  expect_length(oob, 1L)                 # one message, not one per file
  expect_match(oob, "across 6 trials")   # aggregated across 3 files x 2 trials
})

test_that("get_all_object_pos reads its files strictly and names the file", {
  tmp <- withr::local_tempdir()
  lm <- c("1\t0\t0", "2\t1\t0", "6\t0\t0", "7\t-1\t0")
  trk <- sprintf("%d\t%s\t%s", 1:10, format(seq(0, -1, length.out = 10)),
                 format(seq(0, 0.2, length.out = 10)))
  run <- function(landmarks = lm, track = trk) {
    writeLines(landmarks, file.path(tmp, "v_point01.txt"))
    writeLines(track, file.path(tmp, "v_point02.txt"))
    ft <- tibble::tibble(basename = "v", landmark = "v_point01.txt", track = "v_point02.txt")
    suppressWarnings(get_all_object_pos(file_tbl = ft, track_dir = tmp))
  }
  expect_s4_class(run(), "Tracks")
  e <- expect_error(run(track = gsub(".", ",", trk, fixed = TRUE)),
                    class = "radiatR_malformed_values")
  expect_identical(e$source, "v_point02.txt")
  bad <- trk; bad[5] <- "5\tabc\t0.1"
  e <- expect_error(run(track = bad), class = "radiatR_malformed_values")
  expect_identical(c(e$columns, e$rows), c("x", "5"))
  bad <- trk; bad[5] <- "5\t0.1"
  e <- expect_error(run(track = bad), class = "radiatR_malformed_file")
  expect_identical(c(e$problem, e$lines), c("columns", "5"))
  expect_match(conditionMessage(e), "^get_all_object_pos: file 'v_point02.txt'")
  # fields past the third need not be on every line
  extra <- trk; extra[c(2, 7)] <- paste0(extra[c(2, 7)], "\t\tnote")
  expect_s4_class(run(track = extra), "Tracks")
  e <- expect_error(run(landmarks = c("1\t0", "2\t1")), class = "radiatR_malformed_file")
  expect_identical(e$problem, "columns")
  e <- expect_error(run(landmarks = lm[1:3]), class = "radiatR_malformed_file")
  expect_identical(c(e$problem, e$source), c("odd_landmarks", "v_point01.txt"))
  # a blank line is no row, and its file keeps its line numbers
  bad <- c(trk[1:2], "", trk[3:4], "4\t0.1")
  e <- expect_error(run(track = bad), class = "radiatR_malformed_file")
  expect_identical(c(e$problem, e$lines), c("columns", "6"))
  # every trial starts far from the centre
  far <- sprintf("%d\t%s\t0", 1:10, format(seq(-0.8, -1, length.out = 10)))
  expect_error(run(track = far), class = "radiatR_no_valid_trials")
})

test_that("the loader pipeline exposes `track`, not `animal_track`", {
  for (fn in list(get_trial_limits, get_tracked_object_pos, get_all_object_pos)) {
    args <- names(formals(fn))
    expect_true("track" %in% args)
    expect_false("animal_track" %in% args)
  }
})

test_that("first_past gives no landmark for a radius the track never reaches", {
  landmarks <- data.frame(frame = c(1, 2), x = c(0, 1), y = c(0, 0))
  track <- data.frame(frame = 1:5, x = c(0, .05, .12, .15, .1), y = 0)  # past .1, never .2
  file_tbl <- tibble::tibble(basename = "v", arc = 0, type = "stimulus",
                             landmark = "v_point01.txt", track = "v_point02.txt")
  tl <- get_trial_limits(landmarks, track, file_tbl, vid_num = 1)
  expect_warning(ts <- get_tracked_object_pos(tl, track, circ0 = .1, circ1 = .2),
                 "outer landmark is NA")
  lim <- ts@meta$trial_limits
  expect_equal(lim$x0, .12)
  expect_true(is.na(lim$x1) && is.na(lim$y1))
})

test_that("get_all_object_pos keeps the trial_limits of a file with no valid trial", {
  landmarks <- data.frame(frame = c(1, 2, 6, 7), x = c(0, 1, 0, 1), y = c(0, 0, 0, 0))
  good <- data.frame(frame = 1:10, x = c(0, .5, .8, .9, .3, 0, .5, .8, .9, .2), y = 0)
  far  <- data.frame(frame = 1:10, x = 5, y = 0)                    # every trial invalid
  tmp <- withr::local_tempdir()
  rows <- lapply(c("good", "far"), function(base) {
    utils::write.table(landmarks, file.path(tmp, paste0(base, "_point01.txt")),
                       sep = "\t", col.names = FALSE, row.names = FALSE)
    utils::write.table(get(base), file.path(tmp, paste0(base, "_point02.txt")),
                       sep = "\t", col.names = FALSE, row.names = FALSE)
    tibble::tibble(basename = base, landmark = paste0(base, "_point01.txt"),
                   track = paste0(base, "_point02.txt"))
  })
  ts <- suppressWarnings(get_all_object_pos(file_tbl = do.call(rbind, rows), track_dir = tmp))
  lim <- ts@meta$trial_limits
  expect_equal(nrow(lim), 4L)
  expect_identical(lim$valid_track, c(TRUE, TRUE, FALSE, FALSE))
  expect_true(all(is.na(lim[3:4, c("x0", "y0", "x1", "y1")])))
  expect_true(all(is.na(lim$order[3:4])))
  expect_type(lim$order, "character")
})

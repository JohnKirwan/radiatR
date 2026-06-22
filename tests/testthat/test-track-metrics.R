test_that("path_straightness: straight path is 1, return-to-start is 0", {
  expect_equal(path_straightness(c(0, 1, 2, 3), c(0, 0, 0, 0)), 1)
  expect_equal(path_straightness(c(0, 1, 0), c(0, 1, 0)), 0)
})

test_that("path_straightness matches the analytic value for an L-shaped path", {
  # (0,0) -> (0,1) -> (1,1): length 2, net sqrt(2) => sqrt(2)/2
  expect_equal(path_straightness(c(0, 0, 1), c(0, 1, 1)), sqrt(2) / 2,
               tolerance = 1e-12)
})

test_that("path_straightness is scale-invariant", {
  x <- c(0, 1, 1, 2); y <- c(0, 0.5, 1.5, 1)
  expect_equal(path_straightness(x, y), path_straightness(10 * x, 10 * y),
               tolerance = 1e-12)
})

test_that("path_straightness handles degenerate input", {
  expect_true(is.na(path_straightness(numeric(0), numeric(0))))
  expect_true(is.na(path_straightness(1, 1)))              # single point
  expect_true(is.na(path_straightness(c(2, 2, 2), c(3, 3, 3))))  # zero length
  # NA coordinates are dropped before measuring
  expect_equal(path_straightness(c(0, NA, 2), c(0, NA, 0)), 1)
})

test_that("straightness_index returns one bounded value per trajectory", {
  ts <- simulate_tracks(conditions = data.frame(n_trials = 5L),
                        n_points = 40, seed = 4, output = "trajset")
  si <- straightness_index(ts)
  idc <- ts@cols$id

  expect_s3_class(si, "data.frame")
  expect_named(si, c(idc, "straightness"))
  expect_equal(nrow(si), length(unique(ts@data[[idc]])))
  vals <- si$straightness[is.finite(si$straightness)]
  expect_true(all(vals >= 0 & vals <= 1))
})

test_that("straightness_index orders points by time before measuring", {
  ts <- simulate_tracks(conditions = data.frame(n_trials = 2L),
                        n_points = 30, seed = 9, output = "trajset")
  idc <- ts@cols$id
  ordered <- straightness_index(ts)

  # Shuffling the row order must not change the result: the function reorders
  # each trajectory by its time column.
  shuffled <- ts
  set.seed(1)
  shuffled@data <- shuffled@data[sample(nrow(shuffled@data)), , drop = FALSE]
  expect_equal(
    straightness_index(shuffled)[order(straightness_index(shuffled)[[idc]]), ],
    ordered[order(ordered[[idc]]), ],
    ignore_attr = TRUE
  )
})

test_that("straightness_index rejects non-Tracks input", {
  expect_error(straightness_index(data.frame(x = 1, y = 1)), "Tracks")
})

test_that("path_tortuosity is the reciprocal of straightness; straight = 1", {
  expect_equal(path_tortuosity(c(0, 1, 2, 3), c(0, 0, 0, 0)), 1)
  expect_equal(path_tortuosity(c(0, 0, 1), c(0, 1, 1)), sqrt(2),
               tolerance = 1e-12)
  x <- c(0, 1, 1, 2); y <- c(0, 0.5, 1.5, 1)
  expect_equal(path_tortuosity(x, y), 1 / path_straightness(x, y),
               tolerance = 1e-12)
})

test_that("path_tortuosity is Inf when the path returns to its start", {
  expect_identical(path_tortuosity(c(0, 1, 0), c(0, 1, 0)), Inf)
})

test_that("path_tortuosity handles degenerate input", {
  expect_true(is.na(path_tortuosity(numeric(0), numeric(0))))
  expect_true(is.na(path_tortuosity(1, 1)))                     # single point
  expect_true(is.na(path_tortuosity(c(2, 2), c(3, 3))))         # zero length
})

test_that("path_tortuosity is scale-invariant", {
  x <- c(0, 1, 2); y <- c(0, 1, 0)
  expect_equal(path_tortuosity(x, y), path_tortuosity(5 * x, 5 * y),
               tolerance = 1e-12)
})

test_that("tortuosity_ratio returns one value >= 1 per trajectory", {
  ts <- simulate_tracks(conditions = data.frame(n_trials = 5L),
                        n_points = 40, seed = 4, output = "trajset")
  tr <- tortuosity_ratio(ts)
  idc <- ts@cols$id

  expect_named(tr, c(idc, "tortuosity"))
  expect_equal(nrow(tr), length(unique(ts@data[[idc]])))
  vals <- tr$tortuosity[is.finite(tr$tortuosity)]
  expect_true(all(vals >= 1 - 1e-9))

  # Reciprocal of the straightness index, trajectory by trajectory.
  si <- straightness_index(ts)
  m  <- match(tr[[idc]], si[[idc]])
  ok <- is.finite(tr$tortuosity) & si$straightness[m] > 0
  expect_equal(tr$tortuosity[ok], 1 / si$straightness[m][ok],
               tolerance = 1e-9)
})

test_that("tortuosity_ratio rejects non-Tracks input", {
  expect_error(tortuosity_ratio(data.frame(x = 1, y = 1)), "Tracks")
})

test_that("step_speed: constant 1 unit/frame at 30 fps -> 30 units/s", {
  x <- 0:4; y <- rep(0, 5); secs <- (0:4) / 30
  expect_equal(radiatR::step_speed(x, y, secs), rep(30, 4))
})

test_that("step_speed: L-shaped move and POSIXct-style seconds", {
  expect_equal(radiatR::step_speed(c(0, 0, 3), c(0, 4, 4), c(0, 1, 2)), c(4, 3))
  expect_equal(length(radiatR::step_speed(1, 1, 0)), 0L)          # n < 2 -> empty
})

test_that("step_speed: dt<=0 or non-finite endpoint -> NA in that step", {
  expect_equal(radiatR::step_speed(c(0, 1, 2), c(0, 0, 0), c(0, 0, 1)), c(NA, 1))      # dt=0
  expect_equal(radiatR::step_speed(c(0, NA, 2), c(0, 0, 0), c(0, 1, 2)), c(NA_real_, NA_real_))    # non-finite
})

# --- Tracks-level fixture: two tracks, known constant speeds, numeric frames ---
mk_speed_ts <- function() {
  d <- rbind(
    data.frame(id = "a", frame = 0:3, x = (0:3) * 2, y = 0, angle = 0),  # 2 units/frame
    data.frame(id = "b", frame = 0:2, x = 0:2,       y = 0, angle = 0)   # 1 unit/frame
  )
  methods::new("Tracks", data = d,
               cols = list(id = "id", time = "frame", angle = "angle", x = "x", y = "y"),
               angle_unit = "radians", meta = list())
}

test_that("track_speed needs a frame rate for numeric frames; then gives real units", {
  ts <- mk_speed_ts()
  expect_error(track_speed(ts), "frame rate")
  ts <- set_frame_rate(ts, 30)
  s <- track_speed(ts)                                   # mean, default
  expect_named(s, c("id", "speed"))
  expect_equal(s$speed[s$id == "a"], 60)                 # 2 units/frame * 30 fps
  expect_equal(s$speed[s$id == "b"], 30)
})

test_that("track_speed stat arg selects the reduction; <2-point track -> NA", {
  ts <- set_frame_rate(mk_speed_ts(), 30)
  expect_equal(track_speed(ts, stat = "max")$speed[1], 60)
  expect_equal(track_speed(ts, stat = "median")$speed[1], 60)
  one <- methods::new("Tracks",
    data = data.frame(id = "z", frame = 0L, x = 0, y = 0, angle = 0),
    cols = list(id = "id", time = "frame", angle = "angle", x = "x", y = "y"),
    angle_unit = "radians", meta = list(frame_rate = 30))
  expect_true(is.na(track_speed(one)$speed))
})

test_that("track_speed: POSIXct time works without a frame rate", {
  t0 <- as.POSIXct("2020-01-01", tz = "UTC")
  d <- data.frame(id = "a", frame = t0 + c(0, 1, 2), x = c(0, 3, 6), y = 0, angle = 0)
  ts <- methods::new("Tracks", data = d,
    cols = list(id = "id", time = "frame", angle = "angle", x = "x", y = "y"),
    angle_unit = "radians", meta = list())
  expect_equal(track_speed(ts)$speed, 3)                 # 3 units / 1 s
})

test_that("instantaneous_speed: per-row, NA at each track's first point, real units", {
  ts <- set_frame_rate(mk_speed_ts(), 30)
  v  <- instantaneous_speed(ts)
  expect_length(v, nrow(ts@data))
  d  <- ts@data
  first_rows <- tapply(seq_len(nrow(d)), d$id, min)
  expect_true(all(is.na(v[first_rows])))               # NA at each track's first point
  expect_equal(v[d$id == "a"][-1], rep(60, sum(d$id == "a") - 1))  # 2 units/frame * 30
  expect_equal(v[d$id == "b"][-1], rep(30, sum(d$id == "b") - 1))
})

test_that("instantaneous_speed: numeric frames need a frame rate; POSIXct does not", {
  expect_error(instantaneous_speed(mk_speed_ts()), "frame rate")
  t0 <- as.POSIXct("2020-01-01", tz = "UTC")
  d  <- data.frame(id = "a", frame = t0 + c(0, 1, 2), x = c(0, 3, 6), y = 0, angle = 0)
  ts <- methods::new("Tracks", data = d,
    cols = list(id = "id", time = "frame", angle = "angle", x = "x", y = "y"),
    angle_unit = "radians", meta = list())
  expect_equal(instantaneous_speed(ts), c(NA, 3, 3))
})

mk_vel_ts <- function() {
  d <- rbind(
    data.frame(id = "a", frame = 0:3, x = (0:3) * 2, y = 0,    angle = 0),  # vx 2/frame
    data.frame(id = "b", frame = 0:2, x = 0:2,       y = 0:2,  angle = 0)   # diagonal 1/frame
  )
  methods::new("Tracks", data = d,
    cols = list(id = "id", time = "frame", angle = "angle", x = "x", y = "y"),
    angle_unit = "radians", meta = list())
}

test_that("velocity_vector: per-row vx/vy, NA at each track's first point, real units", {
  ts <- set_frame_rate(mk_vel_ts(), 30)
  v  <- velocity_vector(ts)
  expect_named(v, c("vx", "vy"))
  expect_equal(nrow(v), nrow(ts@data))
  d  <- ts@data
  first <- tapply(seq_len(nrow(d)), d$id, min)
  expect_true(all(is.na(v$vx[first])) && all(is.na(v$vy[first])))
  expect_equal(v$vx[d$id == "a"][-1], rep(60, 3))     # 2/frame * 30
  expect_equal(v$vy[d$id == "a"][-1], rep(0, 3))
  expect_equal(v$vx[d$id == "b"][-1], rep(30, 2))     # diagonal: 1/frame * 30
  expect_equal(v$vy[d$id == "b"][-1], rep(30, 2))
})

test_that("velocity_vector: scales to physical units; needs a frame rate for numeric frames", {
  expect_error(velocity_vector(mk_vel_ts()), "frame rate")
  ts <- set_distance_scale(set_frame_rate(mk_vel_ts(), 30), 50, "mm")
  expect_equal(velocity_vector(ts)$vx[2], 60 * 50)    # mm/s
})

mk_turn_ts <- function() {
  # CCW (left) 90-degree turn at the middle point of track "a"; straight "b"
  d <- rbind(
    data.frame(id = "a", frame = 0:2, x = c(0, 1, 1), y = c(0, 0, 1), angle = 0),  # right then up -> +90
    data.frame(id = "b", frame = 0:2, x = 0:2,        y = 0,          angle = 0)   # straight -> 0 turn
  )
  methods::new("Tracks", data = d,
    cols = list(id = "id", time = "frame", angle = "angle", x = "x", y = "y"),
    angle_unit = "radians", meta = list())
}

test_that("angular_velocity: signed CCW turning rate, NA at track ends", {
  ts <- set_frame_rate(mk_turn_ts(), 1)        # 1 fps -> seconds == frame index
  w  <- angular_velocity(ts)
  d  <- ts@data
  # track a: interior point (row 2) has a +90deg (left) turn over dt_center = (2-0)/2 = 1 s
  a_rows <- which(d$id == "a")
  expect_equal(w[a_rows[2]], (pi / 2) / 1)
  expect_true(is.na(w[a_rows[1]]) && is.na(w[a_rows[3]]))   # first + last NA
  # track b: straight -> 0 turn
  b_rows <- which(d$id == "b")
  expect_equal(w[b_rows[2]], 0)
})

test_that("angular_velocity: CW turn is negative; degrees option; needs a frame rate", {
  d <- data.frame(id = "a", frame = 0:2, x = c(0, 1, 1), y = c(0, 0, -1), angle = 0) # right then down -> -90
  ts <- set_frame_rate(methods::new("Tracks", data = d,
    cols = list(id = "id", time = "frame", angle = "angle", x = "x", y = "y"),
    angle_unit = "radians", meta = list()), 1)
  expect_equal(angular_velocity(ts)[2], -(pi / 2))
  expect_equal(angular_velocity(ts, units = "degrees")[2], -90)
  expect_error(angular_velocity(methods::new("Tracks", data = d,
    cols = list(id = "id", time = "frame", angle = "angle", x = "x", y = "y"),
    angle_unit = "radians", meta = list())), "frame rate")
})

mk_netvel_ts <- function() {
  d <- rbind(
    data.frame(id = "a", frame = 0:3, x = (0:3) * 2, y = 0,      angle = 0),  # straight, vx 2/frame
    data.frame(id = "b", frame = 0:2, x = c(0, 1, 0), y = c(0, 1, 0), angle = 0)  # closed loop
  )
  methods::new("Tracks", data = d,
    cols = list(id = "id", time = "frame", angle = "angle", x = "x", y = "y"),
    angle_unit = "radians", meta = list())
}

test_that("track_velocity: net velocity = displacement / duration, distance-scaled", {
  ts <- set_frame_rate(mk_netvel_ts(), 30)
  v  <- track_velocity(ts)
  expect_named(v, c("id", "vx", "vy"))
  # track a: net dx = 6 over 3 frames = 0.1 s -> 6 / 0.1 = 60; vy 0
  expect_equal(v$vx[v$id == "a"], 60)
  expect_equal(v$vy[v$id == "a"], 0)
  # track b: returns to start -> net (0, 0)
  expect_equal(v$vx[v$id == "b"], 0)
  expect_equal(v$vy[v$id == "b"], 0)
  # distance scale
  ts2 <- set_distance_scale(ts, 50, "mm")
  expect_equal(track_velocity(ts2)$vx[v$id == "a"], 60 * 50)
})

test_that("track_velocity: numeric frames need a frame rate; <2-point track -> NA", {
  expect_error(track_velocity(mk_netvel_ts()), "frame rate")
  one <- methods::new("Tracks",
    data = data.frame(id = "z", frame = 0L, x = 0, y = 0, angle = 0),
    cols = list(id = "id", time = "frame", angle = "angle", x = "x", y = "y"),
    angle_unit = "radians", meta = list(frame_rate = 30))
  v <- track_velocity(one)
  expect_true(is.na(v$vx) && is.na(v$vy))
})

mk_mixturn_ts <- function() {
  # track a: +90 (left) then -90 (right): right, up, right -> turns +pi/2 then -pi/2
  d <- data.frame(id = "a", frame = 0:3,
                  x = c(0, 1, 1, 2), y = c(0, 0, 1, 1), angle = 0)
  methods::new("Tracks", data = d,
    cols = list(id = "id", time = "frame", angle = "angle", x = "x", y = "y"),
    angle_unit = "radians", meta = list())
}

test_that("track_turning: mean cancels opposite turns, mean_abs does not; max_abs is the sharpest", {
  ts <- set_frame_rate(mk_mixturn_ts(), 1)    # 1 fps -> seconds == frame index
  expect_named(track_turning(ts), c("id", "turning"))
  expect_equal(track_turning(ts, stat = "mean")$turning, 0)            # +pi/2 and -pi/2 cancel
  expect_equal(track_turning(ts, stat = "mean_abs")$turning, pi / 2)   # magnitude (dt_center 1 s)
  expect_equal(track_turning(ts, stat = "max_abs")$turning, pi / 2)
  expect_equal(track_turning(ts, stat = "mean_abs", units = "degrees")$turning, 90)
})

test_that("track_turning: <3-point track -> NA; numeric frames need a frame rate", {
  expect_error(track_turning(mk_mixturn_ts()), "frame rate")
  two <- methods::new("Tracks",
    data = data.frame(id = "z", frame = 0:1, x = c(0, 1), y = 0, angle = 0),
    cols = list(id = "id", time = "frame", angle = "angle", x = "x", y = "y"),
    angle_unit = "radians", meta = list(frame_rate = 1))
  expect_true(is.na(track_turning(two)$turning))
})

test_that("velocity_angle: per-row movement direction in [0, 2*pi), NA at first row", {
  ts <- set_frame_rate(mk_vel_ts(), 30)
  a  <- velocity_angle(ts)
  expect_length(a, nrow(ts@data))
  d  <- ts@data
  first <- tapply(seq_len(nrow(d)), d$id, min)
  expect_true(all(is.na(a[first])))                 # NA at each track's first row
  expect_equal(a[d$id == "a"][-1], rep(0, 3))       # due East -> 0
  expect_equal(a[d$id == "b"][-1], rep(pi / 4, 2))  # NE diagonal -> pi/4
  expect_true(all(a[!is.na(a)] >= 0 & a[!is.na(a)] < 2 * pi))
})

test_that("velocity_angle: westward step wraps to pi (not -pi)", {
  d <- data.frame(id = "a", frame = 0:1, x = c(0, -1), y = 0, angle = 0)  # due West
  ts <- set_frame_rate(methods::new("Tracks", data = d,
    cols = list(id = "id", time = "frame", angle = "angle", x = "x", y = "y"),
    angle_unit = "radians", meta = list()), 1)
  expect_equal(velocity_angle(ts), c(NA, pi))
})

test_that("velocity_angle: degrees option returns [0, 360)", {
  ts <- set_frame_rate(mk_vel_ts(), 30)
  a  <- velocity_angle(ts, units = "degrees")
  d  <- ts@data
  expect_equal(a[d$id == "b"][-1], rep(45, 2))      # NE diagonal -> 45 deg
  expect_true(all(a[!is.na(a)] >= 0 & a[!is.na(a)] < 360))
})

test_that("velocity_angle: scale-invariant and numeric frames need a frame rate", {
  expect_error(velocity_angle(mk_vel_ts()), "frame rate")
  ts  <- set_frame_rate(mk_vel_ts(), 30)
  scaled <- set_distance_scale(ts, 50, "mm")
  expect_equal(velocity_angle(scaled), velocity_angle(ts))   # direction unchanged by scale
})

test_that("velocity_angle: output composes with the circular stack", {
  ts <- set_frame_rate(mk_vel_ts(), 30)
  a  <- stats::na.omit(velocity_angle(ts))
  r  <- circular::rho.circular(circular::circular(a, units = "radians"))
  expect_true(is.finite(as.numeric(r)) && r >= 0 && r <= 1)  # a valid resultant length
})

#' Simulate trajectory sets under configurable experimental conditions
#'
#' This module generates synthetic trajectories whose directional concentration
#' and tortuosity vary across experimental conditions and continuous predictors.
#' It is useful for creating reproducible examples, tutorials, and test fixtures
#' without relying on large raw tracking files.
#'
#' @param n_points Integer number of frames per trajectory.
#' @param conditions Optional data frame describing experimental conditions. See
#'   Details for column descriptions. When omitted, a three-condition template is
#'   used.
#' @param output Character string controlling the return type: "tibble"
#'   (default) returns a long-form data frame, "trajset" returns a [Tracks]
#'   object, and "both" returns a list containing both representations.
#' @param write_path Optional file path (CSV) to which the simulated data should
#'   be written.
#' @param seed Optional integer seed supplied to [set.seed()] for
#'   reproducibility.
#' @param radial_noise Standard deviation of radial noise applied to the unit
#'   radius profile.
#' @param phi Autocorrelation parameter (0-1) used when generating angular noise
#'   series; higher values produce smoother paths.
#' @param frame_rate Optional capture frame rate (frames per second) attached to
#'   the returned [Tracks] via [set_frame_rate()] when `output` is `"trajset"` or
#'   `"both"`. The default `NULL` leaves the output unchanged.
#'
#' @details
#' The `conditions` data frame can contain the following columns (defaults are
#' supplied when missing):
#'
#' - `condition` (character): condition label.
#' - `n_trials` (integer): number of trajectories to simulate for the condition.
#' - `ref_mean` (numeric radian): baseline reference heading (default 0).
#' - `mean_slope` (numeric, default 0): per-condition slope shifting the mean
#'   heading with the predictor. The effective per-trial mean is
#'   `ref_mean + mean_slope * predictor` and is recorded in `ref_heading`. A
#'   default of 0 reproduces the historical seeded output byte-for-byte.
#' - `concentration_base` (numeric): baseline von Mises concentration (kappa).
#' - `concentration_slope` (numeric): optional slope applied to the predictor.
#' - `tortuosity_base` (numeric): baseline angular noise scale.
#' - `tortuosity_slope` (numeric): slope applied to the predictor.
#' - `tortuosity_sd` (numeric): additional random variation per trial.
#' - `predictor_mean`, `predictor_sd` (numeric): parameters used to sample the
#'   per-trial predictor when explicit values are not supplied.
#' - `predictor_values` (list-column): optional explicit predictor values (length
#'   `n_trials`) overriding the generated values.
#' - `modality` (character): the sample modality from which per-trial principal
#'   headings are drawn. One of `"unimodal"` (default), `"uniform"`, `"axial"`,
#'   or `"multimodal"`, `"unimodal_uniform"` (a directed mode over a uniform
#'   background; `mix_weight` sets the directed fraction), or `"bimodal"` (an
#'   asymmetric two-von-Mises mixture; `mix_weight`, `mode2_mean`, `kappa2` set
#'   the second mode). Controls the *distribution of headings across trials*,
#'   not the within-trial path shape.
#' - `n_modes` (integer): number of evenly spaced modes used when
#'   `modality == "multimodal"` (default 1; ignored by other modalities).
#' - `track_shape` (character): the *within-track* path shape. One of
#'   `"directed"` (default) -- a single sweep towards `final_heading` -- or
#'   `"oscillatory"` -- back-and-forth motion along the principal axis
#'   `final_heading`. Oscillatory tracks form a genuinely axial position cloud,
#'   so the position-based axial methods (`pca_axis`, `ransac_straight`) recover
#'   the axis at default settings; the directional methods (e.g. `net`) cancel.
#'   The step-based `velocity_axis` recovers the axis only when sampling is
#'   coarse enough that the per-step axial motion exceeds the perpendicular
#'   line-width jitter (see the `line_width` note below).
#' - `n_reversals` (integer): number of direction reversals in an oscillatory
#'   track (default 3; ignored when `track_shape == "directed"`).
#' - `amplitude` (numeric): peak excursion along the axis for an oscillatory
#'   track, clamped to `[1e-3, 1]` (default 0.9; ignored when directed).
#' - `line_width` (numeric): half-width of an oscillatory track expressed as a
#'   fraction of `amplitude`, controlling the perpendicular Gaussian jitter
#'   (default 0.05, clamped to `[1e-4, 1]`; ignored when directed). The
#'   line-width is intentionally independent of `tortuosity_*`, so the track is
#'   a genuinely thin line and the principal axis is recoverable by the
#'   position-based methods (`pca_axis`, `ransac_straight`) at default settings.
#'   The step-based `velocity_axis` is sampling-density sensitive: because the
#'   per-frame along-axis step shrinks with `n_points` while the perpendicular
#'   jitter step does not, dense sampling lets the jitter dominate and the
#'   estimate flips toward the perpendicular.
#'
#' The predictor can represent any continuous covariate (e.g. reference
#' intensity). The final heading concentration increases with larger kappa,
#' whereas larger tortuosity values produce more sinuous paths.
#'
#' For each trial the principal (final) heading is drawn according to the
#' condition's `modality`: `"unimodal"` draws from a single von Mises about
#' `ref_mean`; `"uniform"` draws from a circular uniform distribution;
#' `"axial"` draws from a von Mises about `ref_mean` or `ref_mean + pi` with
#' equal probability; `"multimodal"` draws from one of `n_modes` von Mises
#' components evenly spaced around the circle starting at `ref_mean`. The
#' `"unimodal"` branch is identical to the historical draw, so seeded output is
#' unchanged from earlier versions.
#'
#' For an `"oscillatory"` track the position sweeps back and forth along the
#' axis `final_heading` following a deterministic triangle wave of amplitude
#' `amplitude` with `n_reversals` direction changes, plus a small Gaussian
#' jitter perpendicular to the axis with standard deviation
#' `amplitude * line_width`. This line-width is independent of the tortuosity
#' settings, so the track stays a thin line and the axis remains recoverable by
#' the position-based methods (`pca_axis`, `ransac_straight`) at default
#' settings. The step-based `velocity_axis` recovers the same axis only at
#' coarse sampling, since its per-step axial signal shrinks with `n_points`
#' while the perpendicular jitter step does not. The
#' `"directed"` branch is byte-identical to the historical geometry (and never
#' draws the perpendicular jitter), so the default seeded output is unchanged.
#'
#' Every simulated row records the ground-truth generating structure in
#' additional columns: `modality` (character), `n_modes` (integer), `mode_id`
#' (integer index of the component the heading came from), `mode_mean`
#' (numeric radian mean of that component as the un-wrapped generating centre,
#' e.g. `ref_mean + pi` for the second axial pole, versus `final_heading` which
#' is the wrapped draw; `NA` for `"uniform"`),
#' `track_shape` (character), `n_reversals` (integer), `amplitude`
#' (numeric) and `line_width` (numeric). When a `Tracks` is returned, the
#' resolved generating conditions
#' are stored in `meta$sim_conditions`.
#'
#' @return Depending on `output`, a tibble, a `Tracks`, or a list containing
#'   both. When `write_path` is supplied the data are also written to disk.
#' @examples
#' sim <- simulate_tracks(seed = 1)
#' head(sim)
#'
#' # Request both tibble and Tracks representations
#' sim_both <- simulate_tracks(output = "both", seed = 123)
#' names(sim_both)
#'
#' example_conditions <- tibble::tibble(
#'   condition = paste0("level_", 1:4),
#'   n_trials = 30L,
#'   ref_mean = seq(-pi/8, pi/8, length.out = 4),
#'   concentration_base = c(2, 4, 6, 8),
#'   concentration_slope = 0.8,
#'   tortuosity_base = c(0.12, 0.09, 0.06, 0.04),
#'   tortuosity_slope = -0.01,
#'   tortuosity_sd = 0.015,
#'   predictor_mean = seq(-1, 1, length.out = 4),
#'   predictor_sd = 0.15
#' )
#' sim_four_levels <- simulate_tracks(
#'   n_points = 120,
#'   conditions = example_conditions,
#'   output = "both",
#'   seed = 42
#' )
#' aggregate(trial_id ~ condition, data = sim_four_levels$tibble, length)
#' names(sim_four_levels)
#'
#' @seealso \code{\link{circ_model_select}}, \code{\link{test_uniformity}},
#'   \code{\link{derive_headings}}
#' @importFrom stats runif rnorm
#' @importFrom circular rvonmises
#' @importFrom utils write.csv
#' @export
simulate_tracks <- function(n_points = 200,
                            conditions = NULL,
                            output = c("tibble", "trajset", "both"),
                            write_path = NULL,
                            seed = NULL,
                            radial_noise = 0.02,
                            phi = 0.85,
                            frame_rate = NULL) {
  stopifnot(n_points > 1)
  if (!is.null(seed)) {
    stopifnot(is.numeric(seed), length(seed) == 1)
    set.seed(as.integer(seed))
  }

  output <- match.arg(output)
  cond_tbl <- .sim_default_conditions(conditions)

  trials <- lapply(seq_len(nrow(cond_tbl)), function(i) {
    .sim_condition_trials(cond_tbl[i, ], n_points = n_points,
                          radial_noise = radial_noise, phi = phi)
  })
  tracks_tbl <- do.call(rbind, trials)

  if (!is.null(write_path)) {
    utils::write.csv(tracks_tbl, write_path, row.names = FALSE)
  }

  if (output == "tibble") {
    return(tracks_tbl)
  }
  trajset <- .sim_as_trajset(tracks_tbl, cond_tbl)
  if (!is.null(frame_rate)) trajset <- set_frame_rate(trajset, frame_rate)
  if (output == "trajset") {
    return(trajset)
  }
  list(tibble = tracks_tbl, trajset = trajset)
}

.sim_default_conditions <- function(conditions) {
  if (is.null(conditions)) {
    conditions <- tibble::tibble(
      condition = c("control", "moderate", "high"),
      n_trials = c(20L, 20L, 20L),
      ref_mean = c(0, pi / 6, -pi / 4),
      concentration_base = c(8, 4, 2),
      concentration_slope = c(1, 0.5, -0.2),
      tortuosity_base = c(0.04, 0.07, 0.1),
      tortuosity_slope = c(-0.01, 0, 0.02),
      tortuosity_sd = c(0.01, 0.015, 0.02),
      predictor_mean = c(0, 0.5, 1),
      predictor_sd = c(0.2, 0.2, 0.3)
    )
  }
  if (!"condition" %in% names(conditions)) {
    conditions$condition <- paste0("condition_", seq_len(nrow(conditions)))
  }
  if (!"n_trials" %in% names(conditions)) {
    conditions$n_trials <- 10L
  }
  defaults <- list(
    ref_mean = 0,
    concentration_base = 5,
    concentration_slope = 0,
    tortuosity_base = 0.06,
    tortuosity_slope = 0,
    tortuosity_sd = 0.01,
    predictor_mean = 0,
    predictor_sd = 0.2
  )
  for (nm in names(defaults)) {
    if (!nm %in% names(conditions)) {
      conditions[[nm]] <- defaults[[nm]]
    }
  }
  if (!"mean_slope" %in% names(conditions))  conditions$mean_slope <- 0
  if (!is.numeric(conditions$mean_slope))
    stop("simulate_tracks: mean_slope must be numeric.")
  if (!"modality" %in% names(conditions))    conditions$modality    <- "unimodal"
  if (!"n_modes" %in% names(conditions))     conditions$n_modes     <- 1L
  if (!"mix_weight" %in% names(conditions))  conditions$mix_weight  <- 0.6
  if (!"mode2_mean" %in% names(conditions))  conditions$mode2_mean  <- NA_real_
  if (!"kappa2" %in% names(conditions))      conditions$kappa2      <- NA_real_
  ok_mod <- conditions$modality %in%
    c("unimodal", "uniform", "axial", "multimodal", "unimodal_uniform", "bimodal")
  if (!all(ok_mod))
    stop("simulate_tracks: unknown modality value(s): ",
         paste(unique(conditions$modality[!ok_mod]), collapse = ", "))
  if (any(conditions$n_modes < 1L))
    stop("simulate_tracks: n_modes must be >= 1.")
  if (!"track_shape" %in% names(conditions)) conditions$track_shape <- "directed"
  if (!"n_reversals" %in% names(conditions))  conditions$n_reversals <- 3L
  if (!"amplitude" %in% names(conditions))    conditions$amplitude   <- 0.9
  ok_shape <- conditions$track_shape %in% c("directed", "oscillatory")
  if (!all(ok_shape))
    stop("simulate_tracks: unknown track_shape value(s): ",
         paste(unique(conditions$track_shape[!ok_shape]), collapse = ", "))
  conditions$amplitude <- pmin(pmax(conditions$amplitude, 1e-3), 1)
  if (!"line_width" %in% names(conditions))   conditions$line_width  <- 0.05
  conditions$line_width <- pmin(pmax(conditions$line_width, 1e-4), 1)
  conditions
}

# Deterministic triangle wave in [-1, 1] over n points with `n_reversals`
# direction changes (no RNG, so it never perturbs the directed path's seed).
.sim_triangle_wave <- function(n, n_reversals) {
  t <- seq(0, 1, length.out = n)
  (2 / pi) * asin(sin(pi * n_reversals * t))
}

.sim_condition_trials <- function(condition_row, n_points, radial_noise, phi) {
  n_trials <- as.integer(condition_row$n_trials)
  predictor_vals <- .sim_predictor_values(condition_row, n_trials)

  trials <- lapply(seq_len(n_trials), function(idx) {
    .sim_single_trial(
      condition = condition_row$condition,
      trial_index = idx,
      predictor = predictor_vals[idx],
      n_points = n_points,
      ref_mean = condition_row$ref_mean,
      mean_slope = condition_row$mean_slope,
      concentration_base = condition_row$concentration_base,
      concentration_slope = condition_row$concentration_slope,
      tortuosity_base = condition_row$tortuosity_base,
      tortuosity_slope = condition_row$tortuosity_slope,
      tortuosity_sd = condition_row$tortuosity_sd,
      modality = condition_row$modality,
      n_modes = condition_row$n_modes,
      mix_weight = condition_row$mix_weight,
      mode2_mean = condition_row$mode2_mean,
      kappa2     = condition_row$kappa2,
      track_shape = condition_row$track_shape,
      n_reversals = condition_row$n_reversals,
      amplitude = condition_row$amplitude,
      line_width = condition_row$line_width,
      radial_noise = radial_noise,
      phi = phi
    )
  })
  do.call(rbind, trials)
}

# Draw one principal angle per trial under the chosen sample modality, returning
# the angle plus the true component it came from (ground truth). The "unimodal"
# branch is byte-identical to the historical inline draw, so the default seeded
# output is unchanged.
.sim_principal_angle <- function(modality, ref_mean, kappa, n_modes,
                                 mix_weight = 0.6, mode2_mean = NA_real_,
                                 kappa2 = NA_real_) {
  rvm <- function(mu) {
    mu_c <- circular::circular(mu, units = "radians", type = "angles",
                               modulo = "asis", zero = 0, rotation = "counter")
    as.numeric(circular::rvonmises(1, mu = mu_c, kappa = kappa))
  }
  switch(modality,
    unimodal   = list(angle = rvm(ref_mean), mode_id = 1L, mode_mean = ref_mean),
    uniform    = list(angle = stats::runif(1, 0, 2 * pi), mode_id = 1L, mode_mean = NA_real_),
    axial      = {
      p  <- sample(c(0, pi), 1L)
      mm <- ref_mean + p
      list(angle = rvm(mm), mode_id = if (p == 0) 1L else 2L, mode_mean = mm)
    },
    multimodal = {
      j  <- sample.int(n_modes, 1L)
      mm <- ref_mean + (j - 1L) * 2 * pi / n_modes
      list(angle = rvm(mm), mode_id = j, mode_mean = mm)
    },
    unimodal_uniform = {
      if (stats::runif(1) < mix_weight)
        list(angle = rvm(ref_mean), mode_id = 1L, mode_mean = ref_mean)
      else
        list(angle = stats::runif(1, 0, 2 * pi), mode_id = 2L, mode_mean = NA_real_)
    },
    bimodal = {
      m2 <- if (is.na(mode2_mean)) ref_mean + 2 * pi / 3 else mode2_mean
      k2 <- if (is.na(kappa2)) kappa else kappa2
      if (stats::runif(1) < mix_weight)
        list(angle = rvm(ref_mean), mode_id = 1L, mode_mean = ref_mean)
      else {
        mu_c <- circular::circular(m2, units = "radians", type = "angles",
                                   modulo = "asis", zero = 0, rotation = "counter")
        list(angle = as.numeric(circular::rvonmises(1, mu = mu_c, kappa = k2)),
             mode_id = 2L, mode_mean = m2)
      }
    },
    stop("simulate_tracks: unknown modality '", modality,
         "'. Use unimodal, uniform, axial, multimodal, unimodal_uniform, or bimodal.")
  )
}

.sim_predictor_values <- function(condition_row, n_trials) {
  if ("predictor_values" %in% names(condition_row) && !is.null(condition_row$predictor_values[[1]])) {
    vals <- condition_row$predictor_values[[1]]
    if (length(vals) != n_trials) {
      stop("`predictor_values` must have length equal to `n_trials` for each condition.")
    }
    return(as.numeric(vals))
  }
  stats::rnorm(n_trials, mean = condition_row$predictor_mean, sd = condition_row$predictor_sd)
}

.sim_single_trial <- function(condition, trial_index, predictor, n_points,
                              ref_mean, mean_slope, concentration_base, concentration_slope,
                              tortuosity_base, tortuosity_slope, tortuosity_sd,
                              modality, n_modes, mix_weight, mode2_mean, kappa2,
                              track_shape, n_reversals, amplitude, line_width,
                              radial_noise, phi) {
  predictor <- as.numeric(predictor)
  kappa <- max(0.1, concentration_base + concentration_slope * predictor)
  ref_theta <- ref_mean + mean_slope * predictor
  # Use modulo="asis" so rvonmises returns values in (-pi, pi], keeping
  # wrap_to_pi() downstream correct.  modulo="2pi" (from .as_circ) would
  # shift near-zero headings to near 2*pi, which wrap_to_pi maps to -pi.
  pa            <- .sim_principal_angle(modality, ref_theta, kappa, n_modes,
                                        mix_weight = mix_weight,
                                        mode2_mean = mode2_mean,
                                        kappa2     = kappa2)
  final_heading <- pa$angle

  sigma_mean <- tortuosity_base + tortuosity_slope * predictor
  sigma <- max(1e-4, sigma_mean + stats::rnorm(1, sd = tortuosity_sd))

  if (identical(track_shape, "oscillatory")) {
    s_t <- amplitude * .sim_triangle_wave(n_points, n_reversals)
    ux  <- cos(final_heading); uy <- sin(final_heading)
    vx  <- -sin(final_heading); vy <- cos(final_heading)
    w_t <- stats::rnorm(n_points, 0, amplitude * line_width)
    abs_x <- s_t * ux + w_t * vx
    abs_y <- s_t * uy + w_t * vy
    abs_theta <- wrap_to_pi(atan2(abs_y, abs_x))
    rho <- pmin(sqrt(abs_x^2 + abs_y^2), 1)
    rel_theta <- wrap_to_pi(abs_theta - final_heading)
    rel_x <- rho * cos(rel_theta); rel_y <- rho * sin(rel_theta)
  } else {
    angle_noise <- .sim_angle_series(n_points, sigma = sigma, phi = phi)
    abs_theta <- wrap_to_pi(final_heading + angle_noise)
    rel_theta <- wrap_to_pi(abs_theta - final_heading)
    rho <- seq(0, 1, length.out = n_points) + stats::rnorm(n_points, sd = radial_noise)
    rho <- pmin(pmax(rho, 0), 1)
    abs_x <- rho * cos(abs_theta); abs_y <- rho * sin(abs_theta)
    rel_x <- rho * cos(rel_theta); rel_y <- rho * sin(rel_theta)
  }

  tibble::tibble(
    condition = condition,
    trial_id = sprintf("%s_%03d", condition, trial_index),
    trial_num = trial_index,
    frame = seq_len(n_points),
    predictor = predictor,
    concentration = kappa,
    tortuosity = sigma,
    ref_heading = ref_theta,
    final_heading = final_heading,
    rho = rho,
    abs_theta = abs_theta,
    rel_theta = rel_theta,
    abs_x = abs_x,
    abs_y = abs_y,
    rel_x = rel_x,
    rel_y = rel_y,
    modality    = modality,
    n_modes     = as.integer(n_modes),
    mode_id     = pa$mode_id,
    mode_mean   = pa$mode_mean,
    track_shape = track_shape,
    n_reversals = as.integer(n_reversals),
    amplitude   = amplitude,
    line_width  = line_width
  )
}

.sim_angle_series <- function(n, sigma, phi) {
  eps <- stats::rnorm(n, mean = 0, sd = sigma)
  out <- numeric(n)
  for (i in seq_len(n)) {
    if (i == 1) {
      out[i] <- eps[i]
    } else {
      out[i] <- phi * out[i - 1] + eps[i]
    }
  }
  out - out[n]
}

.sim_as_trajset <- function(tracks_tbl, cond_tbl) {
  tracks(
    tracks_tbl,
    id = "trial_id",
    time = "frame",
    x = "abs_x",
    y = "abs_y",
    angle = "abs_theta",
    angle_unit = "radians",
    normalize_xy = FALSE,
    meta = list(source = "simulate_tracks", sim_conditions = cond_tbl)
  )
}

wrap_to_pi <- function(angle) {
  if (is.null(angle)) return(angle)
  angle <- angle %% (2 * pi)
  angle[angle > pi] <- angle[angle > pi] - 2 * pi
  angle
}

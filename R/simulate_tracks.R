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
#'   (default) returns a long-form data frame, "trajset" returns a [TrajSet]
#'   object, and "both" returns a list containing both representations.
#' @param write_path Optional file path (CSV) to which the simulated data should
#'   be written.
#' @param seed Optional integer seed supplied to [set.seed()] for
#'   reproducibility.
#' @param radial_noise Standard deviation of radial noise applied to the unit
#'   radius profile.
#' @param phi Autocorrelation parameter (0-1) used when generating angular noise
#'   series; higher values produce smoother paths.
#'
#' @details
#' The `conditions` data frame can contain the following columns (defaults are
#' supplied when missing):
#'
#' - `condition` (character): condition label.
#' - `n_trials` (integer): number of trajectories to simulate for the condition.
#' - `stim_mean` (numeric radian): baseline stimulus heading (default 0).
#' - `concentration_base` (numeric): baseline von Mises concentration (kappa).
#' - `concentration_slope` (numeric): optional slope applied to the predictor.
#' - `tortuosity_base` (numeric): baseline angular noise scale.
#' - `tortuosity_slope` (numeric): slope applied to the predictor.
#' - `tortuosity_sd` (numeric): additional random variation per trial.
#' - `predictor_mean`, `predictor_sd` (numeric): parameters used to sample the
#'   per-trial predictor when explicit values are not supplied.
#' - `predictor_values` (list-column): optional explicit predictor values (length
#'   `n_trials`) overriding the generated values.
#'
#' The predictor can represent any continuous covariate (e.g. stimulus
#' intensity). The final heading concentration increases with larger kappa,
#' whereas larger tortuosity values produce more sinuous paths.
#'
#' @return Depending on `output`, a tibble, a `TrajSet`, or a list containing
#'   both. When `write_path` is supplied the data are also written to disk.
#' @examples
#' sim <- simulate_tracks(seed = 1)
#' head(sim)
#'
#' # Request both tibble and TrajSet representations
#' sim_both <- simulate_tracks(output = "both", seed = 123)
#' names(sim_both)
#'
#' example_conditions <- tibble::tibble(
#'   condition = paste0("level_", 1:4),
#'   n_trials = 30L,
#'   stim_mean = seq(-pi/8, pi/8, length.out = 4),
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
                            phi = 0.85) {
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
  trajset <- .sim_as_trajset(tracks_tbl)
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
      stim_mean = c(0, pi / 6, -pi / 4),
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
    stim_mean = 0,
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
  conditions
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
      stim_mean = condition_row$stim_mean,
      concentration_base = condition_row$concentration_base,
      concentration_slope = condition_row$concentration_slope,
      tortuosity_base = condition_row$tortuosity_base,
      tortuosity_slope = condition_row$tortuosity_slope,
      tortuosity_sd = condition_row$tortuosity_sd,
      radial_noise = radial_noise,
      phi = phi
    )
  })
  do.call(rbind, trials)
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
                              stim_mean, concentration_base, concentration_slope,
                              tortuosity_base, tortuosity_slope, tortuosity_sd,
                              radial_noise, phi) {
  predictor <- as.numeric(predictor)
  kappa <- max(0.1, concentration_base + concentration_slope * predictor)
  stim_theta <- stim_mean
  final_heading <- as.numeric(circular::rvonmises(1, mu = stim_theta, kappa = kappa))

  sigma_mean <- tortuosity_base + tortuosity_slope * predictor
  sigma <- max(1e-4, sigma_mean + stats::rnorm(1, sd = tortuosity_sd))

  angle_noise <- .sim_angle_series(n_points, sigma = sigma, phi = phi)
  abs_theta <- wrap_to_pi(final_heading + angle_noise)
  rel_theta <- wrap_to_pi(abs_theta - final_heading)

  rho <- seq(0, 1, length.out = n_points) + stats::rnorm(n_points, sd = radial_noise)
  rho <- pmin(pmax(rho, 0), 1)

  abs_x <- rho * cos(abs_theta)
  abs_y <- rho * sin(abs_theta)
  rel_x <- rho * cos(rel_theta)
  rel_y <- rho * sin(rel_theta)

  tibble::tibble(
    condition = condition,
    trial_id = sprintf("%s_%03d", condition, trial_index),
    trial_num = trial_index,
    frame = seq_len(n_points),
    predictor = predictor,
    concentration = kappa,
    tortuosity = sigma,
    stim_heading = stim_theta,
    final_heading = final_heading,
    rho = rho,
    abs_theta = abs_theta,
    rel_theta = rel_theta,
    abs_x = abs_x,
    abs_y = abs_y,
    rel_x = rel_x,
    rel_y = rel_y
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

.sim_as_trajset <- function(tracks_tbl) {
  TrajSet(
    tracks_tbl,
    id = "trial_id",
    time = "frame",
    x = "abs_x",
    y = "abs_y",
    angle = "abs_theta",
    normalize_xy = FALSE,
    meta = list(source = "simulate_tracks")
  )
}

wrap_to_pi <- function(angle) {
  if (is.null(angle)) return(angle)
  angle <- (angle + pi) %% (2 * pi)
  angle[angle > pi] <- angle[angle > pi] - 2 * pi
  angle
}

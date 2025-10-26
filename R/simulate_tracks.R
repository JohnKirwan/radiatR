#' Simulate radial tracks with controlled tortuosity.
#'
#' This helper generates synthetic tracks radiating away from the origin. The
#' output is useful for examples, tests, and demonstrations that should not rely
#' on large raw tracking files bundled with the package. Each simulated trial is
#' assigned a stimulus direction (`stim_theta`) and a tortuosity level that
#' controls the angular noise around that direction.
#'
#' @param n_trials Number of independent trials to simulate.
#' @param n_points Number of frames (rows) per trial.
#' @param tortuosity_levels Character vector of tortuosity labels; recycled over
#'   `n_trials`. Valid values are `"low"`, `"medium"`, and `"high"`.
#' @param seed Seed passed to [set.seed()] to ensure reproducibility.
#'
#' @return A tibble with columns describing the simulated tracks, including:
#'   `trial_id`, `trial_num`, `frame`, `tortuosity`, `stim_theta`, `rho`,
#'   `abs_theta`, `rel_theta`, `rel_x`, `rel_y`, `abs_x`, and `abs_y`.
#' @examples
#' tracks <- simulate_tracks(n_trials = 3, n_points = 100, seed = 1)
#' head(tracks)
#' @importFrom stats runif rnorm
#' @export
simulate_tracks <- function(n_trials = 6,
                            n_points = 200,
                            tortuosity_levels = c("low", "medium", "high"),
                            seed = 123) {
  stopifnot(n_trials > 0, n_points > 1)
  tortuosity_levels <- tolower(tortuosity_levels)
  valid_levels <- c("low", "medium", "high")
  invalid <- setdiff(tortuosity_levels, valid_levels)
  if (length(invalid) > 0) {
    stop("Unsupported tortuosity levels: ", paste(invalid, collapse = ", "))
  }

  set.seed(seed)
  level_lookup <- c(low = 0.03, medium = 0.08, high = 0.15)
  tort_seq <- rep_len(tortuosity_levels, n_trials)

  tracks <- lapply(seq_len(n_trials), function(trial_idx) {
    level <- tort_seq[[trial_idx]]
    sigma <- level_lookup[[level]]
    stim_theta <- stats::runif(1, -pi, pi)
    rho <- seq(0, 1, length.out = n_points) +
      stats::rnorm(n_points, mean = 0, sd = 0.02)
    rho <- pmin(pmax(rho, 0), 1)

    angle_offsets <- cumsum(stats::rnorm(n_points, mean = 0, sd = sigma))
    abs_theta <- wrap_to_pi(stim_theta + angle_offsets)
    rel_theta <- wrap_to_pi(abs_theta - stim_theta)

    abs_x <- rho * cos(abs_theta)
    abs_y <- rho * sin(abs_theta)
    rel_x <- rho * cos(rel_theta)
    rel_y <- rho * sin(rel_theta)

    tibble::tibble(
      trial_id = sprintf("trial_%02d", trial_idx),
      trial_num = trial_idx,
      frame = seq_len(n_points),
      tortuosity = level,
      stim_theta = stim_theta,
      rho = rho,
      abs_theta = abs_theta,
      rel_theta = rel_theta,
      abs_x = abs_x,
      abs_y = abs_y,
      rel_x = rel_x,
      rel_y = rel_y
    )
  })

  tibble::as_tibble(do.call(rbind, tracks))
}

wrap_to_pi <- function(angle) {
  ((angle + pi) %% (2 * pi)) - pi
}

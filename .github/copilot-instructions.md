# Copilot instructions for radiatR

Purpose: quick reference for automated assistants (Copilot sessions) to run, navigate, and modify this repository safely and effectively.

---

## Quick commands (build / test / run)

- Activate project environment: renv is used and .Rprofile activates it automatically in interactive R. Use:
  - renv::restore()

- Rebuild docs (Roxygen):
  - devtools::document()

- Run tests (full suite):
  - devtools::test()
  - or testthat::test_dir("tests/testthat")

- Run a single test file (example):
  - testthat::test_file("tests/testthat/test-circular_statistics.R")
  - Or run any single file under tests/testthat by specifying its path.

- Package check:
  - devtools::check()  # runs R CMD check
  - or from shell: R CMD check .

- Install locally for iterative development:
  - devtools::install_local(".")

- Launch the Shiny app (after installation):
  - radiatR::launch_app()

- CI: GitHub Actions uses r-lib/actions/check-r-package (see .github/workflows/R-CMD-check.yaml). CI sets NOT_CRAN=true so shinytest2 browser tests run on Ubuntu runner.

---

## High-level architecture (big picture)

- Core data structure: Tracks (S4) — defined in R/Tracks.R. Tracks wrap a list of per-trial tibbles plus unit-circle geometry and a transform history. Most package functions accept or return a Tracks.

- Pipeline stages (each stage is implemented primarily in the listed R files):
  1. Import / loaders: R/loaders.R and read_tracks_* functions. A dialect-based loader registry allows adding new formats without editing core code.
  2. Coordinate transformation: R/circular_trials.R and R/circular_mapping.R convert Cartesian coordinates to unit-circle coordinates and angles.
  3. Heading computation: R/headings.R derives per-trial headings via a registry of rules (e.g., `distal`, `crossing`, `velocity_mean`, `pca_axis`, `bodypart_axis`, etc.). New rules register with register_heading_rule().
  4. Statistics: R/circular_statistics.R wraps the circular package and exposes circ_summary / circ_summarise helpers and testing utilities (Rayleigh, Hermans–Rasson, model selection).
  5. Visualization: R/circular_plotting.R implements radiate() and composable helpers (add_ticks, add_circ, add_heading_points, add_vonmises_density).

- Shiny app (inst/app): the app is a thin presentation layer over exported package functions. The app's plot-spec roundtrip ensures the UI uses package APIs (plot_spec.R::build_plot_spec → spec_to_plot/spec_to_code).

- Object contract: only a few rich types (Tracks, circ_display, circ_regression, headings_frame). Most inputs/outputs are plain tibbles. circular-package objects remain internal.

---

## Key repository conventions and patterns

- renv: the project uses renv for dependency isolation; .Rprofile activates it. Always run renv::restore() when setting up.

- Roxygen / documentation: Edit @ tags in R/ source and run devtools::document(); man/ is generated and should not be edited by hand.

- Collate order: DESCRIPTION::Collate lists R source order and matters for package build — do not reorder files accidentally.

- Registries: loader formats and heading rules use registry patterns (named lists stored in package environment). Use register_loader_* and register_heading_rule() for extensions.

- Normalised coordinates: radiatR normalises trajectories to the unit circle internally. Do not expect pixel units — upstream calibration/lens correction should be done before import.

- headings_frame: a tibble subclass that carries display conventions through dplyr verbs. When creating or transforming headings data, prefer hf_* accessors to preserve metadata.

- Tests: tests/testthat contains one file per module. Sample datasets used by tests are in data/ (cpunctatus*.rda). testthat edition 3 is used (see DESCRIPTION).

- Shiny testing: prefer shiny::testServer for app logic tests (fast, no browser). shinytest2 is used only where DOM/browser behavior is necessary; setup-shinytest2.R configures sandbox flags. CI runs shinytest2 on Ubuntu because chromote + Chrome are available.

- Do not edit NAMESPACE or man/*.Rd by hand. Use Roxygen tags and devtools::document().

- CI notes: GitHub Actions runs R-CMD-check via r-lib/actions/check-r-package and sets NOT_CRAN=true so tests guarded by skip_on_cran() still run. If reproducing CI locally, set NOT_CRAN=TRUE in the environment when appropriate.

---

## Existing assistant/config files to consult

- CLAUDE.md — contains an authoritative short guide to architecture, test commands, and app/testing guidance. Consult it for project-specific rationale and conventions.

- .github/workflows/* — CI workflows (R-CMD-check.yaml, pkgdown.yaml, shinyapps.yaml) contain useful automation details (notably the server-side shinyapps.io app deploy on releases / workflow_dispatch).

---

If maintained: suggest improvements rather than full replacements — include new top-level commands or changed CI steps in this file.

---

Created by an automated Copilot session. For adjustments (additions, more single-test examples, or test/debug recipes), reply with what to include next.

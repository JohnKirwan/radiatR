# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

`radiatR` is an R package for plotting and analyzing animal movement trajectories in circular/radial arenas. It handles the full pipeline: camera calibration → data import → coordinate transformation → circular statistics → visualization.

## Development commands

All commands run from R (or `Rscript`). The project uses `renv` for dependency isolation — the `.Rprofile` activates it automatically.

```r
# Install/restore dependencies
renv::restore()

# Rebuild documentation from Roxygen2 tags (run before check)
devtools::document()

# Run all tests
devtools::test()

# Run a single test file
testthat::test_file("tests/testthat/test-circular_statistics.R")

# Full R CMD CHECK
devtools::check()

# Install locally
devtools::install_local(".")
```

## Architecture

### Core data structure: `TrajSet` (S4 class)

`R/TrajSet.R` defines the central container. A `TrajSet` holds a list of trajectory tibbles plus metadata (arena geometry, transform history). Almost every downstream function either accepts or returns a `TrajSet`. Understand this class before touching any other module.

### Pipeline stages

1. **Calibration** (`R/calibration.R`, `R/calibration_capture.R`) — maps pixel coordinates to real-world positions via a `CalModel` S4 object. `cam_cal_pt` / `cam_cal_many` build models; `calibrate_positions` (a `TrajSet` method) applies them.

2. **Import** (`R/loaders.R`) — the largest module. Reads manifest files that describe where track data lives, then dispatches to format-specific readers (`TrajSet_read_*`). The loader format registry (`register_loader_*`) lets callers add new file formats without modifying core code.

3. **Coordinate transformation** (`R/circular_trials.R`, `R/circular_mapping.R`) — converts Cartesian pixel coordinates to unit-circle angles. `circular_trials.R` extracts per-trial segments and normalises them to a unit arena; `circular_mapping.R` handles pixel→angle arithmetic.

4. **Heading computation** (`R/headings.R`) — derives directional headings from position sequences. Uses a rule registry pattern (analogous to the loader registry) so custom heading rules can be registered and applied by name.

5. **Statistics** (`R/circular_statistics.R`) — thin wrapper around the `circular` package. `circ_summary` is the main entry point.

6. **Visualisation** (`R/circular_plotting.R`) — `radiate()` is the primary plot function; returns a `ggplot2` object. Helper functions (`add_ticks`, `add_circ`, `degree_labs`, etc.) return ggplot2 layers and are designed to be added with `+`.

### Extension points

- **New file formats**: use `register_loader_read_function` / `register_loader_info_function` (see `R/loaders.R`).
- **New heading rules**: use `register_heading_rule` (see `R/headings.R`).
- Both registries follow the same pattern: a named list stored in package env, dispatched at runtime.

### Documentation

All public API is documented with Roxygen2. `man/` is auto-generated — edit `@` tags in `R/` source, then run `devtools::document()`. Never edit `NAMESPACE` or `man/*.Rd` by hand.

### Tests

`tests/testthat/` has one file per module. `urchin_tracks.rda` (in `data/`) is the canonical fixture dataset used across multiple test files.

# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

`radiatR` is an R package for plotting and analyzing animal movement trajectories in circular/radial arenas. It handles the full pipeline: data import → coordinate transformation → circular statistics → visualization.

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

1. **Import** (`R/loaders.R`) — the largest module. Reads manifest files that describe where track data lives, then dispatches to format-specific readers (`TrajSet_read_*`). The loader format registry (`register_loader_*`) lets callers add new file formats without modifying core code.

2. **Coordinate transformation** (`R/circular_trials.R`, `R/circular_mapping.R`) — converts Cartesian pixel coordinates to unit-circle angles. `circular_trials.R` extracts per-trial segments and normalises them to a unit arena; `circular_mapping.R` handles pixel→angle arithmetic. (radiatR analyses normalised, unit-arena tracks; correcting lens distortion or scaling to metric units is left to the upstream tracking pipeline.)

3. **Heading computation** (`R/headings.R`) — derives directional headings from position sequences. Uses a rule registry pattern (analogous to the loader registry) so custom heading rules can be registered and applied by name.

4. **Statistics** (`R/circular_statistics.R`) — thin wrapper around the `circular` package. `circ_summary` is the main entry point.

5. **Visualisation** (`R/circular_plotting.R`) — `radiate()` is the primary plot function; returns a `ggplot2` object. Helper functions (`add_ticks`, `add_circ`, `degree_labs`, etc.) return ggplot2 layers and are designed to be added with `+`.

### Object contract

Rich objects only for irreducible structure: `TrajSet` (trajectories + geometry
+ transform history), `circ_display` (orientation convention), `circ_regression`
(fitted model). Everything else is a plain tibble in and out, addressed by a
column-name argument. `circular`-package objects are internal only — never in a
public signature or return. `headings_frame` is a tibble subclass that carries
the display convention durably through dplyr verbs (`dplyr_reconstruct`); read it
via the `hf_*` accessors. A plain data frame with a heading column is always an
acceptable input. See `vignette("design")`.

### Extension points

- **New file formats**: use `register_loader_read_function` / `register_loader_info_function` (see `R/loaders.R`).
- **New heading rules**: use `register_heading_rule` (see `R/headings.R`).
- Both registries follow the same pattern: a named list stored in package env, dispatched at runtime.

### Shiny app (`inst/app/`)

The package is the single source of truth; the app is a thin presentation layer
over it. Every figure the app draws must be produced by exported package
functions, never by plotting logic that lives only in the app. The Results
figure is resolved into a plain spec (`plot_spec.R::build_plot_spec`) that both
renders to a ggplot (`spec_to_plot`, calling only exported functions) and emits
the equivalent radiatR script (`spec_to_code`). This is what lets the app hand
the user runnable ggplot2 code that reproduces exactly what is on screen. When
adding an app feature, push the real work into an exported package function and
keep the app code a light wrapper; if the app needs behaviour the package can't
yet express, extend the package first. A round-trip test asserts the emitted
code reproduces the rendered figure (comparing layer coordinates), so the two
paths cannot silently diverge.

### Documentation

All public API is documented with Roxygen2. `man/` is auto-generated — edit `@` tags in `R/` source, then run `devtools::document()`. Never edit `NAMESPACE` or `man/*.Rd` by hand.

### Tests

`tests/testthat/` has one file per module. `cpunctatus.rda` / `cpunctatus_tracks.rda` (in `data/`) are the bundled example datasets (a *Cylindroiulus punctatus* millipede visual-orientation experiment).


### shinytest2

- Prefer shiny::testServer over shinytest2 for app logic. It needs no browser, runs in seconds, and reliably exercises the
server reactives (it proved the column-mapping panel renders and the load wiring works in this very project, after shinytest2
hung on the same scenario). Reserve shinytest2 for genuinely DOM/browser-only behavior.
- Avoid app$upload_file entirely — browser file upload is the flakiest path; simulate uploads with session$setInputs(file = 
list(datapath=..., name=...)) under testServer instead.
- Each shinytest2 test costs an install + Chromium launch (minutes), and the runner does it twice per TDD task (red then
green) — expensive even when it doesn't hang.
- If you must use shinytest2, run it in the background or with a hard timeout, never inline, and check git state rather than
trusting a possibly-lost result.
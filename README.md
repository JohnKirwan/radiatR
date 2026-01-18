# radiatR

radiatR is an R package that provides a toolkit for working with movement trajectories with circular support. It is designed to import, summarise, analyze, and visualise large numbers of movement tracks—especially suited for behavioural experiments involving movement from or around a central origin, such as radial mazes or arena-based trials.

## Overview

This package comprises tools to import, summarise, and visualise movement trajectories with circular support. The aim is to have a convenient package to plot and explore many trajectories of angular data with ease and flexibility.

## Key Features

- Import and normalize tracking data from multi-trial experiments
- Extract circular trial boundaries and metadata from paired landmark/track files
- Transform Cartesian coordinates into circular coordinates (centering/scaling on a per-trial basis)
- Compute high-level circular statistics (mean direction, resultant length, concentration, etc.)
- Flexible visualization of tracks and summary statistics with high-quality ggplot2 layers
- Support for faceting and group overlays

## Core Modules

**1. Circular Trials (`R/circular_trials.R`)**
- Extract per-trial time bounds, arena origin, and stimulus angle from input data
- Transform and standardize position data for all trials and combine across multiple files
- Functions: `get_trial_limits`, `get_tracked_object_pos`, `get_all_object_pos`

**2. Circular Statistics (`R/circular_statistics.R`)**
- Compute summary statistics for angular data (leveraging the `circular` package)
- Provide group-wise or global summaries (means, concentration, resultant length, more)
- Main function: `circ_summary`

**3. Circular Plotting (`R/circular_plotting.R`)**
- Visualize trajectories and circular statistics using ggplot2
- Add concentric guides, direction ticks, mean resultant arrows, and custom radial overlays
- Main function: `radiate`

## Typical Workflow

1. **Import Data:** Use helper functions to load track and landmark files per trial or video.
2. **Extract and Transform:** Call `get_trial_limits` and `get_tracked_object_pos` to extract, normalize, and convert positions to circular form.
3. **Aggregate Across Files:** For large experiments, use `get_all_object_pos` to combine and standardize data from many sources.
4. **Summarize:** Calculate circular summary statistics (e.g., per trial, per group) using `circ_summary`.
5. **Visualize:** Create and customize high-quality circular plots of tracks and statistics using `radiate` and plot helpers from the package.

## Dependencies

radiatR depends on the following R packages (these will be installed automatically by R when you use the standard installation methods):

- ggplot2
- tibble
- purrr
- circular
- rlang
- methods
- utils
- stats

### Optional and Suggested Packages
- **ggrepel** (for improved label placement on plots; recommended but optional)
- **testthat**, **knitr**, **rmarkdown** (for testing and documentation)
- Additional suggested: arrow, data.table, jsonlite, readr, yaml, withr

To use all features (e.g., non-overlapping labels), install ggrepel:

```r
install.packages("ggrepel")
```

## Installation

### Install from Local Source
If you have downloaded or cloned this repository, you can install the package in R with:

```r
# In the repository directory:
devtools::install_local(path = ".")
# or
remotes::install_local(path = ".")
```

### Install from GitHub
If you want to install the latest version directly from GitHub:

```r
remotes::install_github("JohnKirwan/radiatR")
```

## Example

```r
library(radiatR)

# generate a reproducible demo data set
tracks <- simulate_tracks(n_trials = 3, n_points = 150, seed = 42)

# draw the tracks with concentric guides
radiate(tracks, x_col = "rel_x", y_col = "rel_y", group_col = "trial_id")
```

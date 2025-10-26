# radiatR

This package comprises tools to import, summarise, and visualise movement trajectories with
circular support. The aim is to have a convenient package to plot and explore many
trajectories of angular data with ease and flexibility. 


```r
library(radiatR)

# generate a reproducible demo data set
tracks <- simulate_tracks(n_trials = 3, n_points = 150, seed = 42)

# draw the tracks with concentric guides
radiate(tracks, x_col = "rel_x", y_col = "rel_y", group_col = "trial_id")
```

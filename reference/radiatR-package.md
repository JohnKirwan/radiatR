# radiatR: Analysis and Visualisation of Headings and Trajectories in Circular Space

A complete pipeline for analysing directional data – headings and
trajectories in circular space. Supply a table of angles directly, or
reconstruct headings from movement trajectories imported from 20+
tracking tools (EthoVision, DeepLabCut, SLEAP, TRex, ANY-maze,
TrackMate, idtracker.ai, Ctrax, and others) via a unified dialect-based
loader system, including multi-bodypart pose estimation with
likelihood-weighted centroid. Derives per-trial heading directions using
multiple rules (ring-crossing, distal point, pose body-axis,
velocity-based, and more). Computes circular statistics including mean
direction, resultant length, von Mises and wrapped Cauchy parametric
fits, circular correlation (circular-linear and circular-circular), and
multi-sample hypothesis tests (Watson-Williams, Rayleigh, and others)
with multiple-comparison correction. Renders publication-quality ggplot2
radial plots with overlaid rose diagrams, parametric density curves, and
non-parametric kernel density estimates. Includes a browser-based
graphical interface requiring no R coding via launch_app().

## American spellings

Every \`colour...\` argument and the \`assign_colour\_\*\` /
\`cycle_colours\` / \`hf_colour_col\` functions accept the American
\`color...\` spelling as an alias (e.g. \`color\`, \`color_col\`,
\`track_color\`). British spelling is canonical; supplying both
spellings of a pair is an error.

## See also

Useful links:

- <https://johnkirwan.github.io/radiatR/>

- <https://github.com/JohnKirwan/radiatR>

- Report bugs at <https://github.com/JohnKirwan/radiatR/issues>

## Author

**Maintainer**: John D. Kirwan <john@jkirwan.org>

Authors:

- John D. Kirwan <john@jkirwan.org>

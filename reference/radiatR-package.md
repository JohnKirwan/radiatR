# radiatR: Analyse and Visualise Headings and Trajectories on the Unit Disk

A complete pipeline for analysing directional data – headings and
trajectories on the unit disk. Supply a table of angles directly, or
reconstruct headings from movement trajectories imported from tracking
tools ('EthoVision', 'DeepLabCut', 'SLEAP', 'TRex', 'ANY-maze',
'TrackMate', 'idtracker.ai', and others) via a unified dialect-based
loader system, including multi-bodypart pose estimation with
likelihood-weighted centroid. Derives per-trial heading directions using
multiple rules (ring-crossing, distal point, pose body-axis,
velocity-based, and more). Computes circular statistics including mean
direction, resultant length, von Mises and wrapped Cauchy parametric
fits, circular correlation (circular-linear and circular-circular),
circular-linear regression (Fisher and Lee (1992)
[doi:10.2307/2532334](https://doi.org/10.2307/2532334) ), model
selection among orientation models (Fitak and Johnsen (2017)
[doi:10.1242/jeb.167056](https://doi.org/10.1242/jeb.167056) ), and
uniformity and multi-sample hypothesis tests (Rayleigh, Watson-Williams,
Hermans-Rasson (Landler, Ruxton and Malkemper (2019)
[doi:10.1186/s12898-019-0246-8](https://doi.org/10.1186/s12898-019-0246-8)
), and others) with multiple-comparison correction. Path tortuosity
follows Benhamou (2004)
[doi:10.1016/j.jtbi.2004.03.016](https://doi.org/10.1016/j.jtbi.2004.03.016)
. Renders publication-quality 'ggplot2' radial plots with overlaid rose
diagrams, parametric density curves, non-parametric kernel density
estimates, and circular boxplots (Buttarazzi, Pandolfo and Porzio (2018)
[doi:10.1111/biom.12889](https://doi.org/10.1111/biom.12889) ). Includes
a browser-based 'shiny' graphical interface requiring no R coding via
launch_app().

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

**Maintainer**: John D. Kirwan <john@jkirwan.org> \[copyright holder\]

Authors:

- John D. Kirwan <john@jkirwan.org> \[copyright holder\]

Other contributors:

- A. & M.W. Mathis Labs (test fixture
  tests/testthat/fixtures/real/dlc_CollectedData_Pranav.csv, from
  'DeepLabCut') \[copyright holder\]

- Talmo Pereira (example file inst/extdata/sleap_example.csv, from
  'SLEAP') \[copyright holder\]

- The Trustees of Princeton University (example file
  inst/extdata/sleap_example.csv, from 'SLEAP') \[copyright holder\]

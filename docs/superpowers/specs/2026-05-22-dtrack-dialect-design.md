# Design: dtrack loader dialect and provenance documentation

**Date:** 2026-05-22  
**Scope:** R/loaders.R · vignettes/loaders.Rmd · data-raw/P_lividus_example.R

---

## Background

The bundled example data (`inst/extdata/tracks/`) was tracked using **dtrack**
(https://bitbucket.org/jochensmolka/dtrack). The package already ships named
dialects for DeepLabCut, idtracker.ai, EthoVision, TrackMate, ToxTrac, and
BORIS, but has no `"dtrack"` entry. This design adds one and documents the
format provenance.

---

## Data provenance

**Paper:** Kirwan J.D., Li T., Ullrich-Lueter J., La Camera G., Nilsson D.-E.,
Arnone M.I. (2024). *The sea urchin Paracentrotus lividus orients to visual
stimuli.* bioRxiv. https://doi.org/10.1101/2024.01.05.574409

**Experiment:** 50 *P. lividus* adults placed individually at the centre of a
submerged cylindrical arena (~2 m diameter). Animals were allowed to walk
freely toward a patterned wall stimulus; tracks recorded from arena centre
outward. Five stimulus arc widths (0°, 15°, 30°, 45°, 60°, 150°) plus an
obstacle/no-obstacle condition.

**Tracking software:** dtrack (Smolka, Bitbucket). The experiment used two
dtrack markers per video:
- `_point01.txt` — 2 rows per trial: arena centre (x, y) and stimulus edge
  on wall (x, y). This landmark role is **project-specific**, not a general
  dtrack convention.
- `_point02.txt` — one row per frame: full xy trajectory of the animal.

---

## Section 1 — `dtrack` dialect (`R/loaders.R`)

Register at the bottom of `R/loaders.R` alongside existing dialects.

**Contract:**
- Input: file path (character) or data frame
- If a path, read with `read.delim(sep = "\t", header = FALSE)`
- Expect ≥ 3 columns; stop with a clear message if fewer
- Name columns `frame`, `x`, `y`; drop column 4 if present (dtrack
  confidence/flag, always `1` in practice)
- Return a plain `data.frame(frame, x, y)`

**No assumption** about whether the file is a landmark file or a trajectory
file — that distinction is the caller's responsibility.

---

## Section 2 — Roxygen update to `import_tracks()` (`R/loaders.R`)

Add a `@details` paragraph to the existing `import_tracks()` Roxygen block:

> The default suffixes (`_point01.txt`, `_point02.txt`) match the export
> naming convention used by dtrack
> (https://bitbucket.org/jochensmolka/dtrack). In the bundled *P. lividus*
> example data, `_point01` files contain two landmark rows (arena centre and
> stimulus edge on the wall) and `_point02` files contain the per-frame animal
> trajectory. This two-file role split is specific to that experiment and is
> not a general dtrack convention.

No functional change to the function.

---

## Section 3 — Loaders vignette (`vignettes/loaders.Rmd`)

Add a **"dtrack format"** section directly after the "Loading a Real
Experiment" section.

Contents:
- One sentence naming dtrack as the source software with URL
- Paper citation (Kirwan et al. 2024, doi)
- Show loading a single raw file with `TrajSet_read(path, format = "dtrack")`
  using `system.file("extdata", ...)` to locate the bundled file
- One short paragraph clarifying that `_point01`/`_point02` role split is
  experiment-specific

---

## Section 4 — Provenance comment (`data-raw/P_lividus_example.R`)

Replace the existing header comment with a full provenance block:

- Paper citation: Kirwan et al. 2024, doi:10.1101/2024.01.05.574409
- Tracking software: dtrack (https://bitbucket.org/jochensmolka/dtrack)
- Brief experimental description matching the abstract
- Link to full dataset: https://github.com/JohnKirwan/P_lividus_vision

---

## Out of scope

- A higher-level `dtrack_read_pair()` helper for the landmark/trajectory
  pairing — the convention is project-specific and does not generalise.
- Any change to the functional pipeline in `circular_trials.R`.

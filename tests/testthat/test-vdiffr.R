# Visual-regression snapshots (vdiffr) ----------------------------------------
#
# These doppelgangers guard radiate()/add_* RENDERING (theme, scales,
# coord_fixed, faceting, legend) against ggplot2 version drift -- complementing
# the per-layer coordinate assertions in test-circular-plotting.R /
# test-radiate-style.R.
#
# POLICY: skip on CI, compare locally. Every figure goes through expect_fig(),
# which skip_on_ci()s and skip_if_not_installed("vdiffr"). On CI all four skip,
# so R CMD check stays green across the ubuntu x {release, oldrel-1} matrix.
# The drift guard fires locally: run devtools::test(); vdiffr compares against
# the committed _snaps/vdiffr/*.svg (and version-gates its own comparison).
#
# UPDATING SNAPSHOTS: when a change INTENTIONALLY alters a figure, run
#   devtools::test(filter = "vdiffr")
# review the diffs with  testthat::snapshot_review("vdiffr")  and accept with
#   testthat::snapshot_accept("vdiffr")
# Requires vdiffr + svglite installed.
#
# All figures are built only from EXPORTED package functions on the bundled
# `cpunctatus` Tracks, with no RNG (analytic overlays only), so the SVGs are
# reproducible.
#
# DETERMINISM: label-bearing figures pass `label_use_repel = FALSE`. The default
# repel labeller (ggrepel::geom_text_repel) uses random force-directed placement
# with no fixed seed, so its output -- and its leader-line count -- jitters every
# render, which would make the snapshot compare non-deterministic. Plain
# geom_text (label_use_repel = FALSE) places labels analytically. Figures that
# hide labels (show_labels = FALSE) are unaffected.

expect_fig <- function(title, fig) {
  skip_on_ci()
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(title, fig)
}

test_that("radiate() baseline track plot renders", {
  fig <- radiate(cpunctatus, label_use_repel = FALSE)
  expect_fig("radiate-baseline", fig)
})

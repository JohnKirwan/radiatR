# Round-trip: the downloaded Summary & stats script must reproduce the numbers
# the app displays. We render the table via the app's circ_summary_table and
# execute the emitted script (extracting just the two test_uniformity lines,
# in the same order the app runs them), then compare every group's p-values.
.p <- system.file("app", "plot_spec.R", package = "radiatR")
if (!nzchar(.p) || !file.exists(.p))
  .p <- testthat::test_path("..", "..", "inst", "app", "plot_spec.R")
source(.p, local = TRUE)

roundtrip_spec <- function(by_col, omnibus, axial) {
  list(
    mode = "trajectory", data = list(source = "example"),
    headings = list(rule = "distal"),
    facet_by = NULL, group_col = "trial_id",
    show = list(vectors = FALSE), colour = list(by = "trajectory"),
    stats = list(by_col = by_col, pooled = is.null(by_col),
                 omnibus = omnibus, axial = axial,
                 n_groups = if (is.null(by_col)) NULL else 2L)
  )
}

test_that("emitted stats code reproduces the displayed uniformity p-values", {
  skip_if_not_installed("shiny")
  app_file <- system.file("app", "app.R", package = "radiatR")
  if (!nzchar(app_file))
    app_file <- testthat::test_path("..", "..", "inst", "app", "app.R")
  skip_if(!file.exists(app_file), "app.R not found")
  e <- new.env()
  suppressWarnings(suppressMessages(sys.source(app_file, envir = e, chdir = TRUE)))

  data(cpunctatus, package = "radiatR", envir = environment())
  ts <- cpunctatus
  hd <- derive_headings(ts, rule = "distal")
  df <- as.data.frame(ts)
  hd[["arc"]] <- df[["arc"]][match(hd$id, df[[ts@cols$id]])]

  grid <- expand.grid(by_col = c("arc"), omnibus = c("rao", "hermans_rasson"),
                      axial = c(FALSE, TRUE), stringsAsFactors = FALSE)

  saw_real_p <- FALSE   # guard against a vacuous all-"—" pass

  for (i in seq_len(nrow(grid))) {
    by_col  <- grid$by_col[i]; omnibus <- grid$omnibus[i]; axial <- grid$axial[i]

    ui <- e$circ_summary_table(hd, by_col, axial = axial, omnibus = omnibus)

    spec <- roundtrip_spec(by_col, omnibus, axial)
    code <- spec_to_stats_code(spec)
    lines <- strsplit(code, "\n")[[1]]

    # Extract just the test_uniformity + seed lines and eval them, in the exact
    # order the app runs them: Rayleigh is analytic (no seed needed); the seed
    # line must run immediately before the omnibus line so the RNG streams match.
    ray_line  <- grep('test = "rayleigh"', lines, value = TRUE)
    seed_line <- grep("^set.seed", lines, value = TRUE)
    omn_line  <- grep(sprintf('test = "%s"', omnibus), lines, value = TRUE)
    omn_line  <- grep("n_sim", omn_line, value = TRUE)  # exclude the rayleigh line if omnibus == "rao" false-matches

    env <- new.env(parent = globalenv()); env$hd <- hd; env$ts <- ts
    ray <- eval(parse(text = ray_line), env)
    eval(parse(text = seed_line), env)
    omn <- eval(parse(text = omn_line), env)

    ui_ray_col <- if (axial) "Rayleigh (axial) p" else "Rayleigh p"
    ui_omn_col <- if (identical(omnibus, "hermans_rasson")) "Hermans-Rasson p" else "Rao spacing"
    ui_ray <- ui[[ui_ray_col]]
    ui_omn <- ui[[ui_omn_col]]
    g   <- ui[["Group"]]
    fmt <- function(p) vapply(p, e$.fmt_p, character(1L))

    emitted_ray <- fmt(ray$p_value[match(g, ray[[by_col]])])
    emitted_omn <- fmt(omn$p_value[match(g, omn[[by_col]])])

    if (any(emitted_omn != "—")) saw_real_p <- TRUE

    expect_identical(ui_ray, emitted_ray,
                     info = sprintf("rayleigh/%s/axial=%s", omnibus, axial))
    expect_identical(ui_omn, emitted_omn,
                     info = sprintf("omnibus/%s/axial=%s", omnibus, axial))
  }

  # If this fails, every grid case degenerated to NA -> "—" and the equality
  # checks above would have passed vacuously without exercising real p-values.
  expect_true(saw_real_p,
             info = "expected at least one grid case with actual numeric p-values, not all em dashes")
})

# radiatR — Shiny companion app
# Launch with: radiatR::launch_app()
# Hosted on Posit Connect Cloud (git-backed): Connect Cloud rebuilds this app
# from the repo and installs dependencies from inst/app/manifest.json.

library(shiny)
library(bslib)

# The analysis/plot packages are attached lazily (see ensure_pkgs()), not here,
# so the upload screen paints on shiny + bslib alone. ggplot2 and radiatR (the
# latter pulling in circular -> boot, mvtnorm) attach the first time the user
# loads data. The library() calls below stay in the source so the dependencies
# are discoverable (e.g. by rsconnect::writeManifest); they only run when
# ensure_pkgs() is first called.
ensure_pkgs <- function() {
  if (!"radiatR" %in% .packages()) library(radiatR)
  if (!"ggplot2" %in% .packages()) library(ggplot2)
  invisible()
}

# Configure-step preview helpers (demo_tracks(), build_method_preview()).
# local = TRUE keeps the helpers in the app's environment: CRAN policy forbids
# writing to the user's global environment.
source("preview.R", local = TRUE)
source("preview_constructions.R", local = TRUE)
source("download_helpers.R", local = TRUE)
source("ui_helpers.R", local = TRUE)
source("plot_spec.R", local = TRUE)
source("upload.R", local = TRUE)

# ---- dialect registry --------------------------------------------------------

DIALECT_CHOICES <- c(
  "Auto-detect"                   = "auto",
  "EthoVision XT (Noldus)"        = "ethovision",
  "DeepLabCut"                    = "deeplabcut",
  "SLEAP"                         = "sleap",
  "TrackMate (Fiji)"              = "trackmate",
  "TRex"                          = "trex",
  "ANY-maze"                      = "anymaze",
  "idtracker.ai"                  = "idtrackerai_wide",
  "ToxTrac"                       = "toxtrac",
  "Generic CSV"                   = "generic"
)

# ---- helpers -----------------------------------------------------------------

guess_dialect <- function(path) {
  hdr <- tryCatch(
    readLines(path, n = 4L),
    error = function(e) character(0L)
  )
  # A binary file (an Excel workbook is a zip) or one in another encoding has
  # lines that are not valid text: they label nothing.
  hdr[!validUTF8(hdr)] <- ""
  # A UTF-8 byte-order mark before the first header is not part of it.
  bom <- rawToChar(as.raw(c(0xef, 0xbb, 0xbf)))
  if (length(hdr)) hdr[1L] <- sub(paste0("^", bom), "", hdr[1L], useBytes = TRUE)
  # DeepLabCut labels its header rows: scorer, (individuals,) bodyparts, coords
  lab <- tolower(gsub("^\"|\"$", "", trimws(sub(",.*$", "", hdr))))
  if (identical(lab[1:3], c("scorer", "bodyparts", "coords")) ||
      identical(lab, c("scorer", "individuals", "bodyparts", "coords"))) {
    return("deeplabcut")
  }
  if (!length(hdr) || !nzchar(hdr[1L])) return("generic")
  # An EthoVision text export says so on its first line, which is plain ASCII
  # even where the file is latin-1. (A UTF-16 export is not valid UTF-8 and is
  # chosen from the dropdown.)
  if (grepl("^\"?number of header lines", tolower(trimws(hdr[1L]))))
    return("ethovision")
  # Header cells, lower-cased and then reduced to [a-z0-9_]. Lower-casing must
  # come first: reducing first turned every capital into "_", so TRACK_ID and
  # "Pos. X (mm)" never matched. Only dialects checked against real exports are
  # detected.
  sep <- if (grepl("\t", hdr[1L], fixed = TRUE)) "\t"
         else if (grepl(";", hdr[1L], fixed = TRUE)) ";" else ","
  nm <- gsub("^_+|_+$", "",
             gsub("[^a-z0-9]+", "_", tolower(strsplit(hdr[1L], sep, fixed = TRUE)[[1L]])))

  # A flat <bodypart>_x / _y / _likelihood table is offered to the deeplabcut
  # dialect, which refuses it by name: read as plain columns it would load one
  # bodypart, placeholder rows included, without saying so.
  if (any(grepl("_likelihood$", nm)) && any(grepl("_x$", nm)))
    return("deeplabcut")
  if ("frame_idx" %in% nm && any(grepl("_score$", nm)))
    return("sleap")
  if ("track_id" %in% nm && "position_x" %in% nm)
    return("trackmate")
  if (all(c("centre_position_x", "centre_position_y") %in% nm) ||
      all(c("center_position_x", "center_position_y") %in% nm))
    return("anymaze")
  # idtracker.ai's wide table is nothing but x<id>/y<id> pairs and a time
  # column; a file with other columns that happens to have x1/y1 is not one.
  pair <- grepl("^[xy]_?[0-9]+$", nm)
  if (sum(pair) >= 2L && all(pair | nm %in% c("seconds", "frame", "frames", "time", "t")))
    return("idtrackerai_wide")
  if ("trackid" %in% nm || all(c("track", "pos_x_mm", "pos_y_mm") %in% nm))
    return("toxtrac")
  "generic"
}

# Low-cardinality categorical columns usable for *faceting* (the Facet-by
# selector): character/factor with 2-12 distinct values, excluding the structural
# id and time columns (faceting into many panels is unhelpful).
grouping_cols <- function(ts) {
  df   <- as.data.frame(ts)
  cats <- names(df)[vapply(df, function(v) {
    nu <- length(unique(stats::na.omit(v)))
    (is.character(v) || is.factor(v)) && nu >= 2L && nu <= 12L
  }, logical(1))]
  setdiff(cats, c(ts@cols$id, ts@cols$time))
}

# Categorical columns usable for *colouring* (the Colour-by selector): any
# character/factor column with >= 2 levels (no upper cap -- high-cardinality keys
# like an individual id are fine because colours cycle/cap), excluding time and
# the trajectory id (the latter is the default "Trajectory" option).
colour_cols <- function(ts) {
  df   <- as.data.frame(ts)
  cats <- names(df)[vapply(df, function(v) {
    nu <- length(unique(stats::na.omit(v)))
    (is.character(v) || is.factor(v)) && nu >= 2L
  }, logical(1))]
  setdiff(cats, c(ts@cols$id, ts@cols$time))
}

# Categorical columns of a headings data frame usable for faceting/grouping (the
# headings analogue of grouping_cols(): char/factor with 2-12 levels, excluding
# the id and heading columns). Used to populate the headings "Facet by" selector.
.hd_group_choices <- function(hd) {
  if (is.null(hd)) return(character(0))
  hc   <- attr(hd, "heading_col") %||% "heading"
  cats <- names(hd)[vapply(hd, function(v) {
    nu <- length(unique(stats::na.omit(v)))
    (is.character(v) || is.factor(v)) && nu >= 2L && nu <= 12L
  }, logical(1))]
  setdiff(cats, c("id", hc))
}

# Identify a candidate condition column in a loaded Tracks.
# Looks for character columns with 2-12 unique values that are not
# structural (id, time, position, derivatives).
detect_cond_col <- function(ts) {
  df   <- as.data.frame(ts)
  skip <- unique(c(
    ts@cols$id, ts@cols$time,
    ts@cols$x,  ts@cols$y,
    ts@cols$rel_x, ts@cols$rel_y,
    grep(
      "^(rho|angle|radius|raw_|x_raw|y_raw|rel_|trans_|dist)",
      names(df), value = TRUE
    )
  ))
  cands <- setdiff(names(df), c(skip, NA_character_))
  hits <- Filter(function(col) {
    v  <- df[[col]]
    nu <- length(unique(stats::na.omit(v)))
    (is.character(v) || is.factor(v)) &&
      nu >= 2L && nu <= 12L && nu < nrow(df)
  }, cands)
  if (length(hits)) hits[1L] else NULL
}

# normalize_xy is a load-time choice surfaced by the Coordinates radio in step 1
# (output$normalize_box); the resolved logical is recorded on rv$normalize_xy.
load_ts <- function(path, dialect, mapping = list(), read_opts = list(delim = NULL),
                    normalize_xy = TRUE, origin = NULL, radius = NULL,
                    on_invalid = "error", dialect_args = list()) {
  if (is.null(dialect) || dialect %in% c("auto", "generic"))
    return(read_tracks(path, mapping = mapping, read_opts = read_opts,
                       normalize_xy = normalize_xy, origin = origin, radius = radius,
                       on_invalid = on_invalid))
  read_tracks(path, dialect = dialect, dialect_args = dialect_args,
              mapping = mapping, read_opts = read_opts,
              normalize_xy = normalize_xy, origin = origin, radius = radius,
              on_invalid = on_invalid)
}

# The rows a failed load could leave out if the user agrees: "positions" for
# rows with a missing or non-finite x/y (read_tracks(on_invalid = "drop")),
# "unlinked" for TrackMate spots in no track (unlinked = "drop"). NULL for any
# other error: a malformed value or a broken track is never dropped.
drop_offer <- function(e) {
  if (inherits(e, "radiatR_unlinked_rows")) return("unlinked")
  if (inherits(e, "radiatR_invalid_rows") && !inherits(e, "radiatR_malformed_values"))
    return("positions")
  NULL
}

# The bundled Cylindroiulus punctatus millipede example, as a Tracks, so
# users can try the app without supplying their own tracking file.
# The test suite sets options(radiatR.app_example_per_arc = k) to load only the
# first k tracks of each `arc` level, which keeps every `arc` and `type` level
# so grouping and faceting still work. Unset (the default) loads all tracks.
example_ts <- function() {
  k <- getOption("radiatR.app_example_per_arc")
  if (!is.null(k) &&
      (!is.numeric(k) || length(k) != 1L || is.na(k) || k < 1 || k != round(k)))
    stop("option radiatR.app_example_per_arc must be a single positive whole ",
         "number, not ", paste(deparse(k), collapse = " "), call. = FALSE)
  e <- new.env()
  utils::data("cpunctatus", package = "radiatR", envir = e)
  ts <- e$cpunctatus
  if (is.null(k)) return(ts)
  d <- unique(as.data.frame(ts)[, c(ts@cols$id, "arc")])
  ts[as.character(d[[1L]])[stats::ave(seq_len(nrow(d)), d$arc, FUN = seq_along) <= k]]
}

# Heading methods that are inherently axial (per-track movement axis, not a
# single direction). Selecting one soft-syncs Directionality to "axial".
# Extend here when the modality piece adds more axial methods.
AXIAL_METHODS <- c("velocity_axis", "pca_axis", "ransac_straight")

# Package website (pkgdown). Used by the header brand + Documentation link.
SITE_URL <- "https://johnkirwan.github.io/radiatR/"

# A compact inline on/off toggle for a results-plot layer. Wraps a standard
# Shiny checkbox (so it registers as input[[id]]) and styles it as a switch.
.layer_switch <- function(id, label, value) {
  div(
    class = "form-switch small",
    checkboxInput(id, label, value = value, width = "auto")
  )
}

# Sentinel "Colour by" value meaning "colour each trajectory" (the default),
# as opposed to a real grouping column name. Must match SPEC_TRAJ_KEY in
# plot_spec.R (build_plot_spec reads input$colour_by against it).
TRAJ_COLOUR_KEY <- "__trajectory__"

# A finite scalar from a possibly-NULL/NA numeric input, else a default.
num_or <- function(v, default) {
  if (is.null(v) || length(v) != 1L || is.na(v) || !is.finite(v)) default else v
}

# Per-rule one-line descriptions for the Configure-step help line, keyed by the
# selectInput value. "none" is an app-level sentinel (skip headings), not a
# derive_headings() rule.
method_help_text <- c(
  none            = "Plot the tracks and path metrics only - no headings or circular statistics.",
  distal          = "Heading when the subject was furthest from the centre. No setup needed.",
  net             = "Straight-line direction from start to end. Simple and always applicable.",
  crossing        = "Heading as the subject crosses a detection ring. Set the ring radii below.",
  straight        = "Direction of the longest near-straight run in the path.",
  window_net      = "Net direction over a sliding window, smoothing local wobble.",
  origin_mean     = "Mean of the directions from the centre to each point.",
  velocity_mean   = "Mean of the step (velocity) directions along the path.",
  velocity_axis   = "Axial mean of step directions - the back-and-forth movement axis per track. Set Directionality to Axial for correct circular statistics.",
  maxspeed_window = "Direction over the window of the subject's fastest movement.",
  vm_fit          = "Von Mises fit to the step directions (peak of the fitted distribution).",
  pca_axis        = "Principal (long) axis of the visited positions.",
  ransac_straight = "Robust straight-line fit that ignores outlying points (RANSAC).",
  goal_bias       = "Net step direction weighted toward outward (away-from-centre) movement."
)

# One-line path-metrics caption for the no-headings ("none") mode. Reads the
# same exported metric functions the metrics table uses so numbers match. Extend
# by appending more "label/values" clauses as metrics are added.
straightness_caption <- function(ts, gc = NULL) {
  idc <- ts@cols$id
  st  <- straightness_index(ts)
  sn  <- sinuosity(ts)
  if (is.null(gc)) {
    ms <- mean(st$straightness, na.rm = TRUE)
    if (!is.finite(ms)) return("")
    msin <- mean(sn$sinuosity, na.rm = TRUE)
    out  <- sprintf("Mean straightness: %.2f", ms)
    if (is.finite(msin)) out <- paste0(out, sprintf(" | Mean sinuosity: %.2f", msin))
    out
  } else {
    cond_map <- unique(as.data.frame(ts)[, c(idc, gc), drop = FALSE])
    clause <- function(tbl, val, label) {
      tbl <- merge(tbl, cond_map, by = idc)
      agg <- tapply(tbl[[val]], as.character(tbl[[gc]]),
                    function(v) mean(v, na.rm = TRUE))
      paste0(label, " - ",
             paste(sprintf("%s: %.2f", names(agg), as.numeric(agg)),
                   collapse = ", "))
    }
    paste(clause(st, "straightness", "Straightness"),
          clause(sn, "sinuosity",    "Sinuosity"), sep = " | ")
  }
}

# Combined per-track path-metrics table for the no-headings ("none") CSV: id +
# length + straightness + tortuosity + sinuosity, each from its exported package
# function, joined on the id column.
path_metrics_table <- function(ts) {
  idc <- ts@cols$id
  Reduce(function(a, b) merge(a, b, by = idc),
         list(track_length(ts), straightness_index(ts),
              tortuosity_ratio(ts), sinuosity(ts)))
}

derive_hd <- function(ts, method, circ0, circ1, coords = "relative") {
  # Use the requested frame; relative aligns headings with the rel_x/rel_y display
  # (stimulus fixed at East).
  args <- list(x = ts, coords = .resolve_heading_coords(ts, coords))
  # The app surfaces attrition via the Results banner, not the R console; quiet
  # still attaches the n_total/n_missing/missing_ids attributes the banner reads.
  args$on_missing <- "quiet"
  if (method == "crossing") {
    args$rule          <- "crossing"
    args$circ0         <- circ0
    args$circ1         <- circ1
    args$return_coords <- TRUE
  } else {
    args$rule <- method
  }
  with_local_seed(APP_SEED, do.call(derive_headings, args))   # RANSAC rule draws
}

# Fixed Monte-Carlo policy for the Summary & stats uniformity tests. Each grouped
# MC call runs under the spec seed (APP_SEED, plot_spec.R) so the UI and the
# downloaded script (spec_to_stats_code) share the exact RNG stream and produce
# identical p-values.
STATS_MC_NSIM <- 999L

# numeric uniformity p-value -> display string. NA / absent -> em dash.
.fmt_p <- function(p) {
  if (length(p) != 1L || is.na(p)) return("—")
  if (p < 0.001) return("< 0.001")
  sprintf("%.3f", p)
}

# Display-ready circular summary (n, mean direction, resultant R, Rayleigh p) for
# a headings frame, grouped by `by_col`. Returns a data frame with the group
# column renamed to "Group"; callers may append trajectory-only columns
# (e.g. Straightness) keyed on the returned "Group" values.
circ_summary_table <- function(hd, by_col, axial = FALSE,
                               omnibus = "rao", model_sel = NULL,
                               seed = APP_SEED) {
  cm <- circ_summarise(
    hd, "heading", units = "radians", .by = by_col,
    stats = c("n", "n_missing", "mean_dir_deg", "resultant_R"),
    display = circ_display(zero = 0), axial = axial
  )
  # Per-group Rayleigh + omnibus via the package function so the downloaded
  # code (spec_to_stats_code) reproduces these numbers exactly. Omnibus runs on
  # raw angles (axial = FALSE); Rayleigh honours the axial toggle.
  groups <- unique(hd[[by_col]])
  ray <- test_uniformity(hd, group_col = by_col, test = "rayleigh", axial = axial)
  omn <- with_local_seed(seed,
    test_uniformity(hd, group_col = by_col, test = omnibus,
                    n_sim = STATS_MC_NSIM, axial = FALSE,
                    p_method = "monte_carlo"))
  p_vals   <- vapply(ray$p_value[match(groups, ray[[by_col]])], .fmt_p, character(1L))
  omni_vals <- vapply(omn$p_value[match(groups, omn[[by_col]])], .fmt_p, character(1L))
  omni_label <- if (identical(omnibus, "hermans_rasson")) "Hermans-Rasson p" else "Rao spacing"
  rayleigh_label <- if (isTRUE(axial)) "Rayleigh (axial) p" else "Rayleigh p"
  p_df <- stats::setNames(
    data.frame(groups, p_vals, omni_vals, stringsAsFactors = FALSE),
    c(by_col, rayleigh_label, omni_label))

  if (!is.null(model_sel) && nrow(model_sel) > 0L && by_col %in% names(model_sel)) {
    parts <- lapply(split(model_sel, model_sel[[by_col]]), function(x) {
      best <- x[1L, ]                              # circ_model_select sorts best-first
      data.frame(g = best[[by_col]],
                 bm = sprintf("%s (%.2f)", best$model, best$weight),
                 stringsAsFactors = FALSE)
    })
    bm <- do.call(rbind, parts)
    bm <- stats::setNames(bm, c(by_col, "Best model"))
    p_df <- merge(p_df, bm, by = by_col, sort = FALSE)
  }

  cm <- merge(cm, p_df, by = by_col, sort = FALSE)
  names(cm)[names(cm) == by_col]         <- "Group"
  names(cm)[names(cm) == "n_missing"]    <- "Excluded"
  names(cm)[names(cm) == "mean_dir_deg"] <- "Direction (°)"
  names(cm)[names(cm) == "resultant_R"]  <- "R"
  cm[["Direction (°)"]] <- round(cm[["Direction (°)"]], 1)
  cm[["R"]]             <- round(cm[["R"]], 3)
  cm
}

# Between-group comparison: mean direction, concentration, and whole-
# distribution tests, one fixed row each (no method picker -- see the
# 2026-07-10 group-comparison-stats design). Each call is run through
# run_group_test(): a genuinely non-estimable input (radiatR_nonestimable,
# e.g. too few observations in a group) degrades to a dash row that states
# the reason; any other error is a bug and propagates to the caller.
group_compare_table <- function(hd, by_col, axial = FALSE) {
  fmt_stat <- function(x) if (length(x) && is.finite(x)) sprintf("%.2f", x) else "—"
  fmt_df   <- function(x) if (length(x) && !is.na(x)) as.character(x) else "—"
  fmt_p    <- function(p) {
    if (!length(p) || is.na(p)) return("—")
    if (p < 0.001) return("< 0.001")
    sprintf("%.3f", p)
  }
  # Run one between-group test. A radiatR_nonestimable condition -> a dash
  # row whose Method column states the reason. Any other error is a bug and
  # is NOT caught here, so it propagates to the caller (re-raise).
  run_group_test <- function(expr) {
    tryCatch(
      list(ok = TRUE, value = expr),
      radiatR_nonestimable = function(e)
        list(ok = FALSE, reason = e$reason %||% "not estimable")
    )
  }

  dash_row_reason <- function(test_label, reason)
    data.frame(Test = test_label, Method = reason, Statistic = "—", df = "—",
               `p-value` = "—", check.names = FALSE, stringsAsFactors = FALSE)

  n_groups <- length(unique(stats::na.omit(hd[[by_col]])))

  res_mean <- run_group_test(test_mean_directions(hd, by_col, axial = axial))
  row_mean <- if (!res_mean$ok)
    dash_row_reason("Mean direction", res_mean$reason) else {
      r_mean <- res_mean$value
      data.frame(Test = "Mean direction", Method = r_mean$test[1],
                 Statistic = fmt_stat(r_mean$statistic[1]),
                 df = paste(r_mean$df1[1], r_mean$df2[1], sep = ", "),
                 `p-value` = fmt_p(r_mean$p_value[1]),
                 check.names = FALSE, stringsAsFactors = FALSE)
    }

  res_conc <- run_group_test(test_concentration(hd, by_col, axial = axial))
  row_conc <- if (!res_conc$ok)
    dash_row_reason("Concentration", res_conc$reason) else {
      r_conc <- res_conc$value
      data.frame(Test = "Concentration", Method = r_conc$test[1],
                 Statistic = fmt_stat(r_conc$statistic[1]),
                 df = fmt_df(r_conc$df[1]),
                 `p-value` = fmt_p(r_conc$p_value[1]),
                 check.names = FALSE, stringsAsFactors = FALSE)
    }

  dist_method <- if (identical(n_groups, 2L)) "watson_two" else "watson_wheeler"
  res_dist <- run_group_test(
    test_distributions(hd, by_col, axial = axial, method = dist_method))
  row_dist <- if (!res_dist$ok)
    dash_row_reason("Distribution", res_dist$reason) else {
      r_dist <- res_dist$value
      data.frame(Test = "Distribution", Method = r_dist$method[1],
                 Statistic = fmt_stat(r_dist$statistic[1]),
                 df = fmt_df(r_dist$df[1]),
                 `p-value` = fmt_p(r_dist$p_value[1]),
                 check.names = FALSE, stringsAsFactors = FALSE)
    }

  rbind(row_mean, row_conc, row_dist)
}

# Advice naming the Decimal mark control, when the package suggests the other
# mark (it suggests none for Excel, where the control is hidden).
decimal_advice <- function(e) {
  s <- e$suggested_decimal
  if (is.null(s)) return("")
  if (identical(s, ","))
    " If the file's decimal mark is a comma, set Decimal mark to Comma (,)."
  else " If the file's decimal mark is a point, set Decimal mark to Point (.)."
}

# The app control behind a read_opts entry a dialect refused.
read_opt_controls <- c(delim = "Delimiter", decimal = "Decimal mark")

plain_error <- function(e) {
  if (inherits(e, "radiatR_invalid_tracks") && "time_duplicate" %in% e$problems$rule)
    return(paste(
      "Some rows share the same ID and time.",
      "Check that the ID column identifies single tracks; see Technical details."
    ))
  if (inherits(e, "radiatR_malformed_values"))
    return(paste0(sprintf(
      "Column '%s' has values that are not %s, such as '%s'. See Technical details for the rows.",
      e$columns, e$expected, e$values[1]
    ), decimal_advice(e)))
  if (inherits(e, "radiatR_ambiguous_decimal")) {
    if (length(e$columns) == 0L)
      return(paste("The decimal mark can't be told from the file's first lines.",
                   "Set Decimal mark to Point (.) or Comma (,)."))
    return(sprintf(paste(
      "Can't tell the decimal mark of column '%s' (values like '%s': a decimal",
      "point, or digit grouping?). If the file's decimal mark is a point, set",
      "Decimal mark to Point (.). Digit grouping is not supported."),
      e$columns[1L], e$values[1L]))
  }
  if (inherits(e, "radiatR_unlinked_rows"))
    return(sprintf(paste(
      "This TrackMate file contains %d spot(s) that belong to no track.",
      "Export \"Spots in tracks\" from TrackMate and upload that file instead,",
      "or drop those spots to continue."
    ), length(e$rows)))
  if (identical(drop_offer(e), "positions"))
    return(sprintf(paste(
      "%d row(s) of this file have no x/y position.",
      "Drop them to continue, or see Technical details for the rows and ids affected."
    ), length(e$rows)))
  if (inherits(e, c("radiatR_invalid_rows", "radiatR_invalid_tracks")))
    return(paste(
      "Some rows of this file are not valid track data.",
      "See Technical details for the rows and ids affected."
    ))
  # A read_opts entry a dialect refused: tell the user which control to reset.
  if (identical(e$arg, "read_opts") && !is.null(e$refused_by) && !is.null(e$option)) {
    if (e$option %in% names(read_opt_controls)) {
      lab <- read_opt_controls[[e$option]]
      return(sprintf("This file's format sets its own %s: set %s to Auto-detect.",
                     tolower(lab), lab))
    }
    if (e$option %in% c("sheet", "ext"))
      return(paste("This software's files are read from its text export, not from an",
                   "Excel workbook: upload the .txt or .csv export."))
  }
  # A dialect says what is wrong with the file in its own words.
  if (inherits(e, "radiatR_dialect_error")) return(conditionMessage(e))
  # So does the reader, for a file that is not one table or ids that differ
  # only by whitespace (these name columns, which the guesses below misread).
  if (inherits(e, c("radiatR_malformed_file", "radiatR_ambiguous_ids",
                    "radiatR_invalid_argument")))
    return(sub("^read_tracks: ", "", conditionMessage(e)))
  m <- conditionMessage(e)
  if (grepl("id|time|mapping", m, ignore.case = TRUE))
    return(paste(
      "Could not identify the trial ID or time columns.",
      "Try selecting your software from the dropdown."
    ))
  if (grepl("x.*y|position|col", m, ignore.case = TRUE))
    return(paste(
      "Could not find x/y position columns.",
      "Try selecting your software from the dropdown."
    ))
  if (grepl("crossing|circ0|circ1", m, ignore.case = TRUE))
    return(paste(
      "No ring crossings found.",
      "Try adjusting the ring radii or choosing a different method."
    ))
  paste("Unexpected error:", m)
}

# The full technical detail behind a friendly ingest error: the original
# exception message, plus row numbers / ids when the loader raised a
# structured radiatR_invalid_rows or radiatR_invalid_tracks condition (both
# carry `rows` and `ids`). A radiatR_malformed_values or radiatR_unlinked_rows
# message already lists its rows and values, and has no ids.
ingest_detail <- function(e) {
  detail <- conditionMessage(e)
  if (inherits(e, c("radiatR_invalid_rows", "radiatR_invalid_tracks")) &&
      !inherits(e, c("radiatR_malformed_values", "radiatR_unlinked_rows"))) {
    detail <- paste0(
      detail,
      "\n\nAffected rows (", length(e$rows), "): ",
      paste(utils::head(e$rows, 20L), collapse = ", "),
      if (length(e$rows) > 20L) ", ...",
      "\nAffected ids: ", paste(utils::head(unique(e$ids), 20L), collapse = ", ")
    )
  }
  detail
}

# ---- ui ----------------------------------------------------------------------

ui <- page_fillable(
  theme   = bs_theme(bootswatch = "flatly"),
  padding = "1.5rem",

  tags$head(
    tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "favicon-32x32.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "favicon-16x16.png"),
    tags$link(rel = "apple-touch-icon", href = "apple-touch-icon.png"),
    tags$style(HTML("
    .pill       { display:inline-block; padding:3px 14px;
                  border-radius:99px; font-size:.78rem;
                  font-weight:600; margin-right:3px; }
    .pill-done  { background:#d4edda; color:#1a5c2a; }
    .pill-here  { background:#2780e3; color:#fff; }
    .pill-later { background:#e9ecef; color:#6c757d; }
    .main-body  { min-height:440px; }
  "))),

  card(
    card_header(
      class = "d-flex justify-content-between align-items-center",
      ui_brand(SITE_URL),
      uiOutput("step_pills")
    ),
    card_body(class = "main-body", uiOutput("wizard")),
    card_footer(
      class = "d-flex justify-content-between align-items-center",
      actionButton(
        "back_btn", "Back",
        class = "btn-sm btn-outline-secondary"
      ),
      tags$span(class = "text-muted small",
                paste0("v", utils::packageVersion("radiatR"))),
      uiOutput("fwd_btn_ui")
    )
  )
)

# ---- server ------------------------------------------------------------------

server <- function(input, output, session) {

  rv <- reactiveValues(
    step      = 1L,
    path      = NULL,
    source    = NULL,
    file_name = NULL,
    dialect   = NULL,
    ts        = NULL,
    cond_col  = NULL,
    hd        = NULL,
    method    = NULL,
    error     = NULL,
    error_detail = NULL,
    mode      = "trajectories",
    raw_hd    = NULL,
    hd_map    = NULL,   # list(col, units, convention, group) for the headings spec
    # Row drops (see drop_offer()). `drop_offer`: list(kind, n, error, drop)
    # while the load error shown is one a drop would clear; `drop` holds the
    # kinds that load already ran with. `drop_used` / `dropped`: the kinds
    # applied to, and the rows left out of, the loaded rv$ts (meaningful only
    # for an uploaded file; recorded at load time, like normalize_xy).
    drop_offer = NULL,
    drop_used  = character(0),
    dropped    = 0L
  )

  # Single source of truth for axial mode, resolved from the Directionality control.
  is_axial <- reactive(identical(input$data_model, "axial"))

  # Soft default: choosing an inherently axial heading method pre-selects the
  # Axial data model. Overridable -- the user may switch back afterwards, and a
  # directional method does not force it back.
  observeEvent(input$method, {
    if (!is.null(input$method) && input$method %in% AXIAL_METHODS)
      updateRadioButtons(session, "data_model", selected = "axial")
  }, ignoreInit = TRUE)

  # ---- step pills ------------------------------------------------------------
  output$step_pills <- renderUI({
    labs <- c("1. Upload", "2. Configure", "3. Results")
    tags$span(lapply(seq_along(labs), function(i) {
      cls <- if (i < rv$step) "pill pill-done"
             else if (i == rv$step) "pill pill-here"
             else "pill pill-later"
      tags$span(labs[i], class = cls)
    }))
  })

  # ---- forward button --------------------------------------------------------
  output$fwd_btn_ui <- renderUI({
    if (rv$step == 1L)
      actionButton("go2", "Next", class = "btn-primary")
    else if (rv$step == 2L)
      actionButton("go3", "Analyse", class = "btn-success btn-lg")
    else
      actionButton("restart", "Start over",
                   class = "btn-outline-secondary")
  })

  # ---- navigation ------------------------------------------------------------
  observeEvent(input$back_btn, {
    if (rv$step > 1L) {
      rv$step  <- rv$step - 1L
      rv$error <- NULL
      rv$error_detail <- NULL
    }
  })

  observeEvent(input$input_type, {
    # Also fires once on initial load (harmless: fields are already NULL).
    rv$mode   <- input$input_type
    rv$ts     <- NULL
    rv$hd     <- NULL
    rv$raw_hd <- NULL
    rv$hd_map <- NULL
    rv$error  <- NULL
    rv$error_detail <- NULL
  })

  # The drop offer lasts only as long as the error it came from.
  observeEvent(rv$error, {
    if (!identical(rv$error, rv$drop_offer$error)) rv$drop_offer <- NULL
  }, ignoreNULL = FALSE)

  # Agreeing to a drop re-runs the load that was refused, with that drop added
  # to the ones it already ran with. The agreement is not kept: "Next" always
  # loads with no drops, so a changed file, format or column is asked again.
  observeEvent(input$drop_btn, {
    req(rv$drop_offer)
    load_upload(drop = union(rv$drop_offer$drop, rv$drop_offer$kind))
  })

  observeEvent(input$restart, {
    rv$step     <- 1L
    rv$path     <- NULL
    rv$dialect  <- NULL
    rv$ts       <- NULL
    rv$cond_col <- NULL
    rv$hd       <- NULL
    rv$error    <- NULL
    rv$error_detail <- NULL
    rv$mode     <- "trajectories"
    rv$raw_hd   <- NULL
    rv$hd_map   <- NULL
    updateRadioButtons(session, "input_type", selected = "trajectories")
  })

  # Step 1 → 2: load Tracks and detect condition column
  observeEvent(input$go2, {
    if (identical(rv$mode, "headings")) {
      if (is.null(rv$raw_hd)) {
        # Example headings are already loaded (no file to map) -> straight to results.
        if (identical(rv$source, "example") && !is.null(rv$hd)) {
          rv$step <- 3L; rv$error <- NULL; rv$error_detail <- NULL; return()
        }
        rv$error <- "Please upload a file of headings first."
        rv$error_detail <- NULL
        return()
      }
      if (!length(hd_angle_choices(rv$raw_hd)$choices)) {
        rv$error <- "No numeric angle column found; headings input needs a column of angles."
        rv$error_detail <- NULL
        return()
      }
      rv$step <- 2L; rv$error <- NULL; rv$error_detail <- NULL
      return()
    }
    if (is.null(rv$path)) {
      # An example dataset may already be loaded without a file path.
      if (!is.null(rv$ts)) {
        rv$step  <- 2L
        rv$error <- NULL
        rv$error_detail <- NULL
        return()
      }
      rv$error <- "Please upload a file first."
      rv$error_detail <- NULL
      return()
    }
    load_upload()
  })

  # Load the uploaded tracking file with the current Step 1 choices, leaving out
  # the kinds of row in `drop` (see drop_offer()).
  load_upload <- function(drop = character(0)) {
    d <- if (!is.null(input$dialect_sel) &&
             input$dialect_sel != "auto") {
      input$dialect_sel
    } else {
      rv$dialect
    }
    ensure_pkgs()   # first point that needs radiatR/ggplot2 for a file upload
    map <- if (identical(d, "generic")) {
      pick <- function(v) if (!is.null(v) && nzchar(v)) v else NULL
      list(x = pick(input$map_x), y = pick(input$map_y),
           time = pick(input$map_time), id = pick(input$map_id))
    } else list()
    # radioButtons yields a character; resolve to a logical exactly once here.
    # NULL means the control never rendered -- fall back to the TRUE default.
    # "calibrate" is a third mode: normalize_xy is off and a fixed origin/radius
    # is read from the conditional inputs. Gating origin/radius on the mode (not
    # merely their presence) keeps stale typed values from leaking into a
    # raw/normalize load.
    calibrate <- identical(input$normalize_sel, "calibrate")
    nrm <- if (is.null(input$normalize_sel)) TRUE
           else identical(input$normalize_sel, "TRUE")
    # Coerce blanks to NA (never let c() drop a NULL to a shorter vector): an
    # unfilled reference must reach the package as a non-finite value so its
    # finite-check errors, rather than collapsing to a silent uncalibrated load.
    origin <- if (calibrate)
      c(input$origin_x %||% NA_real_, input$origin_y %||% NA_real_) else NULL
    radius <- if (calibrate) input$radius %||% NA_real_ else NULL
    captured_warn <- character(0)
    dropped <- 0L
    ts <- withCallingHandlers(
      tryCatch(
        load_ts(rv$path, d, mapping = map,
                read_opts = c(upload_read_opts(rv$file_name, input$delim_sel,
                                               input$decimal_sel, input$sheet_sel),
                              if (is.null(d) || d %in% c("auto", "generic"))
                                list(ext = tolower(tools::file_ext(rv$file_name %||% "")))),
                normalize_xy = nrm, origin = origin, radius = radius,
                on_invalid = if ("positions" %in% drop) "drop" else "error",
                dialect_args = if ("unlinked" %in% drop) list(unlinked = "drop") else list()),
        error = function(e) {
          rv$error        <- plain_error(e)
          rv$error_detail <- ingest_detail(e)
          kind <- drop_offer(e)
          rv$drop_offer <- if (!is.null(kind))
            list(kind = kind, n = length(e$rows), error = rv$error, drop = drop)
          NULL
        }
      ),
      warning = function(w) {
        captured_warn <<- c(captured_warn, conditionMessage(w))
        if (inherits(w, "radiatR_dropped_rows")) dropped <<- dropped + length(w$rows)
        invokeRestart("muffleWarning")
      }
    )
    if (!is.null(ts)) {
      rv$ts            <- ts
      rv$dialect       <- d
      rv$drop_used     <- drop
      rv$dropped       <- dropped
      rv$normalize_xy  <- nrm
      rv$origin        <- origin
      rv$radius        <- radius
      rv$cond_col      <- detect_cond_col(ts)
      rv$step          <- 2L
      rv$error         <- NULL
      rv$error_detail  <- if (length(captured_warn))
                            paste(captured_warn, collapse = "\n") else NULL
    }
  }

  # One-line description of the selected heading method, shown under the dropdown.
  output$method_help <- renderUI({
    m <- if (is.null(input$method)) "distal" else input$method
    txt <- method_help_text[[m]]
    if (is.null(txt)) txt <- ""
    tags$p(class = "text-muted small", txt)
  })

  # Contextual note when the heading method and Directionality are mismatched.
  # Hint (grey) for a directional method under the Axial model (directions are
  # folded to axes -- valid but maybe unintended). Warning (amber) for an axial
  # method under the Directional model (a per-track axis analysed as a direction
  # biases the circular statistics). Nothing for matched combos / "none".
  output$method_model_note <- renderUI({
    m <- input$method
    if (is.null(m) || identical(m, "none")) return(NULL)
    axial_method <- m %in% AXIAL_METHODS
    if (is_axial() && !axial_method) {
      tags$div(class = "alert alert-secondary py-1 px-2 small mb-2",
        "Axial model with a directional method: each track's direction is folded ",
        "to an axis (mod-180°). For a per-track movement axis, pick an Axial method.")
    } else if (!is_axial() && axial_method) {
      tags$div(class = "alert alert-warning py-1 px-2 small mb-2",
        "This method returns a per-track axis (0–180°); analysing it as ",
        "Directional biases the circular statistics — set Directionality to Axial.")
    } else {
      NULL
    }
  })

  output$frame_ui <- renderUI({
    req(rv$ts)
    has_rel <- !is.null(rv$ts@cols$rel_x) && !is.null(rv$ts@cols$rel_y)
    rb <- radioButtons(
      "frame", "Heading frame",
      choices  = c("Relative to landmark" = "relative",
                   "Absolute (control)"   = "absolute"),
      selected = isolate(input$frame) %||% "relative")
    if (has_rel) rb else tagList(
      tags$div(rb, style = "opacity:0.5; pointer-events:none;"),
      tags$p(class = "text-muted small",
             "No landmark frame in this dataset — showing absolute coordinates."))
  })

  # When the circular-boxplot overlay is on but the current headings are not
  # drawable (non-unique median or n < 4) the overlay silently no-ops; surface
  # the reason so the empty result is not mysterious.
  output$boxplot_note <- renderUI({
    if (!isTRUE(input$show_boxplot) || is.null(rv$hd)) return(NULL)
    notes <- list()
    boxplot_err <- NULL
    s <- tryCatch(circ_boxplot_stats(rv$hd, axial = is_axial()),
                  error = function(e) { boxplot_err <<- conditionMessage(e); NULL })
    if (!is.null(s) && !isTRUE(s$drawable))
      notes <- c(notes, list(tags$div(
        class = "alert alert-secondary py-1 px-2 small mb-2",
        paste0("Circular boxplot not drawn: ", s$reason, "."))))
    if (!is.null(boxplot_err))
      notes <- c(notes, list(tags$div(
        class = "alert alert-secondary py-1 px-2 small mb-2",
        paste0("Circular boxplot error: ", boxplot_err, "."))))
    if (!length(notes)) return(NULL)
    do.call(tagList, notes)
  })

  # "By elapsed time" colours by a real time axis, which needs a valid frame
  # rate (the package hard-errors otherwise). When time-mode is selected with an
  # unset/invalid fps the figure falls back to sequence colouring; surface why.
  output$track_time_note <- renderUI({
    if (!identical(input$track_colour, "time") && !identical(input$track_colour, "speed"))
      return(NULL)
    if (!.fps_is_set(input$frame_rate)) {
      what <- if (identical(input$track_colour, "speed")) "by speed" else "by elapsed time"
      return(tags$div(class = "alert alert-secondary py-1 px-2 small mb-2",
                      paste0("Enter a positive frame rate to colour ", what,
                             " (showing sequence position instead).")))
    }
    if (identical(fps_source_rv(), "data"))
      return(tags$div(class = "alert alert-info py-1 px-2 small mb-2",
                      paste0("From data (", input$frame_rate, " fps).")))
    NULL
  })

  # Frame the preview illustrates: the one the Results will derive in for the
  # loaded data (the demo tracks always have a relative frame, so resolve against
  # rv$ts rather than them).
  preview_coords <- reactive({
    frame <- input$frame %||% "relative"
    if (is.null(rv$ts)) frame else .resolve_heading_coords(rv$ts, frame)
  })

  # Live illustrative preview of the selected method on fixed demo tracks.
  output$method_preview <- renderPlot({
    method <- if (is.null(input$method)) "distal" else input$method
    tryCatch(
      build_method_preview(
        demo_tracks(), method,
        num_or(input$circ0, 0.3), num_or(input$circ1, 0.6),
        coords = preview_coords()
      ),
      error = function(e) {
        message("method_preview render failed: ", conditionMessage(e))
        build_method_preview(demo_tracks(), "none")
      }
    )
  }, res = 110)

  # Step 2 → 3: derive headings, join condition if present
  observeEvent(input$go3, {
    if (identical(rv$mode, "headings")) {
      if (is.null(rv$raw_hd)) {
        # Example headings already built; apply the chosen facet -> show results.
        if (identical(rv$source, "example") && !is.null(rv$hd)) {
          grp <- if (!is.null(input$hd_group) && nzchar(input$hd_group))
            input$hd_group else NULL
          grp2 <- if (!is.null(input$hd_group2) && nzchar(input$hd_group2))
            input$hd_group2 else NULL
          hm <- rv$hd_map; hm$group <- grp; hm$group2 <- grp2; rv$hd_map <- hm
          rv$step <- 3L; rv$error <- NULL; rv$error_detail <- NULL; return()
        }
        rv$error <- "Please upload a file of headings first."
        rv$error_detail <- NULL
        return()
      }
      grp <- if (!is.null(input$hd_group) && nzchar(input$hd_group))
        input$hd_group else NULL
      grp2 <- if (!is.null(input$hd_group2) && nzchar(input$hd_group2))
        input$hd_group2 else NULL
      # The group columns are labels: read the file again naming them, so
      # "1.1" and "1.10" stay two groups (rv$raw_hd was read before they were
      # chosen).
      advice <- hd_mark_advice(rv$raw_hd, input$hd_col)
      if (!is.null(advice)) {
        rv$error <- advice; rv$error_detail <- NULL
        return()
      }
      labs <- c(grp, grp2)
      src <- NULL
      hd <- tryCatch({
        # a sniffed decimal point must be shown by the angle column itself
        radiatR:::.check_decimal_evidence(attr(rv$raw_hd, "decimal_evidence"),
                                          input$hd_col, rv$file_name)
        src <- if (length(labs))
          as.data.frame(upload_read(rv$path, rv$file_name, input$delim_sel,
                                    input$sheet_sel, unique_names = TRUE, id = labs,
                                    decimal_sel = input$decimal_sel))
        else rv$raw_hd
        # the reader's bookkeeping must not reach the headings frame
        attr(src, "decimal_evidence") <- attr(src, "decimal") <- NULL
        build_headings_input(src, col = input$hd_col,
                             units = input$hd_units,
                             convention = input$hd_convention, group = grp)
      },
        error = function(e) {
          rv$error        <- plain_error(e)
          rv$error_detail <- ingest_detail(e)
          NULL
        })
      if (!is.null(hd)) {
        rv$hd     <- hd; rv$method <- NULL; rv$step <- 3L
        rv$error  <- NULL; rv$error_detail <- NULL
        rv$hd_map <- list(col = input$hd_col, units = input$hd_units,
                          convention = input$hd_convention, group = grp, group2 = grp2,
                          text_cols = labs[vapply(labs, function(cl) is.character(src[[cl]]),
                                                  logical(1))],
                          read = upload_read_format(rv$path, rv$file_name, input$delim_sel,
                                                  input$decimal_sel))
      }
      return()
    }
    req(rv$ts)
    method <- if (is.null(input$method)) "distal" else input$method
    if (identical(method, "none")) {
      rv$hd     <- NULL
      rv$method <- method
      rv$step   <- 3L
      rv$error  <- NULL
      rv$error_detail <- NULL
      return()
    }
    c0     <- if (is.null(input$circ0))  0.3 else input$circ0
    c1     <- if (is.null(input$circ1))  0.6 else input$circ1
    hd <- tryCatch(
      derive_hd(rv$ts, method, c0, c1, input$frame %||% "relative"),
      error = function(e) {
        rv$error        <- plain_error(e)
        rv$error_detail <- ingest_detail(e)
        NULL
      }
    )
    if (!is.null(hd)) {
      gc <- if (!is.null(input$cond_col) &&
                nzchar(input$cond_col)) {
        input$cond_col
      } else {
        NULL
      }
      if (!is.null(gc)) {
        id_col   <- rv$ts@cols$id
        df       <- as.data.frame(rv$ts)
        cond_map <- unique(df[, c(id_col, gc), drop = FALSE])
        # derive_headings() names its trial column "id"; join it to the
        # Tracks's own id column (which may be named anything, e.g. trial_id).
        hd       <- merge(hd, cond_map, by.x = "id", by.y = id_col,
                          all.x = TRUE)
      }
      rv$hd     <- hd
      rv$method <- method
      rv$frame  <- input$frame %||% "relative"
      rv$step   <- 3L
      rv$error  <- NULL
      rv$error_detail <- NULL
    }
  })

  # ---- file upload -----------------------------------------------------------
  observeEvent(input$file, {
    req(input$file)
    rv$path      <- input$file$datapath
    rv$source    <- "file"
    rv$file_name <- input$file$name
    if (identical(rv$mode, "headings")) {
      # Clear stale analysis BEFORE anything reads, so a failed parse cannot
      # leave stale headings mistaken for a valid example (PR #20 contract).
      rv$hd     <- NULL
      rv$hd_map <- NULL
      rv$method <- NULL
      rv$ts     <- NULL
      rv$error  <- NULL
      rv$error_detail <- NULL
      rv$raw_hd <- NULL          # the lazy reader (below) repopulates this
      rv$step   <- 1L
      return()
    }
    rv$dialect   <- guess_dialect(rv$path)
    rv$ts        <- NULL
    rv$cond_col  <- NULL
    rv$hd        <- NULL
    rv$error     <- NULL
    rv$error_detail <- NULL
  })

  # Lazy headings read: fires on the file path and the format controls, so the
  # delimiter/sheet selectors are live (the read no longer happens eagerly in
  # the upload observer above). Commits rv$raw_hd only on success (NULL on
  # failure).
  observe({
    req(identical(rv$mode, "headings"), rv$source == "file", rv$path)
    ds <- input$delim_sel
    ss <- input$sheet_sel
    dc <- input$decimal_sel
    parsed <- tryCatch(
      as.data.frame(upload_read(rv$path, rv$file_name, ds, ss, unique_names = TRUE,
                                  decimal_sel = dc)),
      error = function(e) {
        rv$error        <- plain_error(e)
        rv$error_detail <- ingest_detail(e)
        NULL
      })
    rv$raw_hd <- if (is.null(parsed) || !ncol(parsed)) NULL else parsed
  })

  # ---- example dataset -------------------------------------------------------
  observeEvent(input$load_example, {
    ensure_pkgs()   # materialising the cpunctatus Tracks needs radiatR's S4 class
    ts <- tryCatch(
      example_ts(),
      error = function(e) NULL
    )
    if (is.null(ts)) {
      rv$error <- "Could not load the bundled example dataset."
      rv$error_detail <- NULL
      return()
    }
    rv$ts        <- ts
    rv$path      <- NULL
    rv$source    <- "example"
    rv$file_name <- NULL
    rv$dialect   <- NULL
    rv$cond_col  <- detect_cond_col(ts)
    rv$hd        <- NULL
    rv$step      <- 2L
    rv$error     <- NULL
    rv$error_detail <- NULL
  })

  observeEvent(input$load_example_hd, {
    ensure_pkgs()
    ex <- tryCatch(example_ts(), error = function(e) NULL)
    if (is.null(ex)) {
      rv$error <- "Could not load the bundled example dataset."
      rv$error_detail <- NULL
      return()
    }
    # Carry EVERY condition column (e.g. arc, type), not just one, so the facet
    # is selectable in Configure rather than hard-coded.
    cond_cols <- grouping_cols(ex)
    id_col    <- ex@cols$id
    hd <- tryCatch({
      hd0  <- derive_headings(ex, rule = "distal", coords = "relative")
      cond <- unique(as.data.frame(ex)[, c(id_col, cond_cols), drop = FALSE])
      hd0  <- merge(hd0, cond, by.x = "id", by.y = id_col, all.x = TRUE)
      build_headings_input(hd0, col = "heading", units = "radians",
                           convention = "unit_circle")
    }, error = function(e) NULL)
    if (is.null(hd)) {
      rv$error <- "Could not build the example headings."
      rv$error_detail <- NULL
      return()
    }
    rv$mode      <- "headings"
    rv$hd        <- hd
    rv$hd_map    <- list(col = "heading", units = "radians",
                         convention = "unit_circle",
                         group = detect_cond_col(ex), group2 = NULL)   # default facet (e.g. arc)
    rv$ts        <- NULL
    rv$raw_hd    <- NULL
    rv$source    <- "example"
    rv$file_name <- NULL
    rv$method    <- NULL
    rv$step      <- 2L     # land on Configure so the facet can be chosen
    rv$error     <- NULL
    rv$error_detail <- NULL
  })

  # ---- wizard ----------------------------------------------------------------
  output$wizard <- renderUI({

    err_box <- if (!is.null(rv$error))
      div(class = "alert alert-danger mt-3 mb-0",
          div(rv$error),
          if (!is.null(rv$drop_offer))
            actionButton("drop_btn",
                         sprintf("Drop these %d row(s) and continue", rv$drop_offer$n),
                         class = "btn-sm btn-outline-danger my-1"),
          if (!is.null(rv$error_detail))
            tags$details(class = "small mt-1",
              tags$summary("Technical details"),
              tags$pre(class = "mb-0", rv$error_detail)))
    else if (!is.null(rv$error_detail))
      # A successful ingest can still emit warnings (captured above); surface
      # them in a muted notes box rather than dropping them silently.
      div(class = "alert alert-secondary mt-3 mb-0",
          tags$details(class = "small mb-0",
            tags$summary("Notes"),
            tags$pre(class = "mb-0", rv$error_detail)))
    err_box <- tagList(dropped_ui(), err_box)

    # ---- Step 1: upload ----
    if (rv$step == 1L) {
      tagList(
        ui_upload_logo(),
        h5("Upload your tracking data"),
        p(class = "text-muted",
          "Upload a CSV or text file from your tracking software.",
          " EthoVision, DeepLabCut, SLEAP, TRex, ANY-maze,",
          " TrackMate, idtracker.ai, and others are supported."),
        radioButtons(
          "input_type", "What are you uploading?",
          choices = c("Trajectories (track coordinates)" = "trajectories",
                      "Headings (one angle per trial)"    = "headings"),
          # Bind to rv$mode so a wizard re-render (triggered when rv$mode/rv$error
          # change) preserves the user's choice instead of resetting to the
          # hardcoded default -- otherwise selecting "Headings" snaps back.
          selected = rv$mode %||% "trajectories", inline = TRUE
        ),
        fileInput(
          "file", NULL,
          accept      = accepted_exts(rv$mode),
          buttonLabel = "Browse…",
          placeholder = "No file selected"
        ),
        if (identical(rv$mode, "headings")) {
          div(class = "text-muted small mb-2",
              "Don't have a file? ",
              actionLink("load_example_hd",
                         "Load example headings (from the millipede dataset)"),
              ".")
        } else {
          div(class = "text-muted small mb-2",
              "Don't have a file handy? ",
              actionLink("load_example",
                         "Load the example millipede dataset"),
              " (Cylindroiulus punctatus, 235 trials).")
        },
        uiOutput("format_box"),
        uiOutput("delim_box"),
        uiOutput("decimal_box"),
        uiOutput("sheet_box"),
        uiOutput("normalize_box"),
        uiOutput("mapping_box"),
        uiOutput("preview_section"),
        err_box
      )

    # ---- Step 2: configure (headings) ----
    } else if (rv$step == 2L && identical(rv$mode, "headings")) {
      if (is.null(rv$raw_hd)) {
        choices <- .hd_group_choices(rv$hd)
        cur     <- (rv$hd_map %||% list(group = NULL))$group %||% ""
        tagList(
          h5("Example headings"),
          p(class = "text-muted",
            "These headings come from the bundled millipede dataset — no column ",
            "mapping is needed. Choose how to facet the plot, then click Analyse."),
          radioButtons(
            "data_model", "Directionality",
            choices  = c("Directional" = "directional", "Axial" = "axial"),
            selected = "directional", inline = TRUE
          ),
          selectInput("hd_group", "Facet rows",
                      choices  = c("(none)" = "", stats::setNames(choices, choices)),
                      selected = cur),
          selectInput("hd_group2", "Facet cols (grid)",
                      choices  = c("(none)" = "", stats::setNames(choices, choices)),
                      selected = ""),
          err_box
        )
      } else {
        ang <- hd_angle_choices(rv$raw_hd, current = isolate(input$hd_col))
        all_cols <- names(rv$raw_hd)
        tagList(
          h5("Describe your headings"),
          radioButtons(
            "data_model", "Directionality",
            choices  = c("Directional" = "directional", "Axial" = "axial"),
            selected = "directional", inline = TRUE
          ),
          selectInput("hd_col", "Angle column", choices = ang$choices,
                      selected = ang$selected),
          uiOutput("hd_mark_note"),
          selectInput("hd_units", "Units",
                      choices = c("Degrees" = "degrees", "Radians" = "radians"),
                      selected = "degrees"),
          selectInput("hd_convention", "Angle convention",
                      choices = c("Unit circle (0° = East, counter-clockwise)" = "unit_circle",
                                  "Compass (0° = North, clockwise)"            = "clock"),
                      selected = "unit_circle"),
          selectInput("hd_group", "Group column (optional)",
                      choices = c("(none)" = "", stats::setNames(all_cols, all_cols)),
                      selected = ""),
          selectInput("hd_group2", "Facet cols (grid)",
                      choices = c("(none)" = "", stats::setNames(all_cols, all_cols)),
                      selected = ""),
          err_box
        )
      }

    # ---- Step 2: configure ----
    } else if (rv$step == 2L) {
      tagList(
        layout_columns(
          col_widths = c(5, 7),
          # ---- left: controls ----
          tagList(
            # Directionality is the more general question (does head==tail?), so it
            # comes before the heading-measurement method.
            radioButtons(
              "data_model", "Directionality",
              choices  = c("Directional" = "directional", "Axial" = "axial"),
              selected = "directional", inline = TRUE
            ),
            h5("How should headings be measured?"),
            selectInput(
              "method", NULL,
              choices = list(
                "None (no headings)" = "none",
                "Directional methods" = c(
                  "Direction at furthest point"       = "distal",
                  "Net displacement direction"        = "net",
                  "Exit direction (ring crossing)"    = "crossing",
                  "Longest straight segment"          = "straight",
                  "Smoothed (windowed) net direction" = "window_net",
                  "Mean direction from centre"        = "origin_mean",
                  "Mean velocity direction"           = "velocity_mean",
                  "Direction at peak speed"           = "maxspeed_window",
                  "Von Mises fit of step directions"  = "vm_fit",
                  "Goal-biased direction"             = "goal_bias"
                ),
                "Axial methods" = c(
                  "Movement axis (velocity, axial)"   = "velocity_axis",
                  "Principal axis (PCA)"              = "pca_axis",
                  "Robust straight-line fit (RANSAC)" = "ransac_straight"
                )
              ),
              selected = "distal"
            ),
            uiOutput("method_help"),
            uiOutput("method_model_note"),
            uiOutput("frame_ui"),
            conditionalPanel(
              "input.method == 'crossing'",
              tags$hr(),
              p(class = "text-muted small",
                "Radii as fractions of the unit circle radius",
                " (0 = centre, 1 = edge)."),
              layout_columns(
                col_widths = c(6, 6),
                sliderInput(
                  "circ0", "Inner ring",
                  min = 0.05, max = 0.9, value = 0.3, step = 0.05
                ),
                sliderInput(
                  "circ1", "Outer ring",
                  min = 0.1, max = 1.0, value = 0.6, step = 0.05
                )
              )
            ),
            uiOutput("cond_ui"),
            err_box
          ),
          # ---- right: live method preview ----
          card(
            card_header("How this method finds the heading"),
            card_body(
              padding = 0,
              plotOutput("method_preview", height = "360px")
            )
          )
        )
      )

    # ---- Step 3: results ----
    } else {
      tagList(
        uiOutput("dropped_banner"),
        uiOutput("attrition_banner"),
        navset_card_tab(
          id = "results_tab",
          # ---- Circular plots -----------------------------------------------
          nav_panel(
            "Circular plots",
            layout_sidebar(
              sidebar = sidebar(
                width = 360, position = "right", open = TRUE,
                accordion(
                  open = "Display",
                  accordion_panel(
                    "Display",
                    selectInput(
                      "plot_theme", "Theme",
                      choices = c("Void"          = "void",
                                  "Minimal"       = "minimal",
                                  "Classic"       = "classic",
                                  "Black & white" = "bw",
                                  "Grey"          = "grey",
                                  "Light"         = "light",
                                  "Dark"          = "dark",
                                  "Line draw"     = "linedraw"),
                      selected = "void"
                    ),
                    selectInput(
                      "angle_labels", "Angle labels",
                      choices = c("Degrees (45°)" = "degrees",
                                  "None"          = "none",
                                  "Radians (π/4)" = "radians"),
                      selected = "degrees"
                    ),
                    selectInput(
                      "heading_display", "Heading display",
                      choices = c("Points (overlapping)"  = "points",
                                  "Stacked dots (inward)" = "stacked",
                                  "None"                  = "none"),
                      selected = "points"
                    ),
                    selectInput(
                      "track_colour", "Track colour",
                      choices = c("By trajectory"                = "trajectory",
                                  "By sequence (start → finish)" = "sequence",
                                  "By elapsed time"              = "time",
                                  "By speed"                     = "speed"),
                      selected = "trajectory"
                    ),
                    conditionalPanel(
                      condition = "input.track_colour == 'time' || input.track_colour == 'speed'",
                      numericInput("frame_rate", "Frame rate (fps)", value = NA, min = 0, step = 1),
                      uiOutput("track_time_note")
                    ),
                    uiOutput("group_by_ui"),
                    uiOutput("colour_by_ui"),
                    tags$hr(class = "my-2"),
                    div(
                      style = paste("display: grid;",
                                    "grid-template-columns: repeat(auto-fit, minmax(8.5rem, 1fr));",
                                    "column-gap: 0.75rem;"),
                      .layer_switch("show_tracks",    "Trajectories",       TRUE),
                      .layer_switch("show_oob",       "Allow points beyond the circumference", FALSE),
                      .layer_switch("show_arrow",     "Directedness arrow", TRUE),
                      .layer_switch("show_ci",        "Mean-direction CI",  FALSE),
                      .layer_switch("show_vectors",   "Heading vectors",    FALSE),
                      .layer_switch("show_rayleigh",  "Rayleigh circle",    FALSE),
                      .layer_switch("show_vtest",     "V-test line",        FALSE),
                      .layer_switch("show_boxplot",   "Circular boxplot",   FALSE),
                      .layer_switch("show_quadrants", "Quadrant lines",     FALSE),
                      .layer_switch("show_rings",     "Guide rings",        FALSE)
                    ),
                    uiOutput("boxplot_note"),
                    tags$hr(class = "my-2"),
                    sliderInput(
                      "preview_px", "Preview size (px)",
                      min = 480, max = 1000, value = 840, step = 20
                    ),
                    tags$span(
                      class = "text-muted small",
                      "On-screen only; export size is set under Image export."
                    )
                  ),
                  ui_image_export(""),
                  ui_code_section(
                    "figure_code", "dl_code",
                    paste0("radiatR code that reproduces this figure. Edit the ",
                           "data path to point at your own file."))
                )
              ),
              card(
                card_header("Tracks and headings"),
                # fillable = FALSE so the plot honours the explicit pixel height
                # from the Preview-size slider instead of being stretched to fill
                # the fillable card (which pinned it to the viewport height and
                # made the slider appear to do nothing).
                card_body(
                  padding = 0,
                  fillable = FALSE,
                  uiOutput("track_plot_ui")
                )
              )
            )
          ),
          # ---- Summary & stats ----------------------------------------------
          nav_panel(
            "Summary & stats",
            layout_sidebar(
              sidebar = sidebar(
                width = 360, position = "right", open = TRUE,
                selectInput("omnibus_test", "Omnibus test",
                            choices  = c("Rao spacing" = "rao",
                                         "Hermans-Rasson" = "hermans_rasson"),
                            selected = "rao"),
                tags$hr(class = "my-2"),
                uiOutput("dl_csv_ui"),             # stats CSV download (moved from Download)
                tags$hr(class = "my-2"),
                tags$strong("R code"),
                tags$p(class = "text-muted small",
                       "radiatR code reproducing this analysis. Edit the data path",
                       " to point at your own file."),
                tags$div(style = "max-height:320px; overflow:auto;",
                         verbatimTextOutput("stats_code")),
                tags$button(
                  class   = "btn btn-sm btn-outline-primary w-100 mb-2",
                  onclick = paste0(
                    "navigator.clipboard.writeText(",
                    "document.getElementById('stats_code').innerText);",
                    "this.innerText='Copied';",
                    "setTimeout(()=>this.innerText='Copy R code',1200);"),
                  "Copy R code"),
                downloadButton("dl_stats_code", "Download .R",
                               class = "btn-sm btn-outline-primary w-100")
              ),
              card(
                card_header("Summary"),
                card_body(
                  tableOutput("summary_tbl"),
                  tags$hr(class = "my-2"),
                  tags$strong("Model selection (AICc)"),
                  tableOutput("model_sel_tbl"),
                  tags$hr(class = "my-2"),
                  tags$strong("Group comparison"),
                  tableOutput("group_compare_tbl")
                )
              )
            )
          )
          ,
          # ---- Track metrics ------------------------------------------------
          nav_panel(
            "Track metrics",
            layout_sidebar(
              sidebar = sidebar(
                width = 360, position = "right", open = TRUE,
                accordion(
                  open = TRUE,
                  accordion_panel(
                    "Profile",
                    radioButtons("kin_metric", "Metric",
                                 choices = c("Speed" = "speed",
                                             "Turning rate" = "turning"),
                                 selected = "speed"),
                    conditionalPanel(
                      condition = "input.kin_metric == 'turning'",
                      selectInput("kin_units", "Turning units",
                                  choices = c("Radians" = "radians",
                                              "Degrees" = "degrees"),
                                  selected = "radians")
                    ),
                    uiOutput("kin_track_ui"),
                    uiOutput("kin_colour_ui"),
                    sliderInput("kin_smooth", "Smoothing (window, points)",
                                min = 1, max = 25, value = 1, step = 1),
                    conditionalPanel(
                      condition = "input.kin_smooth > 1",
                      checkboxInput("kin_show_raw", "Show raw (faint)",
                                    value = FALSE)
                    ),
                    numericInput("kin_frame_rate", "Frame rate (fps)",
                                 value = NA, min = 0, step = 1),
                    uiOutput("kin_note")
                  ),
                  ui_image_export("kin_"),
                  ui_code_section(
                    "kinematics_code", "dl_kinematics_code",
                    paste0("radiatR code that reproduces this profile. Edit the ",
                           "data path to point at your own file."))
                )
              ),
              card(
                card_header("Per-track metrics"),
                card_body(
                  uiOutput("track_metric_scope_ui"),
                  uiOutput("track_metrics_summary"),
                  div(style = "max-height:300px; overflow:auto;",
                      tableOutput("track_metrics_table")),
                  downloadButton("dl_track_metrics", "Download (CSV)",
                                 class = "btn-sm btn-outline-primary")
                )
              ),
              card(
                card_header("Kinematics profile"),
                card_body(
                  padding = 0,
                  fillable = FALSE,
                  plotOutput("kinematics_plot", height = "460px")
                )
              )
            )
          )
        )
      )
    }
  })

  # ---- step 1 sub-outputs ----------------------------------------------------
  output$format_box <- renderUI({
    req(rv$path)
    d   <- rv$dialect
    lbl <- names(DIALECT_CHOICES)[DIALECT_CHOICES == d]
    lbl <- if (length(lbl)) lbl[1L] else NULL

    tagList(
      if (!is.null(lbl) && d != "generic")
        div(
          class = "alert alert-info py-1 px-2 small mb-2",
          "Format detected: ", tags$b(lbl)
        )
      else
        div(
          class = "alert alert-secondary py-1 px-2 small mb-2",
          "Format not recognised — please select your software:"
        ),
      selectInput(
        "dialect_sel", "Tracking software",
        choices  = DIALECT_CHOICES,
        selected = if (!is.null(d)) d else "auto"
      )
    )
  })

  # Delimiter override for delimited (non-Excel) uploads. Own renderUI so a
  # wizard re-render does not reset the choice.
  output$delim_box <- renderUI({
    req(rv$path)
    ext <- tolower(sub(".*\\.", "", rv$path))
    if (ext %in% c("xlsx", "xls")) return(NULL)
    selectInput("delim_sel", "Delimiter",
                choices = c("Auto-detect" = "auto", "Comma" = ",",
                            "Semicolon" = ";", "Tab" = "\t"),
                selected = "auto")
  })

  # Decimal-mark override for delimited (non-Excel) uploads; own renderUI like
  # delim_box so a wizard re-render does not reset the choice.
  output$decimal_box <- renderUI({
    req(rv$path)
    ext <- tolower(sub(".*\\.", "", rv$path))
    if (ext %in% c("xlsx", "xls")) return(NULL)
    selectInput("decimal_sel", "Decimal mark",
                choices = c("Auto-detect" = "auto", "Point (.)" = ".", "Comma (,)" = ","),
                selected = "auto")
  })

  # Under the headings angle picker: the chosen column holds numbers in the
  # other decimal mark (see hd_angle_choices()).
  output$hd_mark_note <- renderUI({
    req(rv$raw_hd)
    advice <- hd_mark_advice(rv$raw_hd, input$hd_col)
    if (is.null(advice)) return(NULL)
    div(class = "alert alert-warning py-1 px-2 small", advice)
  })

  # Worksheet selector for Excel uploads. Own renderUI (like delim_box) so a
  # wizard re-render does not reset the choice. Mutually exclusive with
  # delim_box by construction: delim_box returns NULL for Excel, this NULL
  # for non-Excel.
  output$sheet_box <- renderUI({
    req(rv$path)
    ext <- tolower(tools::file_ext(rv$file_name %||% rv$path))
    if (!ext %in% c("xlsx", "xls")) return(NULL)
    sheets <- tryCatch(readxl::excel_sheets(rv$path), error = function(e) character(0))
    if (!length(sheets)) return(NULL)
    selectInput("sheet_sel", "Worksheet", choices = sheets, selected = sheets[1L])
  })

  # The per-trajectory shape transform (normalize_xy). Own renderUI, like
  # delim_box: the wizard re-renders on rv$mode/rv$error changes and a shared
  # render would reset the user's choice. Trajectories only -- normalize_xy is
  # x/y-only, and the bundled example bypasses load_ts entirely.
  output$normalize_box <- renderUI({
    req(rv$path)
    if (identical(rv$mode, "headings")) return(NULL)
    tagList(
      radioButtons(
        "normalize_sel", "Coordinates",
        choices = c("Keep raw coordinates as supplied" = "FALSE",
                    "Fit each trajectory to the unit circle (shape only)" = "TRUE",
                    "Calibrate to a fixed origin and radius" = "calibrate"),
        selected = "TRUE"
      ),
      div(class = "text-muted small mb-2",
          "Fit rescales each trajectory independently to its own bounding box.",
          " It preserves path shape, not bearings from a fixed origin, so it is",
          " not a calibration."),
      # Shown only in calibrate mode. A single radio makes calibrate and the
      # shape normalizer mutually exclusive by construction. Inputs default
      # blank: calibration is never applied until real values are entered.
      conditionalPanel(
        "input.normalize_sel == 'calibrate'",
        div(class = "text-muted small mb-2",
            "Translate and scale every trajectory by one shared reference:",
            " (x, y) becomes (x - origin) / radius, preserving bearings from",
            " the origin. In an experimental arena the origin is the arena",
            " centre and the radius its edge distance, in the raw coordinate",
            " units."),
        numericInput("origin_x", "Origin x", value = NA),
        numericInput("origin_y", "Origin y", value = NA),
        numericInput("radius",   "Radius",   value = NA)
      )
    )
  })

  # Column mapping for a Generic CSV upload. Kept in its OWN renderUI (not in
  # format_box) so that reading input$dialect_sel here does not re-render and
  # reset the dialect selector.
  output$mapping_box <- renderUI({
    req(rv$path)
    d <- if (!is.null(input$dialect_sel) && input$dialect_sel != "auto")
      input$dialect_sel else rv$dialect
    if (!identical(d, "generic")) return(NULL)
    df <- tryCatch(
      upload_read(rv$path, rv$file_name, input$delim_sel, input$sheet_sel,
                  decimal_sel = input$decimal_sel),
      error = function(e) NULL)
    if (is.null(df) || !ncol(df)) return(NULL)
    df   <- utils::head(as.data.frame(df), 200L)
    cols <- names(df)
    g    <- tryCatch(radiatR:::guess_columns(df), error = function(e) list())
    sel  <- function(v) if (is.null(v)) "" else v
    xy   <- stats::setNames(cols, cols)
    tagList(
      tags$hr(),
      div(class = "small text-muted mb-1", "Map columns:"),
      layout_columns(
        col_widths = c(6, 6),
        selectInput("map_x", "X", choices = xy, selected = sel(g$x)),
        selectInput("map_y", "Y", choices = xy, selected = sel(g$y))),
      layout_columns(
        col_widths = c(6, 6),
        selectInput("map_time", "Time",
                    choices = c("(row order)" = "", xy), selected = sel(g$time)),
        selectInput("map_id", "ID",
                    choices = c("(single track)" = "", xy), selected = sel(g$id)))
    )
  })

  output$preview_section <- renderUI({
    req(rv$path)
    tagList(tags$hr(), tableOutput("preview_tbl"))
  })

  output$preview_tbl <- renderTable({
    req(rv$path)
    d <- if (!is.null(input$dialect_sel) && input$dialect_sel != "auto")
      input$dialect_sel else rv$dialect
    # the mapped id is a label column (see upload_read())
    id <- if (identical(d %||% "generic", "generic") && !is.null(input$map_id) &&
              nzchar(input$map_id)) input$map_id else NULL
    df <- tryCatch(
      upload_preview(rv$path, rv$file_name, dialect = d %||% "generic",
                     input$delim_sel, input$sheet_sel, id = id,
                     decimal_sel = input$decimal_sel),
      error = function(e) NULL)
    if (is.null(df))
      return(data.frame(Note = "Could not preview this file"))
    if (identical(names(df), "Line")) df else utils::head(df, 5L)
  }, striped = TRUE, caption = "Preview",
     caption.placement = "top")

  # ---- step 2 condition selector ---------------------------------------------
  output$cond_ui <- renderUI({
    req(rv$ts)
    col <- rv$cond_col
    if (is.null(col)) return(NULL)

    other <- grouping_cols(rv$ts)
    if (!length(other)) return(NULL)
    choices <- c("None" = "", stats::setNames(other, other))

    tagList(
      tags$hr(),
      selectInput("cond_col",  "Facet rows",        choices = choices, selected = col),
      selectInput("cond_col2", "Facet cols (grid)", choices = choices, selected = "")
    )
  })

  # Colour-by selector (Display panel). Defaults to per-trajectory colour; any
  # grouping column can instead drive the track + marker colour. Independent of
  # the Facet-by selector.
  output$colour_by_ui <- renderUI({
    if (identical(rv$mode, "headings")) {
      grp <- if (!is.null(input$group_by) && nzchar(input$group_by))
        input$group_by else NULL
      choices <- c("Follow group" = SPEC_GROUP_KEY, "Single colour" = SPEC_TRAJ_KEY)
      if (!is.null(grp)) choices <- c(choices, stats::setNames(grp, grp))
      sel <- if (!is.null(input$colour_by) && input$colour_by %in% choices)
        input$colour_by else SPEC_GROUP_KEY
      return(selectInput("colour_by", "Colour by", choices = choices, selected = sel))
    }
    req(rv$ts)
    cats    <- colour_cols(rv$ts)
    choices <- c("Follow group" = SPEC_GROUP_KEY,
                 "Trajectory"    = TRAJ_COLOUR_KEY,
                 stats::setNames(cats, cats))
    sel <- if (!is.null(input$colour_by) && input$colour_by %in% choices)
      input$colour_by else SPEC_GROUP_KEY
    selectInput("colour_by", "Colour by", choices = choices, selected = sel)
  })

  # Group-by selector: the analytical unit (drives per-group overlays + stats),
  # independent of the Facet controls. Trajectory mode offers the low-cardinality
  # grouping columns; headings mode offers the headings-frame categorical columns.
  output$group_by_ui <- renderUI({
    if (identical(rv$mode, "headings")) {
      cols <- .hd_group_choices(rv$hd)
    } else {
      req(rv$ts)
      cols <- grouping_cols(rv$ts)
    }
    choices <- c("(none)" = "", stats::setNames(cols, cols))
    sel <- if (!is.null(input$group_by) && input$group_by %in% choices)
      input$group_by else ""
    selectInput("group_by", "Group by (overlays + stats)",
                choices = choices, selected = sel)
  })

  # ---- step 3 outputs --------------------------------------------------------

  # Treat an as-yet-unrendered toggle (NULL) as its declared default.
  tog <- function(v, default) if (is.null(v)) default else isTRUE(v)

  # Single app-wide capture frame rate. Both the Circular time/speed colouring
  # and the Kinematics tab read this one value; the per-sidebar numeric inputs
  # (frame_rate, kin_frame_rate) mirror it. Equality guards prevent feedback loops.
  fps_rv <- reactiveVal(NA_real_)
  # Provenance of the current fps: "none" (unset), "data" (from a loaded Tracks),
  # or "user" (typed into a control). Drives the frame-rate helper text.
  fps_source_rv <- reactiveVal("none")
  # both-unset -> TRUE (suppress the sync feedback loop); exactly-one-set -> FALSE
  # (a real change to propagate); both-set -> numeric equality.
  fps_equal <- function(a, b) {
    sa <- .fps_is_set(a); sb <- .fps_is_set(b)
    if (!sa && !sb) return(TRUE)
    if (sa != sb)   return(FALSE)
    isTRUE(all.equal(a, b))
  }
  # When a loaded Tracks carries its own capture rate, adopt it as the app fps
  # and mark the source as data-provided.
  observeEvent(rv$ts, {
    fr <- if (is.null(rv$ts)) NULL else frame_rate(rv$ts)
    if (.fps_is_set(fr) && !fps_equal(fr, fps_rv())) {
      fps_rv(fr)
      fps_source_rv("data")
    }
  }, ignoreNULL = FALSE)
  observeEvent(input$frame_rate, {
    if (!fps_equal(input$frame_rate, fps_rv())) {
      fps_rv(input$frame_rate)
      # A genuine user edit (not the programmatic echo, which compares equal and
      # never reaches here): valid -> user, cleared -> none.
      fps_source_rv(if (.fps_is_set(input$frame_rate)) "user" else "none")
    }
  }, ignoreInit = TRUE, ignoreNULL = FALSE)
  observeEvent(fps_rv(), {
    v <- fps_rv()
    if (!fps_equal(input$frame_rate, v))
      updateNumericInput(session, "frame_rate", value = v)
    if (!fps_equal(input$kin_frame_rate, v))
      updateNumericInput(session, "kin_frame_rate", value = v)
  }, ignoreNULL = FALSE)

  # The plot spec resolved from the current inputs (shared by the figure and the
  # code export so they cannot drift).
  current_spec <- function() {
    if (identical(rv$mode, "headings")) {
      # Read the mapping from rv$hd_map (set by Step 2 OR the example button) so
      # the spec is correct even when Step 2 was skipped (example path).
      map <- rv$hd_map %||% list(col = "heading", units = "radians",
                                 convention = "unit_circle", group = NULL, group2 = NULL)
      grp  <- map$group
      grp2 <- map$group2
      return(build_plot_spec(
        ts = NULL, hd = rv$hd, method = NULL,
        data = list(
          source = if (identical(rv$source, "example")) "example" else "file",
          mode = "headings", path = rv$file_name %||% "your_headings.csv",
          col = map$col, units = map$units,
          convention = map$convention, group = grp, text_cols = map$text_cols,
          delim = map$read$delim, decimal = map$read$decimal, latin1 = map$read$latin1,
          sheet = input$sheet_sel,
          ext = tolower(tools::file_ext(rv$file_name %||% ""))),
        inputs = list(
          cond_col = grp, hd_group2 = grp2, group_by = input$group_by,
          colour_by = input$colour_by,
          omnibus_test = input$omnibus_test,
          plot_theme = input$plot_theme, angle_labels = input$angle_labels,
          heading_display = input$heading_display,
          track_colour = input$track_colour %||% "trajectory",
          frame_rate = fps_rv(),
          show_arrow = tog(input$show_arrow, TRUE),
          show_vectors = tog(input$show_vectors, FALSE),
          show_rayleigh = tog(input$show_rayleigh, FALSE),
          show_ci = tog(input$show_ci, FALSE),
          axial = is_axial(),
          show_vtest = tog(input$show_vtest, FALSE),
          show_boxplot = tog(input$show_boxplot, FALSE),
          show_quadrants = tog(input$show_quadrants, FALSE),
          show_rings = tog(input$show_rings, FALSE))))
    }
    gc <- if (!is.null(input$cond_col) && nzchar(input$cond_col))
      input$cond_col else NULL
    ro <- upload_read_opts(rv$file_name, input$delim_sel, input$decimal_sel,
                           input$sheet_sel)
    build_plot_spec(
      ts = rv$ts, hd = rv$hd, method = rv$method,
      data = list(
        source  = if (identical(rv$source, "example")) "example" else "file",
        mode    = "trajectories",
        path    = rv$file_name %||% "your_tracks.csv",
        dialect = if (is.null(rv$dialect) || rv$dialect %in% c("auto", "generic"))
          NULL else rv$dialect,
        normalize_xy = rv$normalize_xy,
        origin = rv$origin, radius = rv$radius, drop = rv$drop_used,
        delim = ro$delim, decimal = ro$decimal, sheet = ro$sheet),
      inputs = list(
        cond_col = input$cond_col, cond_col2 = input$cond_col2,
        group_by = input$group_by,
        colour_by = input$colour_by,
        omnibus_test = input$omnibus_test,
        circ0 = input$circ0, circ1 = input$circ1,
        plot_theme = input$plot_theme, angle_labels = input$angle_labels,
        heading_display = input$heading_display,
        track_colour = input$track_colour %||% "trajectory",
        frame = rv$frame %||% "relative",
        frame_rate = fps_rv(),
        # Path-metrics caption, none mode only (no headings). spec_to_plot and
        # spec_to_code render and reproduce it.
        caption  = if (is.null(rv$hd)) straightness_caption(rv$ts, gc) else NULL,
        show_tracks = tog(input$show_tracks, TRUE),
        show_oob    = tog(input$show_oob,    FALSE),
        show_arrow  = tog(input$show_arrow,  TRUE),
        show_vectors = tog(input$show_vectors, FALSE),
        show_rayleigh = tog(input$show_rayleigh, FALSE),
        show_ci    = tog(input$show_ci,    FALSE),
        axial      = is_axial(),
        show_vtest = tog(input$show_vtest, FALSE),
        show_boxplot = tog(input$show_boxplot, FALSE),
        show_quadrants = tog(input$show_quadrants, FALSE),
        show_rings     = tog(input$show_rings,     FALSE)))
  }

  # The Results figure -- tracks, every statistical overlay (CI, Rayleigh,
  # V-test), and the none-mode caption -- now comes entirely from the shared
  # spec, so the code export reproduces exactly what is on screen.
  build_results_plot <- function() spec_to_plot(current_spec(), rv$ts, rv$hd)

  # Preview canvas height tracks the chosen export aspect ratio so the on-screen
  # plot reflects the width/height the user will download. The Preview size
  # slider scales the canvas without affecting the exported file. Width fills
  # the card.
  output$track_plot_ui <- renderUI({
    w    <- num_or(input$plot_w, 7)
    h    <- num_or(input$plot_h, 7)
    base <- num_or(input$preview_px, 840)
    px   <- max(160, min(1000, round(base * (h / w))))
    plotOutput("track_plot", height = paste0(px, "px"))
  })

  output$track_plot <- renderPlot({
    req(rv$ts %||% rv$hd)
    p <- tryCatch(
      build_results_plot(),
      error = function(e) {
        msg <- conditionMessage(e)
        message("track_plot render failed: ", msg)
        ggplot() +
          annotate(
            "text", x = 0, y = 0,
            label = paste0("Plot error:\n", msg),
            colour = "grey40", size = 3.5
          ) +
          theme_void()
      }
    )
    print(p)
  }, res = 120)

  # Rows the user agreed to leave out of the uploaded file, shown under the
  # wizard and above the results. An example or a headings upload drops none.
  dropped_ui <- reactive({
    if (is.null(rv$ts) || !identical(rv$source, "file")) return(NULL)
    note <- dropped_note(rv$dropped, rv$drop_used)
    if (!is.null(note)) div(class = "alert alert-warning mt-3 mb-3", note)
  })
  output$dropped_banner <- renderUI(dropped_ui())

  # Attrition banner: shown only when some trials produced no heading. Derived
  # headings get a loud bias caveat; provided headings get a neutral note.
  output$attrition_banner <- renderUI({
    if (is.null(rv$hd)) return(NULL)
    derived <- !identical(rv$mode, "headings") &&
               !is.null(rv$method) && !identical(rv$method, "none")
    if (derived) {
      n_total   <- attr(rv$hd, "n_total")   %||% nrow(rv$hd)
      n_missing <- attr(rv$hd, "n_missing") %||% sum(is.na(rv$hd$heading))
      # from the data: the condition-column merge() drops the attributes
      n_trials         <- length(unique(rv$hd$id))
      n_trials_missing <- length(unique(rv$hd$id[is.na(rv$hd$heading)]))
      rule      <- rv$method
    } else {
      n_total   <- nrow(rv$hd)
      n_missing <- sum(is.na(rv$hd$heading))
      n_trials  <- n_trials_missing <- NULL
      rule      <- NULL
    }
    note <- attrition_note(n_total, n_missing, derived = derived, rule = rule,
                           n_trials = n_trials, n_trials_missing = n_trials_missing)
    if (is.null(note)) return(NULL)
    div(class = if (derived) "alert alert-warning mt-0 mb-3"
                else "alert alert-info mt-0 mb-3",
        note)
  })

  # Single gate for every download: no active error, and a results object present.
  results_ok <- reactive({
    is.null(rv$error) && !is.null(rv$ts %||% rv$hd)
  })

  # Single source of truth for the summary's headings frame + group column, used
  # by output$summary_tbl, model_sel(), and output$model_sel_tbl so they cannot
  # drift. has_hd = FALSE means no headings (none-mode / not yet loaded).
  summary_ctx <- reactive({
    if (identical(rv$mode, "headings")) {
      if (is.null(rv$hd)) return(list(has_hd = FALSE, mode = "none"))
      hd     <- rv$hd
      grp_by <- if (!is.null(input$group_by) && nzchar(input$group_by) &&
                    input$group_by %in% names(hd)) input$group_by else NULL
      by_col <- grp_by
      pooled <- is.null(by_col)
      if (pooled) { hd[[".all"]] <- "All"; by_col <- ".all" }
      return(list(has_hd = TRUE, mode = "headings", hd = hd, by_col = by_col,
                  pooled = pooled, gc = NULL))
    }
    if (is.null(rv$ts)) return(list(has_hd = FALSE, mode = "none"))
    gc <- if (!is.null(input$cond_col) && nzchar(input$cond_col)) input$cond_col else NULL
    grp_by <- if (!is.null(input$group_by) && nzchar(input$group_by)) input$group_by else NULL
    if (is.null(rv$hd)) return(list(has_hd = FALSE, mode = "none_traj", gc = gc))
    hd <- rv$hd
    # Attach the group column onto hd if it isn't already there (the app merges
    # only the facet column at Analyse time). Keyed on the derive_headings "id".
    if (!is.null(grp_by) && !(grp_by %in% names(hd))) {
      idc  <- rv$ts@cols$id
      gmap <- unique(as.data.frame(rv$ts)[, c(idc, grp_by), drop = FALSE])
      hd   <- merge(hd, gmap, by.x = "id", by.y = idc, all.x = TRUE)
    }
    by_col <- grp_by %||% (if (!is.null(gc)) gc else "id")
    list(has_hd = TRUE, mode = "traj", hd = hd, by_col = by_col,
         pooled = FALSE, gc = gc)
  })

  # Candidate-model ranking (detection): all five candidate models, independent of
  # the Directionality control. NULL when there are no headings.
  model_sel <- reactive({
    ctx <- summary_ctx()
    if (!isTRUE(ctx$has_hd)) return(NULL)
    tryCatch(circ_model_select(ctx$hd, group_col = ctx$by_col),
             error = function(e) NULL)
  })

  output$summary_tbl <- renderTable({
    ctx <- summary_ctx()

    # No headings frame: trajectory Straightness-only, or nothing yet.
    if (!isTRUE(ctx$has_hd)) {
      if (identical(ctx$mode, "none_traj")) {
        req(rv$ts)
        st  <- straightness_index(rv$ts); idc <- rv$ts@cols$id; gc <- ctx$gc
        if (!is.null(gc)) {
          cond_map <- unique(as.data.frame(rv$ts)[, c(idc, gc), drop = FALSE])
          st  <- merge(st, cond_map, by = idc)
          agg <- tapply(st$straightness, as.character(st[[gc]]),
                        function(v) mean(v, na.rm = TRUE))
          return(data.frame(Group = names(agg),
                            Straightness = round(as.numeric(agg), 3),
                            stringsAsFactors = FALSE))
        }
        return(data.frame(Group = as.character(st[[idc]]),
                          Straightness = round(st$straightness, 3),
                          stringsAsFactors = FALSE))
      }
      req(rv$hd %||% rv$ts)
      return(data.frame(Note = "Summary not available"))
    }

    hd <- ctx$hd; by_col <- ctx$by_col
    tryCatch({
      cm <- circ_summary_table(hd, by_col, axial = is_axial(),
                               omnibus = input$omnibus_test %||% "rao",
                               model_sel = model_sel())
      if (identical(ctx$mode, "traj")) {
        st  <- straightness_index(rv$ts); idc <- rv$ts@cols$id
        key <- ctx$by_col
        if (!identical(key, "id")) {
          key_map <- unique(as.data.frame(rv$ts)[, c(idc, key), drop = FALSE])
          st  <- merge(st, key_map, by = idc)
          agg <- tapply(st$straightness, as.character(st[[key]]),
                        function(v) mean(v, na.rm = TRUE))
        } else {
          agg <- stats::setNames(st$straightness, as.character(st[[idc]]))
        }
        cm[["Straightness"]] <- round(as.numeric(agg[as.character(cm[["Group"]])]), 3)
      }
      if (isTRUE(ctx$pooled)) cm[["Group"]] <- NULL
      cm
    }, error = function(e) data.frame(Note = "Summary not available"))
  }, striped = TRUE, hover = TRUE, align = "c")

  output$model_sel_tbl <- renderTable({
    ms <- model_sel()
    if (is.null(ms) || nrow(ms) == 0L)
      return(data.frame(Note = "Model selection needs headings"))
    ctx <- summary_ctx()
    ms$AICc   <- round(ms$AICc, 1)
    ms$dAICc  <- round(ms$dAICc, 1)
    ms$weight <- round(ms$weight, 3)
    keep <- c(ctx$by_col, "model", "k", "AICc", "dAICc", "weight")
    ms <- ms[, intersect(keep, names(ms)), drop = FALSE]
    names(ms)[names(ms) == ctx$by_col] <- "Group"
    if (isTRUE(ctx$pooled)) ms[["Group"]] <- NULL
    ms
  }, striped = TRUE, hover = TRUE, align = "c")

  # The summary tables live in the "Summary & stats" sub-tab, which is not the
  # default Results tab. Without this, Shiny suspends their outputs (returning
  # NULL) until the user first clicks onto that tab. Keep them live so the
  # summary/model tables — and the CSV download that reads them — are populated
  # as soon as Results is reached, regardless of which sub-tab is in front.
  output$group_compare_tbl <- renderTable({
    ctx <- summary_ctx()
    no_group_note <- data.frame(
      Note = "Select a Group by column with 2+ groups to compare")

    if (!isTRUE(ctx$has_hd) || isTRUE(ctx$pooled)) return(no_group_note)

    hd <- ctx$hd; by_col <- ctx$by_col
    if (!by_col %in% names(hd)) return(no_group_note)
    grp_counts <- table(hd[[by_col]])
    if (sum(grp_counts >= 2L) < 2L) return(no_group_note)

    tryCatch(group_compare_table(hd, by_col, axial = is_axial()),
             error = function(e) data.frame(
               Note = paste("Group comparison error:", conditionMessage(e))))
  }, striped = TRUE, hover = TRUE, align = "c")
  outputOptions(output, "group_compare_tbl", suspendWhenHidden = FALSE)

  outputOptions(output, "summary_tbl",   suspendWhenHidden = FALSE)
  outputOptions(output, "model_sel_tbl", suspendWhenHidden = FALSE)

  # The radiatR script reproducing the current figure (shown, copied, downloaded).
  figure_code_text <- reactive({
    req(rv$ts %||% rv$hd)
    spec_to_code(current_spec())
  })
  output$figure_code <- renderText(figure_code_text())
  output$dl_code <- downloadHandler(
    filename = function() paste0("radiatR_figure_", Sys.Date(), ".R"),
    content  = function(file) {
      req(results_ok())
      writeLines(figure_code_text(), file)
    }
  )

  # The radiatR script reproducing the Summary & stats analysis.
  stats_code_text <- reactive({
    req(rv$hd %||% rv$ts)
    spec_to_stats_code(current_spec())
  })
  output$stats_code <- renderText(stats_code_text())
  output$dl_stats_code <- downloadHandler(
    filename = function() paste0("radiatR_stats_", Sys.Date(), ".R"),
    content  = function(file) {
      req(results_ok())
      writeLines(stats_code_text(), file)
    }
  )

  output$dl_plot <- downloadHandler(
    filename = function() {
      fmt <- if (is.null(input$plot_fmt)) "pdf" else input$plot_fmt
      paste0("radiatR_plot_", Sys.Date(), ".", fmt)
    },
    content = function(file) {
      req(results_ok())
      req(rv$ts %||% rv$hd)
      fmt <- if (is.null(input$plot_fmt)) "pdf" else input$plot_fmt
      # Transparent background applies to every format except JPG (no alpha).
      transparent <- isTRUE(input$plot_transparent) && fmt != "jpg"
      p <- build_results_plot()
      if (transparent) p <- p + .transparent_theme()
      ggsave(
        file, p,
        device = .plot_device(fmt),
        width  = num_or(input$plot_w, 7),
        height = num_or(input$plot_h, 7),
        units  = "in",
        dpi    = num_or(input$plot_dpi, 300),
        bg     = if (transparent) "transparent" else NULL
      )
    }
  )

  # Data-download button: relabel for None mode and use a clearer (non-ghosted)
  # outline-primary style matching the plot-download button.
  output$dl_csv_ui <- renderUI({
    lab <- if (is.null(rv$hd)) "Metrics (CSV)" else "Headings (CSV)"
    downloadButton("dl_csv", lab, class = "btn-sm btn-outline-primary w-100")
  })

  output$dl_csv <- downloadHandler(
    filename = function() {
      stem <- if (is.null(rv$hd)) "radiatR_metrics_" else "radiatR_headings_"
      paste0(stem, Sys.Date(), ".csv")
    },
    content = function(file) {
      req(results_ok())
      req(rv$ts %||% rv$hd)
      dat <- if (is.null(rv$hd)) path_metrics_table(rv$ts) else rv$hd
      utils::write.csv(dat, file, row.names = FALSE)
    }
  )

  # ---- Kinematics sub-tab ----------------------------------------------------

  # Ghost (hide) the Track metrics tab when only headings are loaded, since
  # track metrics require trajectory data. Show it once track data is present.
  observe({
    req(rv$step == 3L)
    if (is.null(rv$ts)) {
      nav_hide("results_tab", "Track metrics", session = session)
    } else {
      nav_show("results_tab", "Track metrics", select = FALSE, session = session)
    }
  })

  # Sync the Kinematics frame-rate widget into the single shared fps value.
  observeEvent(input$kin_frame_rate, {
    if (!fps_equal(input$kin_frame_rate, fps_rv())) {
      fps_rv(input$kin_frame_rate)
      fps_source_rv(if (.fps_is_set(input$kin_frame_rate)) "user" else "none")
    }
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  output$kin_track_ui <- renderUI({
    if (is.null(rv$ts)) return(NULL)
    tid <- as.character(ids(rv$ts))
    selectInput("kin_track", "Track",
                choices = c(stats::setNames(tid, tid),
                            "All tracks (overlay)" = ""),
                selected = tid[1])
  })

  # Colour-by selector: categorical columns of the loaded Tracks ("None" default).
  output$kin_colour_ui <- renderUI({
    cols <- if (is.null(rv$ts)) character(0) else colour_cols(rv$ts)
    selectInput("kin_colour_by", "Colour lines by",
                choices = c("None" = "", stats::setNames(cols, cols)),
                selected = "")
  })

  kinematics_spec <- reactive({
    req(rv$ts)
    ro <- upload_read_opts(rv$file_name, input$delim_sel, input$decimal_sel,
                           input$sheet_sel)
    build_kinematics_spec(rv$ts, list(
      kin_metric    = input$kin_metric,
      kin_units     = input$kin_units,
      kin_colour_by = input$kin_colour_by,
      kin_track     = input$kin_track,
      kin_smooth    = input$kin_smooth,
      kin_show_raw  = input$kin_show_raw,
      fps           = fps_rv(),
      data = list(
        source  = if (identical(rv$source, "example")) "example" else "file",
        mode    = "trajectories",
        path    = rv$file_name %||% "your_tracks.csv",
        dialect = if (is.null(rv$dialect) || rv$dialect %in% c("auto", "generic"))
          NULL else rv$dialect,
        normalize_xy = rv$normalize_xy,
        origin = rv$origin, radius = rv$radius, drop = rv$drop_used,
        delim = ro$delim, decimal = ro$decimal, sheet = ro$sheet)))
  })

  # fps is required only for numeric-frame Tracks; POSIXct time and numeric
  # seconds (a dialect that reads its time column as seconds) need none.
  kin_fps_ok <- reactive({
    if (is.null(rv$ts)) return(FALSE)
    if (inherits(as.data.frame(rv$ts)[[rv$ts@cols$time]], "POSIXct")) return(TRUE)
    if (identical(rv$ts@meta$time_unit, "seconds")) return(TRUE)
    .fps_is_set(fps_rv())
  })

  output$kin_note <- renderUI({
    if (is.null(rv$ts))
      return(div(class = "alert alert-secondary py-1 px-2 small mb-2",
                 "Kinematics needs trajectory data (load tracks, not headings)."))
    if (!isTRUE(kin_fps_ok()))
      return(div(class = "alert alert-warning py-1 px-2 small mb-2",
                 "Enter a positive frame rate to enable time/speed plots & kinematics."))
    if (identical(fps_source_rv(), "data") && .fps_is_set(fps_rv()))
      return(div(class = "alert alert-info py-1 px-2 small mb-2",
                 paste0("From data (", fps_rv(), " fps).")))
    NULL
  })

  output$kinematics_plot <- renderPlot({
    req(rv$ts)
    validate(need(isTRUE(kin_fps_ok()),
                  "Set a positive frame rate to plot kinematics."))
    kinematics_spec_to_plot(kinematics_spec(), rv$ts)
  })

  kinematics_code_text <- reactive({
    req(rv$ts)
    spec_to_kinematics_code(kinematics_spec())
  })
  output$kinematics_code <- renderText(kinematics_code_text())
  output$dl_kinematics_code <- downloadHandler(
    filename = function() paste0("radiatR_kinematics_", Sys.Date(), ".R"),
    content  = function(file) {
      req(results_ok())
      writeLines(kinematics_code_text(), file)
    }
  )

  # Image export for the kinematics profile, mirroring dl_plot for the figure.
  output$dl_kin_plot <- downloadHandler(
    filename = function() {
      fmt <- if (is.null(input$kin_plot_fmt)) "pdf" else input$kin_plot_fmt
      paste0("radiatR_kinematics_", Sys.Date(), ".", fmt)
    },
    content = function(file) {
      req(results_ok())
      req(rv$ts, isTRUE(kin_fps_ok()))
      fmt <- if (is.null(input$kin_plot_fmt)) "pdf" else input$kin_plot_fmt
      transparent <- isTRUE(input$kin_plot_transparent) && fmt != "jpg"
      p <- kinematics_spec_to_plot(kinematics_spec(), rv$ts)
      if (transparent) p <- p + .transparent_theme()
      ggsave(
        file, p,
        device = .plot_device(fmt),
        width  = num_or(input$kin_plot_w, 7),
        height = num_or(input$kin_plot_h, 7),
        units  = "in",
        dpi    = num_or(input$kin_plot_dpi, 300),
        bg     = if (transparent) "transparent" else NULL
      )
    }
  )

  # ---- Per-track metrics (length / straightness / tortuosity / sinuosity) -----
  # Geometric metrics for every loaded track (no frame rate needed). When a
  # heading method was used, some tracks may not have produced a heading, so the
  # scope control lets the user view all tracks or only the heading-producing
  # subset. The same metrics are downloadable.

  # ids of the tracks that produced a heading (NULL in no-headings mode).
  heading_ids <- reactive(if (is.null(rv$hd)) NULL else unique(as.character(rv$hd$id)))

  output$track_metric_scope_ui <- renderUI({
    req(rv$ts, !is.null(rv$hd))            # only relevant when headings exist
    radioButtons("track_metric_scope", NULL, inline = TRUE,
                 choices  = c("All loaded tracks"      = "all",
                              "Tracks with a heading"  = "heading"),
                 selected = "all")
  })

  track_metrics_tbl <- reactive({
    req(rv$ts)
    m   <- path_metrics_table(rv$ts)
    idc <- rv$ts@cols$id
    hid <- heading_ids()
    if (identical(input$track_metric_scope, "heading") && !is.null(hid))
      m <- m[as.character(m[[idc]]) %in% hid, , drop = FALSE]
    m
  })

  output$track_metrics_summary <- renderUI({
    req(rv$ts)
    m   <- track_metrics_tbl()
    hid <- heading_ids()
    fmean <- function(v) mean(v[is.finite(v)], na.rm = TRUE)
    counts <- if (is.null(hid))
      sprintf("%d tracks", nrow(path_metrics_table(rv$ts)))
    else
      sprintf("%d tracks loaded · %d produced a heading",
              nrow(path_metrics_table(rv$ts)), length(hid))
    div(class = "text-muted small mb-2",
        counts, br(),
        sprintf("Means (shown) — straightness %.2f · tortuosity %.2f · sinuosity %.2f",
                fmean(m$straightness), fmean(m$tortuosity), fmean(m$sinuosity)))
  })

  output$track_metrics_table <- renderTable(track_metrics_tbl(), digits = 3)

  output$dl_track_metrics <- downloadHandler(
    filename = function() paste0("radiatR_track_metrics_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(results_ok())
      req(rv$ts)
      utils::write.csv(track_metrics_tbl(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)

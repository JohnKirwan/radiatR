# Tool definitions for radiatR_tools(). Each tool checks its JSON arguments,
# maps them to one or more .agent_step()s, and wraps the result; evaluation,
# logging and the result envelope live in agent_runtime.R.

# Parameters each built-in heading rule reads from `...`. derive_headings()
# ignores unknown `...` silently, so the tool rejects a parameter the chosen
# rule does not use.
.agent_rule_params <- list(
  crossing        = c("circ0", "circ1", "direction", "first_only", "return_coords"),
  distal          = c("max_radius", "return_coords"),
  straight        = c("tol", "min_len", "return_coords"),
  origin_mean     = "r_power",
  net             = "return_coords",
  velocity_mean   = "weight_by",
  velocity_axis   = "weight_by",
  window_net      = c("window_n", "stride"),
  goal_bias       = character(0),
  pca_axis        = c("source", "return_coords"),
  ransac_straight = c("eps", "min_inliers", "n_iter"),
  maxspeed_window = "window_n",
  vm_fit          = "weight_by",
  exit            = "r0",
  entry           = "r0",
  ring_tangent    = c("r_star", "smooth_n", "deriv_span", "direction")
)

.agent_rule_param_types <- c(
  circ0 = "number", circ1 = "number", direction = "string",
  first_only = "boolean", return_coords = "boolean", max_radius = "number",
  tol = "number", min_len = "integer", r_power = "number",
  weight_by = "string", window_n = "integer", stride = "integer",
  source = "string", eps = "number", min_inliers = "integer",
  n_iter = "integer", r0 = "number", r_star = "number",
  smooth_n = "integer", deriv_span = "integer"
)

.agent_roles <- c("id", "time", "x", "y", "angle", "weight")
.agent_delim_readers <- c(comma = "read.csv", semicolon = "read.csv2", tab = "read.delim")
.agent_delim_seps <- c(comma = ",", semicolon = ";", tab = "\t")
.agent_compare_fns <- c(mean_direction = "test_mean_directions",
                        concentration = "test_concentration",
                        distribution = "test_distributions")
# Uniformity tests whose tabled p-value p_method = "monte_carlo" replaces, and
# those whose p-value is always simulated (see ?test_uniformity).
.agent_mc_tests <- c("kuiper", "rao", "watson")
.agent_sim_tests <- c("hermans_rasson", "pycke")

.agent_guide_ref <- "Conventions and pitfalls: vignette('agent-guide', package = 'radiatR')."

.agent_tool <- function(name, title, description, arguments, fun) {
  ellmer::tool(
    fun,
    name = paste0("radiatR_", name),
    description = paste(description, .agent_guide_ref),
    arguments = arguments,
    annotations = ellmer::tool_annotations(
      title = title, read_only_hint = TRUE, open_world_hint = FALSE)
  )
}

.agent_choices <- function(fn, arg, drop = character(0)) {
  setdiff(eval(formals(fn)[[arg]]), drop)
}

.agent_arg_seed <- function() ellmer::type_integer(
  "RNG seed (whole number). Always required so every result can be reproduced; it is recorded in `code`.")

.agent_seed_step <- function(seed) {
  .agent_step("set.seed", list(.agent_number(seed, "seed", required = TRUE,
                                             whole = TRUE)), ns = "base")
}

# ---- discovery / session -----------------------------------------------------
.tool_list_options <- function(store) {
  .agent_tool(
    "list_options", "List radiatR options",
    "List the loader dialects, heading rules (with the parameters each accepts) and statistical tests available. Call this before loading data or deriving headings.",
    list(),
    function() {
      .agent_envelope(result = list(
        loader_dialects = I(list_loader_dialects()),
        heading_rules = lapply(.agent_rule_params, I),
        uniformity_tests = I(.agent_choices(test_uniformity, "test")),
        compare_groups = list(
          mean_direction = I(.agent_choices(test_mean_directions, "method")),
          concentration = I(c("parametric = true", "parametric = false")),
          distribution = I(.agent_choices(test_distributions, "method")))
      ))
    })
}

.tool_describe <- function(store) {
  .agent_tool(
    "describe", "Describe a session object",
    "Describe one object held in this session (class, size, columns, first rows). With no handle, list every handle in the session.",
    list(handle = ellmer::type_string(
      "Handle returned by a load or derive tool, e.g. 'tracks_1'. Omit to list all handles.",
      required = FALSE)),
    function(handle = NULL) {
      if (is.null(handle)) {
        handles <- ls(store$objects)
        return(.agent_envelope(result = list(handles = lapply(
          stats::setNames(handles, handles),
          function(h) class(get(h, envir = store$objects))[1L]))))
      }
      obj <- .agent_get(store, handle)
      # A plain table is still in the file's own units; say nothing about them.
      .agent_envelope(
        result = .agent_describe(obj),
        units = if (inherits(obj, c("Tracks", "headings_frame"))) .agent_units())
    })
}

.tool_export_script <- function(store) {
  .agent_tool(
    "export_script", "Export the session as an R script",
    "Return one R script that reproduces every object and result produced in this session, in order. Save it next to the analysis so the work can be rerun without the agent.",
    list(),
    function() .agent_envelope(code = .agent_script(store)))
}

# ---- loading -----------------------------------------------------------------
# read_tracks() fills an unmapped angle or weight role from any column with a
# matching name (theta, angle, bearing, w, weight, ...), in the default unit.
# The tool never lets that pass: a weight role nobody mapped, or an angle that
# is not simply the direction of the stored position, means a column was
# guessed.
.agent_check_roles <- function(ts, mapping) {
  if (is.null(mapping$weight) && !is.null(ts@cols$weight))
    rlang::abort(
      sprintf("read_tracks() picked up column '%s' as the weight, but no weight column was mapped. If it holds weights, name it in `mapping.weight`; otherwise rename or remove it before loading (read_tracks() cannot be told to ignore it).",
              ts@cols$weight),
      class = "radiatR_guessed_column", role = "weight", column = ts@cols$weight)
  if (is.null(mapping$angle)) {
    d <- ts@data
    from_xy <- cartesian_to_polar(d[[ts@cols$x]], d[[ts@cols$y]],
                                  normalize = FALSE)$theta
    if (!identical(d[[ts@cols$angle]], from_xy))
      rlang::abort(
        sprintf("read_tracks() picked up column '%s' as the angle, but no angle column was mapped, so its unit was never stated. If it holds the heading angle, name it in `mapping.angle` and give `angle_unit`; otherwise rename or remove it before loading (read_tracks() cannot be told to ignore it).",
                ts@cols$angle),
        class = "radiatR_guessed_column", role = "angle", column = ts@cols$angle)
  }
}

.tool_load_tracks <- function(store) {
  time_types <- .agent_choices(read_tracks, "time_type", drop = "auto")
  angle_units <- .agent_choices(read_tracks, "angle_unit", drop = "auto")
  collisions <- .agent_choices(read_tracks, "id_collision")
  invalids <- .agent_choices(read_tracks, "on_invalid")
  .agent_tool(
    "load_tracks", "Load trajectory files",
    "Load one or more tracking files into a Tracks object with radiatR::read_tracks() and return its handle. Give either a registered `dialect` or a `mapping` naming at least the id, time, x and y columns. Units are never guessed: `time_type` is always required, and `angle_unit` is required when an angle column is mapped. Without a dialect, columns that are not mapped are dropped unless named in `keep` (list every grouping column you will need later).",
    list(
      paths = ellmer::type_array(ellmer::type_string(), "File paths to load."),
      time_type = ellmer::type_enum(time_types,
        "How the time column is expressed. 'frames' also needs `fps`."),
      mapping = ellmer::type_object(
        "Column names in the file for each role. Omit when using a dialect.",
        id = ellmer::type_string("Trajectory id column.", required = FALSE),
        time = ellmer::type_string("Time or frame column.", required = FALSE),
        x = ellmer::type_string("x coordinate column.", required = FALSE),
        y = ellmer::type_string("y coordinate column.", required = FALSE),
        angle = ellmer::type_string("Angle column, if the file has one. It must already be in the unit-circle convention (0 = East, counter-clockwise): read_tracks() has no convention argument, so a compass bearing would be misread.", required = FALSE),
        weight = ellmer::type_string("Weight column, if any.", required = FALSE),
        .required = FALSE),
      dialect = ellmer::type_enum(list_loader_dialects(),
        "Registered tracker dialect that reads the file itself.", required = FALSE),
      angle_unit = ellmer::type_enum(angle_units,
        "Unit of the mapped angle column.", required = FALSE),
      fps = ellmer::type_number(
        "Capture frame rate in frames per second. Only with time_type 'frames'.",
        required = FALSE),
      normalize_xy = ellmer::type_boolean(
        "TRUE rescales each trajectory to the unit circle on its own bounding box. This preserves shape only and is NOT calibration. Default FALSE.",
        required = FALSE),
      origin_x = ellmer::type_number("Arena centre x in file units (calibration; give with origin_y and radius).", required = FALSE),
      origin_y = ellmer::type_number("Arena centre y in file units.", required = FALSE),
      radius = ellmer::type_number("Arena radius in file units.", required = FALSE),
      keep = ellmer::type_array(ellmer::type_string(),
        "Extra, unmapped columns to retain (e.g. treatment or condition columns).", required = FALSE),
      id_collision = ellmer::type_enum(collisions,
        "What to do when the same id appears in more than one file. Default 'error'.", required = FALSE),
      on_invalid = ellmer::type_enum(invalids,
        "What to do with rows whose x/y are missing or non-finite. Default 'error'; 'drop' removes them and reports it under `warnings`.", required = FALSE)
    ),
    function(paths = NULL, time_type = NULL, mapping = NULL, dialect = NULL,
             angle_unit = NULL, fps = NULL, normalize_xy = NULL,
             origin_x = NULL, origin_y = NULL, radius = NULL, keep = NULL,
             id_collision = NULL, on_invalid = NULL) {
      paths <- .agent_paths(paths)
      time_type <- .agent_string(time_type, "time_type", time_types)
      dialect <- .agent_string(dialect, "dialect", list_loader_dialects(),
                               required = FALSE)
      mapping <- .agent_object(mapping, "mapping", .agent_roles)
      for (role in names(mapping))
        .agent_string(mapping[[role]], paste0("mapping.", role))
      if (is.null(dialect)) {
        unmapped <- setdiff(c("id", "time", "x", "y"), names(mapping))
        if (length(unmapped))
          .agent_missing(
            sprintf("mapping.%s", paste(unmapped, collapse = ", mapping.")),
            "when no dialect is given (read_tracks() would otherwise guess the column)")
      }
      if (is.null(mapping$angle))
        .agent_unused(angle_unit, "angle_unit", "an angle column is mapped")
      angle_unit <- .agent_string(angle_unit, "angle_unit", angle_units,
                                  required = !is.null(mapping$angle),
                                  why = "when an angle column is mapped")
      fps <- .agent_number(fps, "fps")
      if (time_type != "frames")
        .agent_unused(fps, "fps", "time_type is 'frames'")
      origin_x <- .agent_number(origin_x, "origin_x")
      origin_y <- .agent_number(origin_y, "origin_y")
      if (is.null(origin_x) != is.null(origin_y))
        .agent_invalid("origin_x, origin_y", "given together or not at all")
      keep <- .agent_strings(keep, "keep")

      handle <- .agent_next_handle(store, "tracks")
      run <- .agent_run(store, "radiatR_load_tracks", list(.agent_step(
        "read_tracks",
        .agent_args(paths,
                    mapping = if (length(mapping)) mapping,
                    dialect = dialect, angle_unit = angle_unit,
                    time_type = time_type, fps = fps,
                    normalize_xy = .agent_flag(normalize_xy, "normalize_xy"),
                    origin = if (!is.null(origin_x)) c(origin_x, origin_y),
                    radius = .agent_number(radius, "radius"),
                    keep = keep,
                    id_collision = .agent_string(id_collision, "id_collision",
                                                 collisions, required = FALSE),
                    on_invalid = .agent_string(on_invalid, "on_invalid",
                                               invalids, required = FALSE)),
        creates = handle,
        check = function(ts) {
          .agent_columns(keep, names(ts@data), "keep")
          .agent_check_roles(ts, mapping)
        })))
      .agent_envelope(run, handle = handle,
                      result = .agent_describe(run$value),
                      units = .agent_units())
    })
}

# read.csv() reshapes a file whose records differ in length without a word: a
# longer data row shifts the columns (the first becomes row names) or starts a
# new row, and a shorter one is padded. Refuse such a file instead.
.agent_check_rectangular <- function(path, sep) {
  n <- utils::count.fields(path, sep = sep, quote = "\"", comment.char = "",
                           blank.lines.skip = TRUE)
  n <- n[!is.na(n)]   # NA marks the inner lines of a quoted multi-line field
  odd <- which(n != n[1L])
  if (length(odd))
    rlang::abort(
      sprintf("'%s' is not a rectangular table: the header has %d field(s) but record(s) %s have %s (records are non-blank lines; the header is record 1). Nothing was loaded.",
              path, n[1L], paste(utils::head(odd, 10L), collapse = ", "),
              paste(unique(n[odd]), collapse = ", ")),
      class = "radiatR_ragged_file", path = path, records = odd)
}

.tool_load_headings <- function(store) {
  .agent_tool(
    "load_headings", "Load a table of headings",
    "Load a delimited text file that already holds one heading angle per row and return a headings handle. The delimiter, the angle column, its unit and its convention must all be stated; nothing is guessed. All other columns are kept, under their names as written in the file, and can be used for grouping.",
    list(
      path = ellmer::type_string("Path to a .csv, .tsv or .txt file."),
      delim = ellmer::type_enum(names(.agent_delim_readers),
        "Field delimiter. 'semicolon' also means decimal commas (read.csv2)."),
      col = ellmer::type_string("Name of the angle column."),
      units = ellmer::type_enum(c("radians", "degrees"), "Unit of the angle column."),
      angle_convention = ellmer::type_enum(c("unit_circle", "clock"),
        "'unit_circle': 0 = East, counter-clockwise. 'clock': 0 = North, clockwise (compass bearings).")
    ),
    function(path = NULL, delim = NULL, col = NULL, units = NULL,
             angle_convention = NULL) {
      path <- .agent_paths(.agent_string(path, "path"), "path")
      ext <- tolower(tools::file_ext(path))
      if (!ext %in% c("csv", "tsv", "txt"))
        rlang::abort(
          sprintf("Unsupported file type '.%s' for '%s'. Headings tables must be delimited text (.csv, .tsv, .txt).", ext, path),
          class = "radiatR_unsupported_file", path = path)
      delim <- .agent_string(delim, "delim", names(.agent_delim_readers))
      col <- .agent_string(col, "col")
      units <- .agent_string(units, "units", c("radians", "degrees"))
      angle_convention <- .agent_string(angle_convention, "angle_convention",
                                        c("unit_circle", "clock"))
      df <- .agent_next_handle(store, "df")
      handle <- .agent_next_handle(store, "headings")
      run <- .agent_run(store, "radiatR_load_headings", list(
        # check.names = FALSE: column names stay exactly as written in the file.
        .agent_step(.agent_delim_readers[[delim]],
                    list(path, check.names = FALSE), ns = "utils", creates = df,
                    check = function(table) {
                      .agent_check_rectangular(path, .agent_delim_seps[[delim]])
                      .agent_columns(col, names(table), "col")
                      if (!is.numeric(table[[col]]))
                        .agent_invalid("col", sprintf(
                          "a numeric column ('%s' was read as %s)", col,
                          class(table[[col]])[1L]))
                    }),
        .agent_step("headings_frame",
                    list(rlang::sym(df), col = col, units = units,
                         angle_convention = angle_convention),
                    creates = handle)))
      .agent_envelope(run, handle = handle,
                      result = .agent_describe(run$value),
                      units = .agent_units())
    })
}

.tool_derive_headings <- function(store) {
  rule_props <- lapply(names(.agent_rule_param_types), function(p) {
    type <- switch(.agent_rule_param_types[[p]],
                   number = ellmer::type_number, integer = ellmer::type_integer,
                   string = ellmer::type_string, boolean = ellmer::type_boolean)
    type(sprintf("Rule parameter `%s`; see ?radiatR::derive_headings.", p),
         required = FALSE)
  })
  names(rule_props) <- names(.agent_rule_param_types)
  .agent_tool(
    "derive_headings", "Derive one heading per track",
    "Derive heading angles from a Tracks handle with radiatR::derive_headings() and return a headings handle. Radii in rule parameters are in the same units as the track coordinates (1 = arena edge only if the tracks were calibrated or normalised). Tracks for which the rule yields no heading are kept as NA and reported; such attrition is often non-random. Use `carry` to bring grouping columns across. radiatR_list_options shows which parameters each rule accepts.",
    list(
      handle = ellmer::type_string("Tracks handle, e.g. 'tracks_1'."),
      rule = ellmer::type_enum(names(.agent_rule_params), "Heading rule."),
      seed = .agent_arg_seed(),
      params = do.call(ellmer::type_object, c(
        list(.description = "Parameters for the chosen rule. A parameter the rule does not use is an error."),
        rule_props, list(.required = FALSE))),
      carry = ellmer::type_array(ellmer::type_string(),
        "Track columns to copy onto the headings (e.g. grouping columns).", required = FALSE),
      coords = ellmer::type_enum(c("absolute", "relative"),
        "Coordinate frame. Default 'absolute'.", required = FALSE),
      on_missing = ellmer::type_enum(c("warn", "error", "quiet"),
        "What to do when a track yields no heading. Default 'warn' (reported under `warnings`).", required = FALSE)
    ),
    function(handle = NULL, rule = NULL, seed = NULL, params = NULL,
             carry = NULL, coords = NULL, on_missing = NULL) {
      ts <- .agent_get(store, handle, "Tracks")
      rule <- .agent_string(rule, "rule", names(.agent_rule_params))
      params <- .agent_object(params, "params", .agent_rule_params[[rule]])
      for (p in names(params)) {
        arg <- paste0("params.", p)
        params[[p]] <- switch(
          .agent_rule_param_types[[p]],
          number = .agent_number(params[[p]], arg),
          integer = .agent_number(params[[p]], arg, whole = TRUE),
          string = .agent_string(params[[p]], arg),
          boolean = .agent_flag(params[[p]], arg))
      }
      carry <- .agent_columns(.agent_strings(carry, "carry"),
                              names(as.data.frame(ts)), "carry")
      out <- .agent_next_handle(store, "headings")
      run <- .agent_run(store, "radiatR_derive_headings", list(
        .agent_seed_step(seed),
        .agent_step(
          "derive_headings",
          c(list(rlang::sym(handle), rule = rule), params,
            .agent_args(
              carry = carry,
              coords = .agent_string(coords, "coords",
                                     c("absolute", "relative"), required = FALSE),
              on_missing = .agent_string(on_missing, "on_missing",
                                         c("warn", "error", "quiet"),
                                         required = FALSE))),
          creates = out)))
      hd <- run$value
      # n_total and n_missing count result rows, which exceed the number of
      # tracks for rules that return several headings per track.
      .agent_envelope(
        run, handle = out,
        result = c(.agent_describe(hd),
                   list(n_total = attr(hd, "n_total", exact = TRUE),
                        n_missing = attr(hd, "n_missing", exact = TRUE))),
        units = .agent_units())
    })
}

# ---- analysis ----------------------------------------------------------------
.agent_headings <- function(store, handle) {
  hd <- .agent_get(store, handle, "headings_frame")
  list(data = hd, col = attr(hd, "heading_col", exact = TRUE))
}

.tool_summarise <- function(store) {
  .agent_tool(
    "summarise", "Circular summary statistics",
    "Summarise a headings handle with radiatR::circ_summarise(): n, mean direction, resultant length and concentration, optionally per group. `mean_dir` is in radians; `mean_dir_deg` is in the display convention reported under `units`, which is NOT a plain radians-to-degrees conversion.",
    list(
      handle = ellmer::type_string("Headings handle, e.g. 'headings_1'."),
      by = ellmer::type_array(ellmer::type_string(), "Grouping column(s).", required = FALSE),
      axial = ellmer::type_boolean("TRUE for axial (bidirectional) data. Default FALSE.", required = FALSE)
    ),
    function(handle = NULL, by = NULL, axial = NULL) {
      hd <- .agent_headings(store, handle)
      by <- .agent_columns(.agent_strings(by, "by"), names(hd$data), "by")
      run <- .agent_run(store, "radiatR_summarise", list(.agent_step(
        "circ_summarise",
        c(list(rlang::sym(handle), col = hd$col, units = "radians"),
          .agent_args(.by = by, axial = .agent_flag(axial, "axial"))))))
      .agent_envelope(run, result = as.data.frame(run$value),
                      units = .agent_units(circ_display()))
    })
}

.tool_test_uniformity <- function(store) {
  tests <- .agent_choices(test_uniformity, "test")
  p_methods <- .agent_choices(test_uniformity, "p_method")
  .agent_tool(
    "test_uniformity", "Test for uniformity",
    "Test whether headings are uniformly distributed around the circle with radiatR::test_uniformity(), optionally per group. An option the chosen test does not use is an error, not ignored.",
    list(
      handle = ellmer::type_string("Headings handle, e.g. 'headings_1'."),
      seed = .agent_arg_seed(),
      test = ellmer::type_enum(tests, "Test to run. Default 'rayleigh'.", required = FALSE),
      group_col = ellmer::type_string("Column to test within, one result row per group.", required = FALSE),
      axial = ellmer::type_boolean("TRUE for axial (bidirectional) data.", required = FALSE),
      p_method = ellmer::type_enum(p_methods, "Tests 'kuiper', 'rao' and 'watson' only: tabled significance level ('table', the default) or a simulated p-value ('monte_carlo').", required = FALSE),
      n_sim = ellmer::type_integer("Monte Carlo simulations, for p_method 'monte_carlo' and for tests 'hermans_rasson' and 'pycke' only. Default 9999.", required = FALSE),
      p_adjust = ellmer::type_enum(stats::p.adjust.methods, "Multiple-comparison adjustment across groups; only with group_col. Default 'none'.", required = FALSE),
      mu = ellmer::type_number("Expected direction in radians, for test 'vtest' only.", required = FALSE)
    ),
    function(handle = NULL, seed = NULL, test = NULL, group_col = NULL,
             axial = NULL, p_method = NULL, n_sim = NULL, p_adjust = NULL,
             mu = NULL) {
      hd <- .agent_headings(store, handle)
      test <- .agent_string(test, "test", tests, required = FALSE)
      mu <- .agent_number(mu, "mu")
      if (!identical(test, "vtest")) .agent_unused(mu, "mu", "test is 'vtest'")
      group_col <- .agent_string(group_col, "group_col", required = FALSE)
      .agent_columns(group_col, names(hd$data), "group_col")
      if (!isTRUE(test %in% .agent_mc_tests))
        .agent_unused(p_method, "p_method", sprintf(
          "test is one of %s", paste0("'", .agent_mc_tests, "'", collapse = ", ")))
      p_method <- .agent_string(p_method, "p_method", p_methods, required = FALSE)
      if (!identical(p_method, "monte_carlo") && !isTRUE(test %in% .agent_sim_tests))
        .agent_unused(n_sim, "n_sim", sprintf(
          "p_method is 'monte_carlo' or test is one of %s",
          paste0("'", .agent_sim_tests, "'", collapse = ", ")))
      if (is.null(group_col))
        .agent_unused(p_adjust, "p_adjust", "group_col is given")
      run <- .agent_run(store, "radiatR_test_uniformity", list(
        .agent_seed_step(seed),
        .agent_step(
          "test_uniformity",
          c(list(rlang::sym(handle)),
            .agent_args(
              group_col = group_col, angle_col = hd$col, test = test,
              p_adjust = .agent_string(p_adjust, "p_adjust",
                                       stats::p.adjust.methods, required = FALSE),
              axial = .agent_flag(axial, "axial"),
              n_sim = .agent_number(n_sim, "n_sim", whole = TRUE, min = 1),
              mu = mu, p_method = p_method)))))
      .agent_envelope(run, result = as.data.frame(run$value),
                      units = .agent_units())
    })
}

.tool_compare_groups <- function(store) {
  .agent_tool(
    "compare_groups", "Compare groups",
    "Compare groups of headings: 'mean_direction' (radiatR::test_mean_directions), 'concentration' (radiatR::test_concentration) or 'distribution' (radiatR::test_distributions). An argument the chosen aspect or method does not use is an error, not ignored. radiatR_list_options lists the methods for each aspect.",
    list(
      handle = ellmer::type_string("Headings handle, e.g. 'headings_1'."),
      aspect = ellmer::type_enum(names(.agent_compare_fns), "What to compare."),
      group_col = ellmer::type_string("Grouping column."),
      seed = .agent_arg_seed(),
      method = ellmer::type_string("Method for the aspect (mean_direction, distribution), named in full.", required = FALSE),
      parametric = ellmer::type_boolean("Concentration only: parametric test (TRUE) or not.", required = FALSE),
      pairwise = ellmer::type_boolean("Also run pairwise comparisons (mean_direction, distribution).", required = FALSE),
      p_adjust = ellmer::type_enum(stats::p.adjust.methods, "Adjustment for pairwise p-values; only with pairwise true.", required = FALSE),
      axial = ellmer::type_boolean("TRUE for axial (bidirectional) data.", required = FALSE),
      p_method = ellmer::type_enum(c("table", "monte_carlo"), "Distribution with method 'watson_two' only: tabled band or permutation p-value.", required = FALSE),
      n_perm = ellmer::type_integer("Label permutations, for method 'permutation' or p_method 'monte_carlo' only. Default 9999.", required = FALSE)
    ),
    function(handle = NULL, aspect = NULL, group_col = NULL, seed = NULL,
             method = NULL, parametric = NULL, pairwise = NULL, p_adjust = NULL,
             axial = NULL, p_method = NULL, n_perm = NULL) {
      hd <- .agent_headings(store, handle)
      aspect <- .agent_string(aspect, "aspect", names(.agent_compare_fns))
      group_col <- .agent_columns(.agent_string(group_col, "group_col"),
                                  names(hd$data), "group_col")
      fn <- get(.agent_compare_fns[[aspect]], mode = "function")
      given <- .agent_compact(list(
        method = method, parametric = parametric, pairwise = pairwise,
        p_adjust = p_adjust, axial = axial, p_method = p_method, n_perm = n_perm))
      for (arg in setdiff(names(given), names(formals(fn))))
        .agent_unused(given[[arg]], arg, sprintf(
          "the aspect takes it ('%s' does not)", aspect))
      if ("method" %in% names(formals(fn)))
        method <- .agent_string(method, "method", .agent_choices(fn, "method"),
                                required = FALSE)
      pairwise <- .agent_flag(pairwise, "pairwise")
      if (!isTRUE(pairwise)) .agent_unused(p_adjust, "p_adjust", "pairwise is true")
      if (aspect == "distribution" && !identical(method, "watson_two"))
        .agent_unused(p_method, "p_method", "method is 'watson_two'")
      p_method <- .agent_string(p_method, "p_method", c("table", "monte_carlo"),
                                required = FALSE)
      if (!identical(method, "permutation") && !identical(p_method, "monte_carlo"))
        .agent_unused(n_perm, "n_perm",
                      "method is 'permutation' or p_method is 'monte_carlo'")
      run <- .agent_run(store, "radiatR_compare_groups", list(
        .agent_seed_step(seed),
        .agent_step(
          .agent_compare_fns[[aspect]],
          c(list(rlang::sym(handle), group_col = group_col, angle_col = hd$col),
            .agent_args(
              method = method,
              parametric = .agent_flag(parametric, "parametric"),
              pairwise = pairwise,
              p_adjust = .agent_string(p_adjust, "p_adjust",
                                       stats::p.adjust.methods, required = FALSE),
              axial = .agent_flag(axial, "axial"),
              p_method = p_method,
              n_perm = .agent_number(n_perm, "n_perm", whole = TRUE, min = 1))))))
      .agent_envelope(run, result = as.data.frame(run$value),
                      units = .agent_units())
    })
}

# ---- plot --------------------------------------------------------------------
.tool_plot <- function(store) {
  .agent_tool(
    "plot", "Plot tracks or headings",
    "Draw a Tracks or headings handle on the unit circle with radiatR::radiate() and return the image. For any further styling, edit the code this tool returns.",
    list(
      handle = ellmer::type_string("Tracks or headings handle."),
      facets = ellmer::type_array(ellmer::type_string(), "Column(s) to facet by.", required = FALSE)
    ),
    function(handle = NULL, facets = NULL) {
      obj <- .agent_get(store, handle, c("Tracks", "headings_frame"))
      facets <- .agent_columns(.agent_strings(facets, "facets"),
                               names(as.data.frame(obj)), "facets")
      file <- tempfile(fileext = ".png")
      on.exit(unlink(file), add = TRUE)
      # Rendering is the step's check, so a plot that cannot be drawn fails the
      # call as a whole and nothing is logged.
      run <- .agent_run(store, "radiatR_plot", list(.agent_step(
        "radiate",
        c(list(rlang::sym(handle)), .agent_args(facets = facets)),
        check = function(plot)
          ggplot2::ggsave(file, plot, width = 6, height = 6, dpi = 120))))
      # resize = "none": any other value makes ellmer require the magick package.
      list(ellmer::ContentText(as.character(.agent_envelope(run))),
           ellmer::content_image_file(file, resize = "none"))
    })
}

# ---- public ------------------------------------------------------------------
#' Tools for driving radiatR from an AI agent
#'
#' Returns a set of [ellmer::tool()] definitions that let a language model run
#' a radiatR analysis: load tracks or a table of headings, derive headings,
#' summarise, test and plot. Register them with an ellmer chat, or serve them
#' to an MCP client such as Claude Code or Claude Desktop with the
#' \pkg{mcptools} package.
#'
#' Each call to `radiatR_tools()` creates its own private session. Objects are
#' referred to by handles (`"tracks_1"`, `"headings_1"`); every result includes
#' the R code that produced it, and the `radiatR_export_script` tool returns
#' the whole session as one script that reproduces the data objects and result
#' tables exactly.
#'
#' The tools do not fill in what was left unsaid: the time type, the unit of
#' any angle column, the convention of a table of angles, and a seed for
#' heading derivation and for every test must be stated. A missing or invalid
#' argument is an error rather than a default, and so is an argument the call
#' would ignore. One thing is still detected rather than stated: `load_tracks`
#' reads a file as [read_tracks()] does, sniffing its delimiter and decimal
#' mark. Warnings and printed
#' output are returned to the model in the result instead of being written to
#' the console, and an error message carries the condition class, the failing
#' call, and anything raised before the failure.
#'
#' @section Serving over MCP:
#' ```
#' claude mcp add radiatR -- Rscript -e \
#'   'mcptools::mcp_server(tools = radiatR::radiatR_tools(), session_tools = FALSE)'
#' ```
#' `session_tools = FALSE` keeps every call in the server process, where the
#' handles live. Handles therefore cannot be inspected from another R console;
#' the exported script is the bridge. Anything R prints at startup corrupts the
#' MCP stream: in a project that uses renv, set
#' `RENV_CONFIG_SYNCHRONIZED_CHECK=FALSE` for the server.
#'
#' @return A named list of `ellmer` tool definitions.
#' @examplesIf rlang::is_installed("ellmer", version = "0.5.0") && rlang::is_installed("jsonlite")
#' tools <- radiatR_tools()
#' names(tools)
#' @md
#' @export
radiatR_tools <- function() {
  rlang::check_installed(c("ellmer (>= 0.5.0)", "jsonlite"),
                         reason = "to build agent tools.")
  store <- .agent_store()
  builders <- list(
    list_options = .tool_list_options, load_tracks = .tool_load_tracks,
    load_headings = .tool_load_headings, derive_headings = .tool_derive_headings,
    describe = .tool_describe, summarise = .tool_summarise,
    test_uniformity = .tool_test_uniformity, compare_groups = .tool_compare_groups,
    plot = .tool_plot, export_script = .tool_export_script)
  lapply(builders, function(build) build(store))
}

skip_on_cran()   # about 20 s: every test loads, derives or resamples
skip_if_not_installed("ellmer", "0.5.0")
skip_if_not_installed("jsonlite")

agent_json <- function(x) jsonlite::fromJSON(x, simplifyVector = FALSE)

# A small simulated tracking export on disk, removed with the calling test.
agent_fixture <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  sim <- as.data.frame(simulate_tracks(seed = 1, n_points = 50))
  path <- file.path(dir, "sim.csv")
  utils::write.csv(sim, path, row.names = FALSE)
  list(dir = dir, path = path, sim = sim,
       mapping = list(id = "trial_id", time = "frame", x = "abs_x", y = "abs_y"))
}

agent_store_of <- function(tools) environment(tools$describe)$store

agent_load <- function(tools, fx, ...) {
  agent_json(tools$load_tracks(paths = fx$path, time_type = "frames", fps = 30,
                               mapping = fx$mapping, keep = "condition", ...))
}

# Call a tool the way mcptools' MCP server does: parse the JSON arguments,
# flatten top-level arrays, and call the function. No schema is applied, so a
# JSON null reaches the tool as NULL.
agent_mcp_call <- function(tool, json) {
  args <- lapply(jsonlite::parse_json(json), function(x) {
    if (is.list(x) && is.null(names(x))) unlist(x, use.names = FALSE) else x
  })
  do.call(tool, args)
}

agent_write <- function(df, dir, name) {
  path <- file.path(dir, name)
  utils::write.csv(df, path, row.names = FALSE)
  path
}

test_that("radiatR_tools returns uniquely named ellmer tools", {
  tools <- radiatR_tools()
  expect_true(all(vapply(tools, inherits, logical(1), "ellmer::ToolDef")))
  own <- vapply(tools, function(tool) tool@name, character(1))
  expect_identical(unname(own), paste0("radiatR_", names(tools)))
  expect_identical(anyDuplicated(own), 0L)
  expect_true(all(c("list_options", "load_tracks", "load_headings", "describe",
                    "export_script") %in% names(tools)))
})

test_that("the heading-rule table covers exactly the built-in rules", {
  expect_setequal(names(.agent_rule_params), names(.builtin_rule_options))
  # The tools add only derive_headings() formals and return_coords.
  for (r in names(.builtin_rule_options))
    expect_setequal(setdiff(.agent_rule_params[[r]], c("first_only", "return_coords")),
                    .builtin_rule_options[[r]])
  expect_setequal(unlist(.agent_rule_params, use.names = FALSE),
                  names(.agent_rule_param_types))
})

test_that("list_options reports dialects, rules and tests", {
  opts <- agent_json(radiatR_tools()$list_options())$result
  expect_true("deeplabcut" %in% unlist(opts$loader_dialects))
  expect_identical(unlist(opts$heading_rules$distal),
                   c("max_radius", "return_coords"))
  expect_true("rayleigh" %in% unlist(opts$uniformity_tests))
})

test_that("load_tracks returns a handle, a description, units and the code", {
  fx <- agent_fixture()
  tools <- radiatR_tools()
  res <- agent_load(tools, fx)

  expect_identical(res$handle, "tracks_1")
  expect_identical(res$result$n_tracks, 60L)
  expect_true("condition" %in% unlist(res$result$data_columns))
  expect_identical(res$result$meta$frame_rate, 30L)
  expect_identical(res$units, list(angles = .agent_angle_units))
  expect_identical(res$warnings, list())
  expect_match(res$code, "^tracks_1 <- radiatR::read_tracks\\(")
  expect_match(res$code, normalizePath(fx$path, winslash = "/"), fixed = TRUE)
  expect_s4_class(agent_store_of(tools)$objects$tracks_1, "Tracks")
})

test_that("load_tracks never defaults a unit or guesses a mapping", {
  fx <- agent_fixture()
  tools <- radiatR_tools()
  load <- function(...) tools$load_tracks(paths = fx$path, ...)

  expect_error(load(fps = 30, mapping = fx$mapping),
               class = "radiatR_missing_argument")              # no time_type
  expect_error(load(time_type = "auto", fps = 30, mapping = fx$mapping),
               class = "radiatR_invalid_argument")              # no guessing
  expect_error(load(time_type = "frames", fps = 30,
                    mapping = c(fx$mapping, list(angle = "abs_theta"))),
               class = "radiatR_missing_argument")              # no angle_unit
  expect_error(load(time_type = "frames", fps = 30,
                    mapping = fx$mapping[c("id", "x", "y")]),
               class = "radiatR_missing_argument")              # time not mapped
  expect_error(load(time_type = "frames", fps = 30, mapping = fx$mapping,
                    origin_x = 0),
               class = "radiatR_invalid_argument")              # half an origin
  expect_error(load(time_type = "seconds", fps = 30, mapping = fx$mapping),
               class = "radiatR_invalid_argument")              # fps would be ignored
  expect_error(load(time_type = "frames", fps = 30, mapping = fx$mapping,
                    angle_unit = "degrees"),
               class = "radiatR_invalid_argument")       # no angle column is mapped
  expect_identical(ls(agent_store_of(tools)$objects), character(0))
})

test_that("a JSON null for a required argument is an error on the MCP path", {
  fx <- agent_fixture()
  tools <- radiatR_tools()
  args <- function(time_type) sprintf(
    '{"paths":["%s"],"time_type":%s,"fps":30,"keep":["condition"],
      "mapping":{"id":"trial_id","time":"frame","x":"abs_x","y":"abs_y"}}',
    normalizePath(fx$path, winslash = "/"), time_type)

  expect_error(agent_mcp_call(tools$load_tracks, args("null")),
               class = "radiatR_missing_argument")
  res <- agent_json(agent_mcp_call(tools$load_tracks, args('"frames"')))
  expect_identical(res$handle, "tracks_1")
  expect_true("condition" %in% names(agent_store_of(tools)$objects$tracks_1@data))
})

test_that("a column that read_tracks() would guess as angle or weight is refused", {
  fx <- agent_fixture()
  tools <- radiatR_tools()
  load <- function(path, mapping = fx$mapping, ...) tools$load_tracks(
    paths = path, time_type = "frames", fps = 30, mapping = mapping, ...)

  with_theta <- agent_write(cbind(fx$sim, theta = 123), fx$dir, "theta.csv")
  err <- expect_error(load(with_theta), class = "radiatR_guessed_column")
  expect_match(conditionMessage(err), "picked up column 'theta' as the angle",
               fixed = TRUE)
  with_w <- agent_write(cbind(fx$sim, w = 1), fx$dir, "w.csv")
  expect_error(load(with_w), class = "radiatR_guessed_column")
  expect_identical(ls(agent_store_of(tools)$objects), character(0))

  # Mapped explicitly, with its unit, the same file loads.
  res <- agent_json(load(with_theta, mapping = c(fx$mapping, list(angle = "theta")),
                         angle_unit = "degrees"))
  expect_identical(res$result$mapped_columns$angle, "theta")
})

test_that("the guessed-column check also runs, without false alarm, under a dialect", {
  tools <- radiatR_tools()
  res <- agent_json(tools$load_tracks(
    paths = test_path("fixtures", "real", "trackmate_v7_spots.csv"),
    dialect = "trackmate", time_type = "seconds"))
  expect_identical(res$result$n_tracks, 2L)
  expect_null(res$result$mapped_columns$weight)
})

test_that("load_tracks rejects a keep column that is not in the data", {
  fx <- agent_fixture()
  tools <- radiatR_tools()
  err <- expect_error(
    tools$load_tracks(paths = fx$path, time_type = "frames", fps = 30,
                      mapping = fx$mapping, keep = c("condition", "conditon")),
    class = "radiatR_invalid_argument")
  expect_match(conditionMessage(err), "not in the data: conditon", fixed = TRUE)
  expect_identical(ls(agent_store_of(tools)$objects), character(0))
})

test_that("calibrated tracks load and the calibration is described", {
  fx <- agent_fixture()
  tools <- radiatR_tools()
  res <- agent_json(tools$load_tracks(
    paths = fx$path, time_type = "frames", fps = 30, mapping = fx$mapping,
    origin_x = 0.1, origin_y = 0.2, radius = 2))
  expect_match(res$code, "origin = c(0.1, 0.2), radius = 2", fixed = TRUE)
  expect_identical(res$result$meta$calibration$radius, 2L)
})

test_that("a failing load keeps its radiatR class and leaves the session unchanged", {
  fx <- agent_fixture()
  bad <- fx$sim
  bad$abs_x[3] <- NA
  bad_path <- agent_write(bad, fx$dir, "bad.csv")
  tools <- radiatR_tools()
  load <- function(path, ...) tools$load_tracks(
    paths = path, time_type = "frames", fps = 30, mapping = fx$mapping, ...)

  err <- expect_error(load(bad_path), class = "radiatR_invalid_rows")
  expect_match(conditionMessage(err),
               "radiatR_load_tracks failed [radiatR_invalid_rows]", fixed = TRUE)
  expect_match(conditionMessage(err), "Call: tracks_1 <- radiatR::read_tracks(",
               fixed = TRUE)
  expect_identical(ls(agent_store_of(tools)$objects), character(0))
  expect_identical(agent_store_of(tools)$log, character(0))

  dropped <- agent_json(load(bad_path, on_invalid = "drop"))
  expect_identical(dropped$handle, "tracks_1")
  expect_true("radiatR_dropped_rows" %in% unlist(dropped$warnings[[1]]$class))

  malformed <- fx$sim
  malformed$abs_x <- as.character(malformed$abs_x)
  malformed$abs_x[3] <- "abc"
  expect_error(load(agent_write(malformed, fx$dir, "malformed.csv")),
               class = "radiatR_malformed_values")
})

test_that("load_tracks reports an id collision and can namespace ids", {
  fx <- agent_fixture()
  copy <- file.path(fx$dir, "b.csv")
  expect_true(file.copy(fx$path, copy))
  tools <- radiatR_tools()
  both <- c(fx$path, copy)

  err <- expect_error(
    tools$load_tracks(paths = both, time_type = "frames", fps = 30,
                      mapping = fx$mapping),
    class = "radiatR_tool_error")
  expect_match(conditionMessage(err), "collide across source files", fixed = TRUE)

  res <- agent_json(tools$load_tracks(
    paths = both, time_type = "frames", fps = 30, mapping = fx$mapping,
    id_collision = "namespace"))
  expect_identical(res$result$n_tracks, 120L)
})

test_that("a missing path is a classed error naming the path", {
  tools <- radiatR_tools()
  err <- expect_error(
    tools$load_tracks(paths = "no-such-file.csv", time_type = "frames"),
    class = "radiatR_path_not_found")
  expect_match(conditionMessage(err), "'no-such-file.csv'", fixed = TRUE)
})

test_that("load_headings needs every unit stated and reads delimited text only", {
  dir <- withr::local_tempdir()
  path <- agent_write(data.frame(bearing = c(10, 20, 350, 5),
                                 grp = c("a", "a", "b", "b"),
                                 note = c("x", "y", "z", "w"),
                                 check.names = FALSE), dir, "h.csv")
  tools <- radiatR_tools()
  load <- function(...) tools$load_headings(path = path, ...)

  res <- agent_json(load(delim = "comma", col = "bearing", units = "degrees",
                         angle_convention = "clock"))
  expect_identical(res$handle, "headings_1")
  expect_identical(res$result$heading_col, "bearing")
  expect_match(res$code, "df_1 <- utils::read.csv(", fixed = TRUE)
  expect_match(res$code, "check.names = FALSE", fixed = TRUE)
  expect_match(res$code, 'angle_convention = "clock"', fixed = TRUE)
  # 10 degrees clockwise from North is 80 degrees counter-clockwise from East.
  expect_equal(agent_store_of(tools)$objects$headings_1$bearing[1], 80 * pi / 180)

  # Nothing is defaulted, including through the MCP path's JSON nulls.
  expect_error(load(col = "bearing", units = "degrees", angle_convention = "clock"),
               class = "radiatR_missing_argument")
  null_units <- sprintf(
    '{"path":"%s","delim":"comma","col":"bearing","units":null,"angle_convention":null}',
    normalizePath(path, winslash = "/"))
  expect_error(agent_mcp_call(tools$load_headings, null_units),
               class = "radiatR_missing_argument")

  # A column that is absent or not numeric is refused before headings are made.
  expect_error(load(delim = "comma", col = "nope", units = "degrees",
                    angle_convention = "clock"),
               class = "radiatR_invalid_argument")
  expect_error(load(delim = "comma", col = "note", units = "radians",
                    angle_convention = "unit_circle"),
               class = "radiatR_invalid_argument")

  # A file whose records differ in length is refused: read.csv() would shift
  # its columns, add a row, or pad it without a word.
  ragged <- function(name, lines) {
    file <- file.path(dir, name)
    writeLines(lines, file)
    expect_error(
      tools$load_headings(path = file, delim = "comma", col = "bearing",
                          units = "degrees", angle_convention = "clock"),
      class = "radiatR_ragged_file")
  }
  ragged("trailing.csv", c("id,bearing,temp", "1,350,20.5,", "2,10,21.5,"))
  ragged("extra.csv", c("bearing,site", "10,a", "50,f,7", "20,b"))
  ragged("short.csv", c("bearing,site", "10,a", "20", "30,b"))
  quoted <- file.path(dir, "quoted.csv")
  writeLines(c("bearing,site", '10,"a, north"', '20,"b"'), quoted)
  expect_identical(
    agent_json(tools$load_headings(path = quoted, delim = "comma", col = "bearing",
                                   units = "degrees",
                                   angle_convention = "clock"))$result$n_rows, 2L)

  xlsx <- file.path(dir, "h.xlsx")
  expect_true(file.copy(path, xlsx))
  expect_error(
    tools$load_headings(path = xlsx, delim = "comma", col = "bearing",
                        units = "degrees", angle_convention = "clock"),
    class = "radiatR_unsupported_file")

  # Every failed call above left the session as the one success made it.
  expect_identical(sort(ls(agent_store_of(tools)$objects)),
                   c("df_1", "df_2", "headings_1", "headings_2"))
  expect_length(agent_store_of(tools)$log, 2L)
})

test_that("describe lists handles, omits units for plain tables, rejects unknown ones", {
  dir <- withr::local_tempdir()
  path <- agent_write(data.frame(a = c(1, 2)), dir, "h.csv")
  tools <- radiatR_tools()
  tools$load_headings(path = path, delim = "comma", col = "a", units = "radians",
                      angle_convention = "unit_circle")

  expect_identical(agent_json(tools$describe())$result$handles,
                   list(df_1 = "data.frame", headings_1 = "headings_frame"))
  expect_null(agent_json(tools$describe(handle = "df_1"))$units)
  expect_identical(agent_json(tools$describe(handle = "headings_1"))$units,
                   list(angles = .agent_angle_units))
  expect_error(tools$describe(handle = "tracks_7"),
               class = "radiatR_unknown_handle")
})

test_that("two tool sets do not share a session", {
  fx <- agent_fixture()
  a <- radiatR_tools()
  b <- radiatR_tools()
  agent_load(a, fx)
  expect_length(agent_json(b$describe())$result$handles, 0L)
})

agent_headings <- function(tools, fx) {
  agent_load(tools, fx)
  agent_json(tools$derive_headings(handle = "tracks_1", rule = "distal",
                                   seed = 1L, params = list(max_radius = 1),
                                   carry = "condition"))
}

test_that("derive_headings returns a headings handle and the attrition counts", {
  fx <- agent_fixture()
  tools <- radiatR_tools()
  res <- agent_headings(tools, fx)

  expect_identical(res$handle, "headings_1")
  expect_identical(res$result$n_tracks, 60L)
  expect_identical(res$result$n_total, 60L)
  expect_identical(res$result$n_missing, 0L)
  expect_identical(
    res$code,
    paste0("base::set.seed(1L)\n",
           'headings_1 <- radiatR::derive_headings(tracks_1, rule = "distal", max_radius = 1, carry = "condition")'))
})

test_that("derive_headings checks rule parameters, carry columns and the seed", {
  fx <- agent_fixture()
  tools <- radiatR_tools()
  agent_load(tools, fx)
  derive <- function(...) tools$derive_headings(handle = "tracks_1", ...)

  err <- expect_error(derive(rule = "distal", seed = 1L, params = list(circ0 = 0.2)),
                      class = "radiatR_invalid_argument")
  expect_match(conditionMessage(err), "unknown: circ0", fixed = TRUE)
  expect_error(derive(rule = "straight", seed = 1L, params = list(min_len = 2.5)),
               class = "radiatR_invalid_argument")
  expect_error(derive(rule = "distal", seed = 1L, params = list(0.2)),
               class = "radiatR_invalid_argument")
  err <- expect_error(derive(rule = "distal", seed = 1L, carry = "conditon"),
                      class = "radiatR_invalid_argument")
  expect_match(conditionMessage(err), "not in the data: conditon", fixed = TRUE)
  # A carried column may not replace one the rule produced.
  clash <- fx$sim
  clash$heading <- 1.234
  tools$load_tracks(paths = agent_write(clash, fx$dir, "clash.csv"),
                    time_type = "frames", fps = 30, mapping = fx$mapping,
                    keep = "heading")
  err <- expect_error(
    tools$derive_headings(handle = "tracks_2", rule = "distal", seed = 1L,
                          carry = "heading"),
    class = "radiatR_invalid_argument")
  expect_match(conditionMessage(err), "would overwrite", fixed = TRUE)
  expect_error(derive(rule = "distal", seed = 1L, params = list(circ0 = NULL)),
               class = "radiatR_invalid_argument")
  expect_error(tools$derive_headings(rule = "distal", seed = 1L),
               class = "radiatR_missing_argument")
  expect_error(derive(rule = "distal"), class = "radiatR_missing_argument")
  expect_error(derive(rule = "distal", seed = 5.9),
               class = "radiatR_invalid_argument")
  expect_error(derive(seed = 1L), class = "radiatR_missing_argument")
  expect_error(tools$derive_headings(handle = "nope", rule = "distal", seed = 1L),
               class = "radiatR_unknown_handle")
  expect_identical(ls(agent_store_of(tools)$objects), c("tracks_1", "tracks_2"))
})

test_that("a rule that draws random numbers is reproducible from its seed", {
  fx <- agent_fixture()
  tools <- radiatR_tools()
  agent_load(tools, fx)
  derive <- function(seed) tools$derive_headings(
    handle = "tracks_1", rule = "ransac_straight", seed = seed, on_missing = "quiet")
  derive(4L); derive(4L); derive(5L)
  objects <- agent_store_of(tools)$objects

  expect_identical(objects$headings_1, objects$headings_2)
  expect_false(identical(objects$headings_1, objects$headings_3))
})

test_that("tracks that yield no heading are reported, not hidden", {
  fx <- agent_fixture()
  tools <- radiatR_tools()
  agent_load(tools, fx)
  res <- agent_json(tools$derive_headings(
    handle = "tracks_1", rule = "crossing", seed = 1L,
    params = list(circ0 = 0.2, circ1 = 50)))

  expect_identical(res$result$n_missing, 60L)
  expect_length(res$result$missing_ids, 60L)
  expect_match(res$warnings[[1]]$message, "produced no heading", fixed = TRUE)
  expect_match(res$code, "circ0 = 0.2, circ1 = 50", fixed = TRUE)
})

test_that("summarise returns one row per group with stated units", {
  fx <- agent_fixture()
  tools <- radiatR_tools()
  agent_headings(tools, fx)
  res <- agent_json(tools$summarise(handle = "headings_1", by = "condition"))

  expect_length(res$result, 3L)
  expect_identical(res$result[[1]]$n, 20L)
  expect_match(res$units$mean_dir_deg, "display convention", fixed = TRUE)
  expect_identical(
    res$code,
    'radiatR::circ_summarise(headings_1, col = "heading", units = "radians", .by = "condition")')
  expect_error(tools$summarise(handle = "tracks_1"),
               class = "radiatR_wrong_handle_type")
  expect_error(tools$summarise(handle = "headings_1", by = "conditon"),
               class = "radiatR_invalid_argument")
})

test_that("test_uniformity is seeded, reproducible and records the seed", {
  fx <- agent_fixture()
  tools <- radiatR_tools()
  agent_headings(tools, fx)
  # Kuiper with a Monte Carlo p-value draws random numbers (Rayleigh does not),
  # and on scattered angles that p-value is not pinned at its minimum.
  scattered <- agent_write(
    data.frame(a = withr::with_seed(42, stats::runif(25, 0, 360))),
    fx$dir, "scattered.csv")
  tools$load_headings(path = scattered, delim = "comma", col = "a",
                      units = "degrees", angle_convention = "unit_circle")
  run <- function(seed, ...) agent_json(tools$test_uniformity(
    handle = "headings_2", seed = seed, test = "kuiper",
    p_method = "monte_carlo", n_sim = 199L, ...))
  first <- run(11L)
  second <- run(11L)
  p <- function(res) res$result[[1]]$p_value

  expect_identical(first$result, second$result)
  expect_false(all(vapply(12:16, function(s) identical(p(run(s)), p(first)), NA)))
  expect_match(first$code, "^base::set.seed\\(11L\\)\n")
  expect_identical(first$result[[1]]$test, "kuiper")

  uniformity <- function(...) tools$test_uniformity(handle = "headings_1", ...)
  expect_error(uniformity(), class = "radiatR_missing_argument")
  # An option the chosen test would ignore is refused, never silently dropped.
  refused <- function(...) expect_error(uniformity(seed = 1L, ...),
                                        class = "radiatR_invalid_argument")
  refused(mu = 1)
  refused(p_method = "monte_carlo")                      # default test: Rayleigh
  refused(test = "rayleigh", p_method = "monte_carlo")
  refused(test = "kuiper", n_sim = 50L)                  # tabled p-value
  refused(test = "kuiper", p_method = "monte_carlo", n_sim = 0L)
  refused(p_adjust = "BH")                               # no group_col
  refused(group_col = "condition", p_adjust = "bonf")    # no abbreviations
  expect_identical(
    agent_json(uniformity(seed = 1L, test = "pycke", n_sim = 99L))$result[[1]]$test,
    "pycke")
  expect_identical(
    agent_json(tools$test_uniformity(handle = "headings_1", seed = 1L,
                                     test = "vtest", mu = 1))$result[[1]]$test,
    "vtest")
})

test_that("compare_groups dispatches on aspect and passes failures through", {
  fx <- agent_fixture()
  tools <- radiatR_tools()
  agent_headings(tools, fx)
  compare <- function(...) tools$compare_groups(
    handle = "headings_1", group_col = "condition", seed = 3L, ...)

  expect_match(agent_json(compare(aspect = "mean_direction"))$code,
               "radiatR::test_mean_directions(", fixed = TRUE)
  expect_match(agent_json(compare(aspect = "concentration"))$code,
               "radiatR::test_concentration(", fixed = TRUE)
  expect_match(agent_json(compare(aspect = "distribution"))$code,
               "radiatR::test_distributions(", fixed = TRUE)

  expect_error(compare(), class = "radiatR_missing_argument")
  null_aspect <- '{"handle":"headings_1","aspect":null,"group_col":"condition","seed":3}'
  expect_error(agent_mcp_call(tools$compare_groups, null_aspect),
               class = "radiatR_missing_argument")
  # An argument the aspect or method would ignore is refused, and a method or
  # adjustment must be named in full.
  refused <- function(...) expect_error(compare(...),
                                        class = "radiatR_invalid_argument")
  logged <- length(agent_store_of(tools)$log)
  refused(aspect = "concentration", method = "watson_williams")
  refused(aspect = "concentration", n_perm = 99L)
  refused(aspect = "mean_direction", parametric = TRUE)
  refused(aspect = "mean_direction", method = "perm")
  refused(aspect = "mean_direction", n_perm = 99L)          # watson_williams
  refused(aspect = "mean_direction", method = "permutation", n_perm = 0L)
  refused(aspect = "mean_direction", p_adjust = "holm")     # not pairwise
  refused(aspect = "mean_direction", pairwise = TRUE, p_adjust = "bonf")
  refused(aspect = "distribution", p_method = "monte_carlo")   # watson_wheeler
  refused(aspect = "distribution", method = "watson_two", n_perm = 99L)
  expect_length(agent_store_of(tools)$log, logged)
  two <- agent_json(compare(aspect = "distribution", method = "watson_two",
                            pairwise = TRUE, p_method = "monte_carlo",
                            n_perm = 99L))
  expect_match(two$code, "n_perm = 99L", fixed = TRUE)
})

test_that("comparing on a column that was not carried names the column", {
  fx <- agent_fixture()
  tools <- radiatR_tools()
  agent_load(tools, fx)
  tools$derive_headings(handle = "tracks_1", rule = "distal", seed = 1L)
  err <- expect_error(
    tools$compare_groups(handle = "headings_1", aspect = "mean_direction",
                         group_col = "condition", seed = 1L),
    class = "radiatR_invalid_argument")
  expect_match(conditionMessage(err), "not in the data: condition", fixed = TRUE)
})

test_that("plot returns the envelope and an inline PNG, and cleans up", {
  skip_if_not(capabilities("png"))
  fx <- agent_fixture()
  tools <- radiatR_tools()
  agent_headings(tools, fx)
  before <- list.files(tempdir(), pattern = "\\.png$")
  out <- tools$plot(handle = "headings_1", facets = "condition")

  expect_length(out, 2L)
  expect_s3_class(out[[1]], "ellmer::ContentText")
  expect_s3_class(out[[2]], "ellmer::ContentImageInline")
  expect_identical(out[[2]]@type, "image/png")
  expect_identical(agent_json(out[[1]]@text)$code,
                   'radiatR::radiate(headings_1, facets = "condition")')
  expect_identical(list.files(tempdir(), pattern = "\\.png$"), before)

  expect_length(tools$plot(handle = "tracks_1"), 2L)
})

test_that("plot refuses a wrong handle or an unknown facet column, logging nothing", {
  fx <- agent_fixture()
  tools <- radiatR_tools()
  agent_headings(tools, fx)
  logged <- length(agent_store_of(tools)$log)

  expect_error(tools$plot(handle = "headings_1", facets = "conditon"),
               class = "radiatR_invalid_argument")
  expect_error(tools$plot(handle = "nope"), class = "radiatR_unknown_handle")
  expect_length(agent_store_of(tools)$log, logged)
})

test_that("radiatR_tools exposes the ten documented tools", {
  expect_identical(
    names(radiatR_tools()),
    c("list_options", "load_tracks", "load_headings", "derive_headings",
      "describe", "summarise", "test_uniformity", "compare_groups", "plot",
      "export_script"))
})

test_that("the exported script reproduces every object and every result", {
  fx <- agent_fixture()
  # A path with a space and an apostrophe (both legal on every CI platform).
  odd <- file.path(fx$dir, "my sim's file.csv")
  expect_true(file.copy(fx$path, odd))
  angles <- agent_write(data.frame(bearing = c(10, 20, 350, 5, 15, 340),
                                   site = rep(c("a", "b"), each = 3)),
                        fx$dir, "angles.csv")
  tools <- radiatR_tools()

  # Tracks path.
  tools$load_tracks(paths = odd, time_type = "frames", fps = 30,
                    mapping = fx$mapping, keep = "condition")
  tools$derive_headings(handle = "tracks_1", rule = "distal", seed = 1L,
                        params = list(max_radius = 1), carry = "condition")
  tools$derive_headings(handle = "tracks_1", rule = "ransac_straight", seed = 2L,
                        on_missing = "quiet")
  tools$summarise(handle = "headings_1", by = "condition")
  tools$test_uniformity(handle = "headings_1", seed = 5L, group_col = "condition",
                        test = "kuiper", p_method = "monte_carlo", n_sim = 199L)
  tools$compare_groups(handle = "headings_1", aspect = "mean_direction",
                       group_col = "condition", seed = 6L,
                       method = "permutation", n_perm = 199L)
  # Table-of-angles path.
  tools$load_headings(path = angles, delim = "comma", col = "bearing",
                      units = "degrees", angle_convention = "clock")
  tools$summarise(handle = "headings_3", by = "site")
  tools$test_uniformity(handle = "headings_3", seed = 7L)
  if (capabilities("png")) tools$plot(handle = "headings_3")

  store <- agent_store_of(tools)
  script <- agent_json(tools$export_script())$code
  expect_identical(script, .agent_script(store))

  # Run the whole script in a fresh environment: every object matches.
  env <- new.env(parent = baseenv())
  eval(parse(text = script), env)
  expect_setequal(ls(env), ls(store$objects))
  for (handle in ls(store$objects))
    expect_identical(get(handle, env), get(handle, store$objects), info = handle)

  # Run it call by call: every result matches the value the tool computed.
  env <- new.env(parent = baseenv())
  for (i in seq_along(store$log)) {
    value <- NULL
    for (stmt in parse(text = store$log[[i]])) value <- eval(stmt, env)
    if (inherits(value, "ggplot")) next
    expect_identical(value, store$results[[i]], info = store$log[[i]])
  }
})

test_that("a tool that prints does not write to stdout", {
  store <- .agent_store()
  expect_silent(
    run <- .agent_run(store, "t", list(.agent_step(
      "print", list("to stdout"), ns = "base"))))
  expect_match(run$output, "to stdout", fixed = TRUE)
  env <- jsonlite::fromJSON(.agent_envelope(run), simplifyVector = FALSE)
  expect_match(env$output, "to stdout", fixed = TRUE)
})

test_that("a kept column absent from one of several files is refused", {
  fx <- agent_fixture()
  tools <- radiatR_tools()
  a <- fx$sim[fx$sim$condition == "control", ]
  b <- fx$sim[fx$sim$condition != "control", ]
  names(b)[names(b) == "condition"] <- "Condition"
  paths <- c(agent_write(a, fx$dir, "fa.csv"), agent_write(b, fx$dir, "fb.csv"))
  err <- expect_error(
    tools$load_tracks(paths = paths, time_type = "frames", fps = 30,
                      mapping = fx$mapping, keep = "condition"),
    class = "radiatR_invalid_argument")
  expect_match(conditionMessage(err), "'condition'.*fb\\.csv")
  expect_length(agent_store_of(tools)$log, 0L)
  # Present in every file: loads.
  b$Condition <- NULL
  b$condition <- "stimulus"
  paths[2] <- agent_write(b, fx$dir, "fb.csv")
  res <- agent_json(tools$load_tracks(paths = paths, time_type = "frames", fps = 30,
                                      mapping = fx$mapping, keep = "condition"))
  expect_identical(res$handle, "tracks_1")
})

test_that("a workbook with more than one sheet is refused", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")
  fx <- agent_fixture()
  tools <- radiatR_tools()
  xl <- file.path(fx$dir, "two.xlsx")
  writexl::write_xlsx(list(day1 = fx$sim, day2 = fx$sim), xl)
  err <- expect_error(
    tools$load_tracks(paths = xl, time_type = "frames", fps = 30,
                      mapping = fx$mapping),
    class = "radiatR_invalid_argument")
  expect_match(conditionMessage(err), "day1, day2", fixed = TRUE)
  one <- file.path(fx$dir, "one.xlsx")
  writexl::write_xlsx(list(day1 = fx$sim), one)
  res <- agent_json(tools$load_tracks(paths = one, time_type = "frames", fps = 30,
                                      mapping = fx$mapping))
  expect_identical(res$handle, "tracks_1")
})

test_that("a string rule parameter must be one of the rule's choices in full", {
  fx <- agent_fixture()
  tools <- radiatR_tools()
  agent_load(tools, fx)
  derive <- function(...) tools$derive_headings(handle = "tracks_1", seed = 1L, ...)
  err <- expect_error(
    derive(rule = "crossing", params = list(circ0 = 0.2, circ1 = 0.8, direction = "in")),
    class = "radiatR_invalid_argument")
  expect_match(conditionMessage(err), "'outward', 'inward' (got 'in')", fixed = TRUE)
  # "either" is a ring_tangent direction, not a crossing one.
  expect_error(
    derive(rule = "crossing", params = list(circ0 = 0.2, circ1 = 0.8, direction = "either")),
    class = "radiatR_invalid_argument")
  expect_error(derive(rule = "velocity_mean", params = list(weight_by = "step")),
               class = "radiatR_invalid_argument")
  expect_error(derive(rule = "pca_axis", params = list(source = "vel")),
               class = "radiatR_invalid_argument")
  res <- agent_json(derive(rule = "pca_axis", params = list(source = "velocity")))
  expect_identical(res$handle, "headings_1")
})

test_that("the rule-parameter choices match the rule helpers", {
  helper <- function(rule)
    get(paste0(".set_headings_", if (rule == "vm_fit") "velocity_mean" else rule, "_one"))
  for (rule in names(.agent_rule_param_choices))
    for (p in names(.agent_rule_param_choices[[rule]]))
      expect_identical(.agent_rule_param_choices[[rule]][[p]],
                       eval(formals(helper(rule))[[p]]), label = paste(rule, p))
  # Every string parameter of every rule has its choices listed.
  for (rule in names(.agent_rule_params)) {
    strings <- .agent_rule_params[[rule]][
      .agent_rule_param_types[.agent_rule_params[[rule]]] == "string"]
    expect_setequal(names(.agent_rule_param_choices[[rule]]) %||% character(), strings)
  }
})

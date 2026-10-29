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

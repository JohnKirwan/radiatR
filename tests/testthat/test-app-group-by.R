# The dedicated Group-by control drives the Summary & stats grouping and the
# resolved plot spec independently of the facet selection. The bundled example
# (cpunctatus) has two categorical grouping columns: `arc` and `type`.
test_that("trajectory: group_by drives summary_ctx by_col independent of facet", {
  skip_if_not_installed("shiny")
  app_dir <- system.file("app", package = "radiatR")
  if (!nzchar(app_dir)) app_dir <- testthat::test_path("..", "..", "inst", "app")
  skip_if(!dir.exists(app_dir), "app dir not found")

  shiny::testServer(app_dir, {
    session$setInputs(load_example = 1)
    session$setInputs(method = "distal", go3 = 1)
    # Facet by one column, Group by another: stats must key on the Group column.
    session$setInputs(cond_col = "type", cond_col2 = "",
                      group_by = "arc", colour_by = "__group__")
    ctx <- summary_ctx()
    expect_equal(ctx$by_col, "arc")            # group, not facet
    expect_true("arc" %in% names(ctx$hd))      # attached onto hd for stats
    spec <- current_spec()
    expect_equal(spec$group_by, "arc")
    expect_true(spec$colour$follows_group)
    expect_equal(spec$facet_by, "type")        # facet stays independent

    # The summary table renders (not the "Summary not available" fallback) with
    # Group != facet. Regression guard for the Straightness aggregation, which
    # must key on ctx$by_col (the group) not ctx$gc (the facet) or it goes all-NA.
    tbl <- output$summary_tbl                  # rendered HTML string
    expect_false(grepl("Summary not available", tbl))
    expect_true(grepl("Straightness", tbl))
    expect_true(grepl("0\\.[0-9]", tbl))       # at least one numeric cell populated
  })
})

test_that("headings: group_by drives stats, decoupled from the facet", {
  skip_if_not_installed("shiny")
  app_dir <- system.file("app", package = "radiatR")
  if (!nzchar(app_dir)) app_dir <- testthat::test_path("..", "..", "inst", "app")
  skip_if(!dir.exists(app_dir), "app dir not found")

  shiny::testServer(app_dir, {
    session$setInputs(input_type = "headings")   # switch to headings mode
    session$setInputs(load_example_hd = 1)       # load the millipede headings
    # Facet on one column (hd_group), Group unset -> stats pool (".all"),
    # NOT keyed on the facet column (decoupled from hd_map$group).
    session$setInputs(hd_group = "arc", hd_group2 = "", group_by = "")
    ctx <- summary_ctx()
    expect_true(ctx$pooled)
    expect_equal(ctx$by_col, ".all")
    # Group set -> stats key on it, independent of the facet.
    session$setInputs(group_by = "type")
    ctx2 <- summary_ctx()
    expect_false(ctx2$pooled)
    expect_equal(ctx2$by_col, "type")
    expect_equal(current_spec()$group_by, "type")
  })
})

test_that("group comparison card shows a placeholder note when pooled", {
  skip_if_not_installed("shiny")
  app_dir <- system.file("app", package = "radiatR")
  if (!nzchar(app_dir)) app_dir <- testthat::test_path("..", "..", "inst", "app")
  skip_if(!dir.exists(app_dir), "app dir not found")

  shiny::testServer(app_dir, {
    session$setInputs(load_example = 1)
    session$setInputs(method = "distal", go3 = 1)
    session$setInputs(cond_col = "", cond_col2 = "", group_by = "")
    tbl <- paste(output$group_compare_tbl, collapse = " ")
    expect_true(grepl("Select a Group by column", tbl, fixed = TRUE))
  })
})

test_that("group comparison card renders three rows once a Group by column is set", {
  skip_if_not_installed("shiny")
  app_dir <- system.file("app", package = "radiatR")
  if (!nzchar(app_dir)) app_dir <- testthat::test_path("..", "..", "inst", "app")
  skip_if(!dir.exists(app_dir), "app dir not found")

  shiny::testServer(app_dir, {
    session$setInputs(load_example = 1)
    session$setInputs(method = "distal", go3 = 1)
    session$setInputs(cond_col = "type", cond_col2 = "", group_by = "arc")
    tbl <- paste(output$group_compare_tbl, collapse = " ")
    expect_true(grepl("Mean direction", tbl, fixed = TRUE))
    expect_true(grepl("Concentration", tbl, fixed = TRUE))
    expect_true(grepl("Distribution", tbl, fixed = TRUE))
    expect_false(grepl("Select a Group by column", tbl, fixed = TRUE))
  })
})

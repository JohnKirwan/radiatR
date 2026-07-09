# default (group unset) figure code is frozen — plain

    Code
      cat(spec_to_code(char_spec()))
    Output
      library(radiatR)
      library(ggplot2)
      
      data(cpunctatus)
      ts <- cpunctatus
      
      hd <- derive_headings(ts, rule = "distal")
      
      ts <- assign_colour_key(ts, by = "trajectory")
      hd <- assign_colour_key(hd, by = "trajectory", reference = ts)
      
      disp <- circ_display(zero = 0)
      attr(hd, "display") <- disp
      
      arrow_df <- compute_circ_mean(hd)
      
      radiate(ts, group_col = "trial_id", colour_col = ".colour", legend = FALSE, theme = "void", angle_labels = "degrees", show_labels = FALSE, show_arrow = FALSE, display = disp) +
        add_heading_points(hd, colour_col = ".colour", size = 2.5, alpha = 0.8) +
        add_circ_mean(arrow_df, colour = "black") +
        add_heading_interval(hd, stat = "bootstrap_ci") +
        add_critical_r(hd, test = "rayleigh", colour = "firebrick", linewidth = 0.7) +
        add_critical_v_line(hd, mu0 = pi / 2, angle_col = "heading", colour = "steelblue", linewidth = 0.8)

# default (group unset) stats code is frozen — plain

    Code
      cat(spec_to_stats_code(char_spec()))
    Output
      library(radiatR)
      
      data(cpunctatus)
      ts <- cpunctatus
      
      hd <- derive_headings(ts, rule = "distal")
      
      # Summary & stats analysis (display formatting is applied in the app)
      if (!"trial_id" %in% names(hd)) hd <- merge(hd, unique(as.data.frame(ts)[, c("trial_id", "trial_id")]), by.x = "id", by.y = "trial_id", all.x = TRUE)
      summ <- circ_summarise(hd, "heading", units = "radians", .by = "trial_id", stats = c("n", "n_missing", "mean_dir_deg", "resultant_R"), display = circ_display(zero = 0))
      summ
      
      test_uniformity(hd, test = "rayleigh")
      test_uniformity(hd, test = "rao")
      
      circ_model_select(hd, group_col = "trial_id")
      
      straightness_index(ts)

# default (group unset) figure code is frozen — faceted

    Code
      cat(spec_to_code(char_spec("type")))
    Output
      library(radiatR)
      library(ggplot2)
      
      data(cpunctatus)
      ts <- cpunctatus
      
      hd <- derive_headings(ts, rule = "distal")
      hd <- merge(hd, unique(as.data.frame(ts)[, c("trial_id", "type")]), by.x = "id", by.y = "trial_id", all.x = TRUE)
      
      ts <- assign_colour_key(ts, by = "trajectory")
      hd <- assign_colour_key(hd, by = "trajectory", reference = ts)
      
      disp <- circ_display(zero = 0)
      attr(hd, "display") <- disp
      
      arrow_df <- compute_circ_mean(hd, facets = "type")
      
      radiate(ts, group_col = "trial_id", colour_col = ".colour", facets = "type", legend = FALSE, theme = "void", angle_labels = "degrees", show_labels = FALSE, show_arrow = FALSE, display = disp) +
        add_heading_points(hd, colour_col = ".colour", size = 2.5, alpha = 0.8) +
        add_circ_mean(arrow_df, colour = "black") +
        add_heading_interval(hd, facets = "type", stat = "bootstrap_ci") +
        add_critical_r(hd, test = "rayleigh", facets = "type", colour = "firebrick", linewidth = 0.7) +
        add_critical_v_line(hd, mu0 = pi / 2, angle_col = "heading", facets = "type", colour = "steelblue", linewidth = 0.8)

# default (group unset) stats code is frozen — faceted

    Code
      cat(spec_to_stats_code(char_spec("type")))
    Output
      library(radiatR)
      
      data(cpunctatus)
      ts <- cpunctatus
      
      hd <- derive_headings(ts, rule = "distal")
      hd <- merge(hd, unique(as.data.frame(ts)[, c("trial_id", "type")]), by.x = "id", by.y = "trial_id", all.x = TRUE)
      
      # Summary & stats analysis (display formatting is applied in the app)
      summ <- circ_summarise(hd, "heading", units = "radians", .by = "type", stats = c("n", "n_missing", "mean_dir_deg", "resultant_R"), display = circ_display(zero = 0))
      summ
      
      test_uniformity(hd, test = "rayleigh")
      test_uniformity(hd, test = "rao")
      
      circ_model_select(hd, group_col = "type")
      
      straightness_index(ts)


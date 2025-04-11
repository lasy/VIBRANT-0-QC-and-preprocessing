

plot_conc_by_analyte <- function(t, color_by = "value_type"){
  g <- 
    t |>
    as_tibble() |>
    group_by(.feature) |> 
    mutate(median = median(conc, na.rm = TRUE)) |>
    ungroup() |> 
    arrange(median) |>
    mutate(.feature = .feature |> fct_inorder()) |>
    ggplot(aes(x = .feature, y = conc, col = !!sym(color_by))) +
    ggbeeswarm::geom_quasirandom(alpha = 0.5, size = 0.1) +
    scale_y_log10() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    ggtitle(str_c(t@metadata$name, " (", t@metadata$n_plates, " plates)")) +
    ylab("Concentration")
  
  if (color_by == "value_type"){
    g <- 
      g +
      scale_color_manual("Value type", breaks = value_types_levels(), values = value_types_colors()) 
  } else if (color_by == "sample_type"){
    g <- 
      g +
      scale_color_manual("Sample type", breaks = c("Standard", "Manuf. control", "Positive control", "Sample"), values = c("coral", "black", "green", "steelblue1")) 
  } else {
    g <- g + scale_color_discrete(name = color_by |> str_replace_all("_"," ") |> str_to_title())
  }
      
  g
}


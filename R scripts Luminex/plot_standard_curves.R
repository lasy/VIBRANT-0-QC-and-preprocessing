
plot_standard_curves <- function(t) {
  t |>
    filter(sample_type == "Standard") |>
    mutate(standard_nb = sample_id |> parse_number()) |>
    ggplot(aes(x = exp_conc |> log2(), y = FI_wo_background |> log2(), col = plate_name)) +
    geom_text(aes(label = standard_nb), alpha = 0.5) +
    facet_wrap(.feature ~ ., scales = "free", ncol = 10) +
    ggtitle(t$metadata$name)
}

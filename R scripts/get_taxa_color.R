
get_taxa_colors <- function(taxa) {
  tibble(
    taxa = taxa
  ) |> 
    mutate(
      cat = 
        case_when(
          taxa == "Other" ~ "Other",
          str_detect(taxa, "crispatus") ~ "crispatus",
          str_detect(taxa, "LBP") ~ "LBP",
          str_detect(taxa, "iner") ~ "iners",
          str_detect(taxa, "revotella") ~ "prevotella",
          str_detect(taxa, "ardnerella") ~ "gardnerella",
          TRUE ~ "non lacto"
        )
    ) |> 
    group_by(cat) |>
    mutate(
      color = 
        case_when(
          taxa == "Other" ~ "gray80",
          str_detect(taxa, "crispatus") ~ "orange",
          str_detect(taxa, "LBP") ~ colorRampPalette(c("orangered1", "red4"))(n()),
          str_detect(taxa, "iner") ~ "green3",
          str_detect(taxa, "jensenii") ~ "green4",
          str_detect(taxa, "ardnerella") ~ colorRampPalette(c("dodgerblue1", "dodgerblue4"))(n()),
          str_detect(taxa, "revotella") ~ colorRampPalette(c("purple1", "purple4"))(n()),
          TRUE ~ colorRampPalette(c("turquoise3", "black"))(n())
        )
    ) |> 
    pull(color)
}
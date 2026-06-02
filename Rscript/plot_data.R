plot_index_comparison <- function(df, file_path, title = "Index Comparison") {
  # Prepare data for ggplot (gathering Truth and Sampled into one column)
  plot_df <- df |>
    dplyr::select(truth_year, truth_value, sampled_value) |>
    tidyr::pivot_longer(cols = c(truth_value, sampled_value),
                        names_to = "Type", values_to = "Value") |>
    dplyr::mutate(Type = dplyr::recode(Type,
                                       truth_value = "OM Truth",
                                       sampled_value = "Sampled (with Error)"))

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = truth_year, y = Value, color = Type)) +
    ggplot2::geom_line(alpha = 0.6) +
    ggplot2::geom_point() +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c("OM Truth" = "black", "Sampled (with Error)" = "firebrick")) +
    ggplot2::labs(title = title, x = "Year", y = "Value", color = NULL)

  ggplot2::ggsave(
    filename = paste0(title, ".png"),
    path = file_path,
    plot = p,
    width = 8,
    height = 6,
    dpi = 1200
  )

}

plot_age_comp_normalized <- function(df, title, file_path) {

  plot_df <- df |>
    dplyr::group_by(truth_year) |>
    # Normalize both columns to proportions (0 to 1)
    dplyr::mutate(
      truth_prop = truth_value / sum(truth_value),
      sampled_prop = sampled_value / sum(sampled_value)
    ) |>
    dplyr::ungroup() |>
    # Pivot for ggplot
    dplyr::select(truth_year, truth_group, truth_prop, sampled_prop) |>
    tidyr::pivot_longer(
      cols = c(truth_prop, sampled_prop),
      names_to = "Type",
      values_to = "Proportion"
    ) |>
    dplyr::mutate(Type = dplyr::recode(Type,
                                       truth_prop = "OM Truth (Expected)",
                                       sampled_prop = "Sampled (Multinomial)"))

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = truth_group, y = Proportion, fill = Type)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::facet_wrap(~truth_year) +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_manual(values = c("OM Truth (Expected)" = "gray70",
                                          "Sampled (Multinomial)" = "steelblue")) +
    ggplot2::labs(
      title = title,
      x = "Age",
      y = "Proportion",
      fill = NULL
    )

  ggplot2::ggsave(
    filename = paste0(title, ".png"),
    path = file_path,
    plot = p,
    width = 8,
    height = 6,
    dpi = 1200
  )
}

plot_weight_trends <- function(df, file_path) {
  p <- ggplot2::ggplot(df, ggplot2::aes(x = truth_year, y = truth_value, color = truth_group)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Weight-at-Age Trends", x = "Year", y = "Weight", color = "Age Group")

  ggplot2::ggsave(
    filename = "weight_at_age_om.png",
    path = file_path,
    plot = p,
    width = 8,
    height = 6,
    dpi = 1200
  )
}

plot_survey_vs_catch <- function(survey_df, catch_df, file_path) {
  # Normalize both to their mean so they are on the same scale
  s <- survey_df |>
    dplyr::mutate(Value = sampled_value / mean(sampled_value), Source = "Survey")
  c <- catch_df |>
    dplyr::mutate(Value = sampled_value / mean(sampled_value), Source = "Catch")

  combined <- dplyr::bind_rows(
    dplyr::select(s, truth_year, Value, Source),
    dplyr::select(c, truth_year, Value, Source)
  )

  p <- ggplot2::ggplot(
    combined, ggplot2::aes(x = truth_year, y = Value, color = Source)
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Normalized Survey vs. Catch Indices",
                  x = "Year", y = "Relative Index")

  ggplot2::ggsave(
    filename = "Normalized Survey vs. Catch Indices.png",
    path = file_path,
    plot = p,
    width = 8,
    height = 6,
    dpi = 1200
  )
}

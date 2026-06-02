# Generate default FIMS model configurations
configurations <- FIMS::create_default_configurations(
  data = data_fims
) |>
  tidyr::unnest(cols = data) |>
  dplyr::rows_update(
    y = tibble::tibble(
      module_name = "Selectivity",
      module_type = "DoubleLogistic"
    ),
    by = c("module_name")
  )

# Estimate maturity parameters
# TODO: check spawning proportion from EwE
maturity_parameters <- ecosystemom::estimate_true_maturity(
  ages = ages,
  spawning_proportion = c(0, 0.1, 0.5, 0.9, 1),
  functional_form = "logistic"
)

# Create default parameter values from the updated model configuration
parameters <- FIMS::create_default_parameters(
  configurations = configurations,
  data = data_fims
) |>
  tidyr::unnest(cols = data) |>
  dplyr::filter(!(module_name == "Selectivity" & fleet_name == fishing_fleet_name)) |>
  dplyr::bind_rows(catch_selectivity) |> 
  dplyr::rows_update(
    y = tibble::tibble(
      fleet_name = fishing_fleet_name,
      label = "log_Fmort",
      time = fishing_mortality_index_om[["truth_year"]],
      value = fishing_mortality_index_om[["truth_value"]] |>
        log()
    ), 
    by = c("fleet_name", "label", "time")
  ) |> 
  dplyr::rows_update(
    y = tibble::tibble(
      fleet_name = survey_fleet_name,
      label = c(
        "inflection_point_asc", "slope_asc", 
        "inflection_point_desc", "slope_desc", 
        "log_q"
      ),
      value = c(
        selectivity_inflection_point_asc_survey,
        selectivity_slope_asc_survey,
        selectivity_inflection_point_desc_survey,
        selectivity_slope_desc_survey,
        log(catchability_survey)
      )
    ),
    by = c("fleet_name", "label")
  ) |>
  dplyr::rows_update(
    y = tibble::tibble(
      label = "log_rzero", 
      module_type = "BevertonHolt",
      value = number_agecomp_om |>
        dplyr::filter(truth_group  == "0", truth_year == years[1]) |>
        dplyr::pull(truth_value) |>
        log()
    ),
    by = c("label", "module_type")
  ) |>
  # TODO: check vulnerability matrix to get steepness
  dplyr::rows_update(
    y = tibble::tibble(
      label = "logit_steep", 
      module_type = "BevertonHolt",
      # calculate from vulnerability matrix: v / (v + 1)
      # v = 411.23 + 1.02 + 191.58 + 2 + 1016.36 + 12.18 + 2 + 403.26 = 2039.63
      # h = v / (v + 1) = 0.99
      value = -log(1.0 - 0.99) + log(0.99 - 0.2)
    ),
    by = c("label", "module_type")
  ) |>
  dplyr::filter(!(module_name == "Maturity")) |>
  dplyr::bind_rows(maturity_parameters) |>
  dplyr::rows_update(
    y = tibble::tibble(
      label = "log_M", 
      age = unname(ages[natural_mortality_agecomp_om[["truth_group"]]]),
      time = natural_mortality_agecomp_om[["truth_year"]],
      value = log(natural_mortality_agecomp_om[["truth_value"]])
    ),
    by = c("label", "age", "time")
  ) |>
  dplyr::rows_update(
    y = tibble::tibble(
      label = "log_init_naa",
      age = number_agecomp_om |>
        dplyr::filter(truth_year == years[1]) |>
        dplyr::pull(truth_group) |>
        (\(x) unname(ages[x]))(),
      value = number_agecomp_om |>
        dplyr::filter(truth_year == years[1]) |>
        dplyr::pull(truth_value) |>
        log()
    ),
    by = c("label", "age")
  )

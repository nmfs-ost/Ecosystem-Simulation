# Generate default FIMS model configurations
#this is causing an error so trying something different below
# configurations <- FIMS::create_default_configurations(
#   data = data_fims
# ) |>
#   tidyr::unnest(cols = data) |>
#   dplyr::rows_update(
#     tibble::tibble(
#       fleet = c(fishing_fleet_name, yoy_fleet_name, survey_fleet_name),
#       module_name = "Selectivity",
#       module_type = "DoubleLogistic"
#     ),
#     by = c("module_name", "fleet")
#   )

# Step 1: Create unnested base configuration table
config_df <- FIMS::create_default_configurations(data = data_fims) |>
  tidyr::unnest(cols = data) |>
  as.data.frame() # Ensures standard data frame/tibble structure

# Step 2: Define update table
updates <- tibble::tibble(
  fleet = c(fishing_fleet_name, yoy_fleet_name, survey_fleet_name),
  module_name = "Selectivity",
  module_type = "DoubleLogistic"
)

# Step 3: Perform rows_update explicitly
configurations <- dplyr::rows_update(
  x = config_df,
  y = updates,
  by = c("module_name", "fleet")
)

# Estimate maturity parameters
# TODO: check spawning proportion from EwE
maturity_parameters <- ecosystemom::estimate_true_maturity(
  ages = ages,
  spawning_proportion = c(0, 0.1, 0.5, 0.9, 1),
  # spawning_proportion = c(0, 0, 0, 1, 1),
  functional_form = "logistic"
)

# Estimate recruitment log_sd
recruitment_ewe <- number_agecomp_om |>
  dplyr::filter(truth_group == "0yr") |>
  dplyr::pull(truth_value)

log_sd_proxy <- (sd(log(recruitment_ewe) - mean(log(recruitment_ewe)))) |>
  log()
# log_sd_proxy <- log(0.5)

# Create default parameter values from the updated model configuration
parameters <- FIMS::create_default_parameters(
  configurations = configurations,
  data = data_fims
) |>
  tidyr::unnest(cols = data) |>
  dplyr::rows_update(
    y = tibble::tibble(
      fleet = fishing_fleet_name,
        label = c("inflection_point_asc", "slope_asc", "inflection_point_desc", "slope_desc"),
        estimation_type = c(
          rep("fixed_effects", 2),
          rep("constant", 2)
        ),
        value = c(
          catch_selectivity_inflection_point_asc,
          catch_selectivity_slope_asc,
          catch_selectivity_inflection_point_desc,
          catch_selectivity_slope_desc
        )
      ), by = c("fleet", "label")
  )
  dplyr::rows_update(
    y = tibble::tibble(
      fleet = fishing_fleet_name,
      label = "log_Fmort",
      time = fishing_mortality_index_om[["truth_year"]],
      # estimation_type = "constant",
      value = fishing_mortality_index_om[["truth_value"]] |>
        log()
    ),
    by = c("fleet", "label", "time")
  ) |>
  dplyr::rows_update(
    y = tibble::tibble(
      fleet = survey_fleet_name,
      label = c(
        "inflection_point_asc", "slope_asc",
        "inflection_point_desc", "slope_desc",
        "log_q"
      ),
      estimation_type = c(
        rep("fixed_effects", 0),
        rep("constant", 5)
      ),
      value = c(
        selectivity_inflection_point_asc_survey,
        selectivity_slope_asc_survey,
        selectivity_inflection_point_desc_survey,
        selectivity_slope_desc_survey,
        log(catchability_survey)
      )
    ),
    by = c("fleet", "label")
  )  |>
    dplyr::rows_update(
      y = tibble::tibble(
        fleet = yoy_fleet_name,
        label = c("inflection_point_asc", "slope_asc",
                  "inflection_point_desc", "slope_desc",
                  "log_q"),
        estimation_type = c(
          rep("constant", 4),
          rep("fixed_effects", 1)
        ),
        value = c(
          yoy_inflection_point_asc,
          yoy_slope_asc,
          yoy_inflection_point_desc,
          yoy_slope_desc,
          log(yoy_q)
        )
      ),
      by = c("fleet", "label")
    ) |>
  dplyr::rows_update(
    y = tibble::tibble(
      label = "log_rzero",
      module_type = "BevertonHolt",
      value = number_agecomp_om |>
        dplyr::filter(truth_group == "0yr") |>
        dplyr::pull(truth_value) |>
        (\(x) mean(log(x)))()
        # dplyr::filter(truth_group  == "0yr", truth_year == years[1]) |>
        # dplyr::pull(truth_value) |>
        # log()
    ),
    by = c("label", "module_type")
  ) |>
  # TODO: check vulnerability matrix to get steepness
  dplyr::rows_update(
    y = tibble::tibble(
      label = "logit_steep",
      module_type = "BevertonHolt",
      # estimation_type = "fixed_effects",
      # estimate steepness from biomass and recruitment:
      # biomass: biomass_index_om |> dplyr::pull(truth_value)
      # recruitment: number_agecomp_om |> dplyr::filter(truth_group == "0yr") |> dplyr::pull(truth_value)
      # h ~0.2 or 0.75
      # value = -log(1.0 - 0.75) + log(0.75 - 0.2),
      # calculate from vulnerability matrix: v / (v + 1)
      # v = 1.01 + 1.62 + 8.4 + 1.9 + 1.35
      # v = 411.23 + 1.02 + 191.58 + 2 + 1016.36 + 12.18 + 2 + 403.26 = 2039.63
      # h = v / (v + 1) = 0.99
      value = -log(1.0 - 0.93) + log(0.93 - 0.2),
    ),
    by = c("label", "module_type")
  ) |>
  dplyr::rows_update(
    y = tibble::tibble(
      label = "log_sd",
      module_type = "BevertonHolt",
      estimation_type = "fixed_effects",
      value = log_sd_proxy
    ),
    by = c("label", "module_type")
  ) |>
  dplyr::rows_update(
    y = tibble::tibble(
     label = "log_devs",
     module_type = "BevertonHolt",
     estimation_type = "random_effects"
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
      value = log(natural_mortality_agecomp_om[["truth_value"]] - 0.2)
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
      estimation_type = c(
        rep("fixed_effects", 3),
        rep("constant", 2)
      ),
      value = number_agecomp_om |>
        dplyr::filter(truth_year == years[1]) |>
        dplyr::pull(truth_value) |>
        log()
    ),
    by = c("label", "age")
  )

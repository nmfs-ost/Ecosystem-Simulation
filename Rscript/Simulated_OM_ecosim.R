# This script creates the simulated ecosystem operating model and gets the "true"
# values from it

# Set TRUE to run the operating model with egg production.
egg_production <- TRUE

#### ---------Setup, load packages ####
if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak")
}

# Required packages
required_packages <- c(
  "fs",
  "ggplot2",
  # For downloading EwE ouputs from a Google Drive folder
  "googledrive",
  "James-Thorson-NOAA/dsem",
  # For standardizing EwE output and simulating observations
  "NOAA-FIMS/ecosystemom",
  # For generating selectivity curves
  "NOAA-FIMS/FIMS",
  "purrr"
)

# Install required packages
pak::pkg_install(required_packages, ask = FALSE)

# Load required packages
library(ecosystemom)
library(FIMS)
library(stockplotr)
library(fs)
library(ggplot2)
library(googledrive)
library(dsem)

# set random seed for reproducibility
set.seed(123)

# Source utility scripts
source(file.path("Rscript", "utils.R"))
source(file.path("Rscript", "plot_data.R"))

#### ----------Set up hard coded values ####

# biomass scalar
biomass_scalar <- 350000
# proportion of weight at infinity
weight_scalar <- 696 / 1000000
# Winf = 696 grams

# Define model years
years <- 1980:2023

# Define ages
ages <- 0:4

# Define fishing fleet
# Define fleet name
fishing_fleet_name <- "fishing_fleet"
# Define the uncertainty of sampled catch observations
catch_index_sd <- 0.05
catch_agecomp_sample_size <- 120

# Define survey fleet for ages 1-4+
# Define fleet name
survey_fleet_name <- "adult_survey_fleet"
# TODO:
# - Define realistic catchability and selectivity patterns
# Define survey catchability
catchability_survey <- 0.05

# Survey selectivity: Double logistic selectivity
selectivity_inflection_point <- 1.2
selectivity_slope <- 2.5

selectivity_module_survey <- methods::new(FIMS::LogisticSelectivity)
selectivity_module_survey$inflection_point[1]$value <- selectivity_inflection_point
selectivity_module_survey$slope[1]$value <- selectivity_slope
selectivity_survey <- purrr::map_dbl(ages, ~selectivity_module_survey$evaluate(.x))
FIMS::clear()

# Define the uncertainty of sampled survey observations
survey_index_sd <- 0.1
survey_agecomp_sample_size <- 120

#### ----------Initializing input files and directories ####

model_folder <- if (egg_production) {
  "ecosim_egg_production_sefsc"
} else {
  "ecosim_sefsc"
}
data_destination <- file.path(getwd(), "data", model_folder)
figures_path <- file.path(getwd(), "figures", model_folder)

# Download data only if directory is missing or empty
if (!dir.exists(data_destination) || length(list.files(data_destination)) == 0) {

  message("Data directory is empty. Starting Google Drive download...")

  # Authenticate with Google Drive (OAuth flow)
  googledrive::drive_auth(scopes = "https://www.googleapis.com/auth/drive")

  # Google Drive folder ID
  ecosim_sefsc_id <- googledrive::as_id(
    if (egg_production) {
      "1XA4s8AamYjpI4gOOcWMW7tS1fSHyvfKC"
    } else {
      "1dDj8RzHSDyaG19N9OgPV371vdRzZY7e8"
    }
  )

  # Download all files recursively
  download_drive_recursive(
    drive_item = ecosim_sefsc_id,
    local_destination_path = data_destination
  )

  message("Download complete.")
} else {
  message("Data already exists in ", data_destination, ". Skipping download.")
}

#### ---------- Get Truth ####

# Load functional groups
functional_groups <- ecosystemom::get_functional_groups(
  file_path = file.path(data_destination, "1-Basic estimates.csv")
)

functional_groups |> print(n = 100)
names(ages) <- functional_groups |>
  dplyr::filter(species == "Menhaden") |>
  dplyr::pull(group)

names(selectivity_survey) <- functional_groups |>
  dplyr::filter(species == "Menhaden") |>
  dplyr::pull(group)
# TODO:
# - Add units for biomass, catch, and weight
# - Confirm whether discard data are included
# Load EwE model output
data_om <- ecosystemom::load_model(
  directory = data_destination,
  type = "ewe_ecosim",
  functional_groups = functional_groups,
  unit = c(
    "biomass" = "mt",
    "catch" = "mt",
    "total_mortality" = "year^-1",
    "weight" = "mt"
  )
) |>
  # TODO: define year range
  dplyr::filter(year %in% years)

# Load environmental data
if (egg_production) {
  data_raw_pp <- read.csv(file.path(data_destination, "PP_integrated_raw_relative.csv"))
  # Convert raw PP data to a tibble with both year and month columns
  # The original data_raw_pp only contains time column that sort by year and month
  data_pp <- data_raw_pp |>
    dplyr::mutate(
      # Check if the row contains a 4-digit year
      is_year = grepl("^\\d{4}$", Time),
      # Extract the year and forward-fill down through the months
      year = as.integer(ifelse(is_year, Time, NA_integer_)),
      # Rows with the year represent January; otherwise keep the month name
      month_name = ifelse(is_year, "Jan", Time)
    ) |>
    tidyr::fill(year, .direction = "down") |>
    dplyr::mutate(
      index = "PP",
      month = match(month_name, month.abb),
      value = as.numeric(Value),
      unit  = NA
    ) |>
    dplyr::select(index, year, month, value, unit)
  write.csv(data_pp, file.path(data_destination, "PP_integrated_processed.csv"), row.names = FALSE)

  # PP values between 1980 - 1997 are the same, so
  # environmental covariates will be considered from
  # 1998 onwards
  pp_years <- 1998:2023
  pp_annual <- data_pp |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      pp_mean = mean(value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(year) |>
    # PP values between 1980-1997 are the same, exclude
    # them from the analysis
    dplyr::filter(year %in% pp_years)

  data_environment <- ecosystemom::load_csv_environmental_data(
    file_path = file.path(data_destination, "PP_integrated_processed.csv"),
    lag_months = 0,
    impacted_group = "Menhaden (0yr)"
  )
} else {
  data_environment <- NULL
}

# Load diet composition data
data_diet_composition <- ecosystemom::load_diet_composition(
  file.path(data_destination, "1-Diet composition.csv")
)

# Combine all inputs into a single object for SEM
data_dsem <- tibble::tibble(
  data_om = list(data_om),
  data_environment = list(data_environment),
  data_diet_composition = list(data_diet_composition)
)

# Calculate truth for Menhaden
truth_om <- ecosystemom::get_truth(
  data_om,
  species_name = "Menhaden"
)

# Extract and unnest annual catch
catch_index_om <- truth_om |>
  dplyr::filter(
    truth_label == "catch",
    truth_type == "index",
    truth_time_step == "yearly") |>
  tidyr::unnest(cols = c(truth_om)) |>
  dplyr::mutate(
    truth_value = truth_value * biomass_scalar,
    truth_unit = "mt"
  )

# Extract and unnest annual weight-at-age
mean_weight_agecomp_om <- truth_om |>
  dplyr::filter(
    truth_label == "weight",
    truth_type == "agecomp",
    truth_time_step == "yearly"
  ) |>
  tidyr::unnest(cols = c(truth_om)) |>
  dplyr::mutate(truth_year = NA) |>
  dplyr::group_by(truth_group) |>
  dplyr::mutate(truth_value = mean(truth_value, na.rm = TRUE)) |>
  dplyr::distinct() |>
  dplyr::ungroup()

weight_agecomp_om <- truth_om |>
  dplyr::filter(
    truth_label == "weight",
    truth_type == "agecomp",
    truth_time_step == "yearly"
  ) |>
  tidyr::unnest(cols = c(truth_om)) |>
  # TODO: double check unit of weight
  dplyr::mutate(
    truth_value = truth_value * weight_scalar,
    truth_unit = "mt"
  )

average_weight_agecomp_om <- weight_agecomp_om |>
  dplyr::group_by(species_name, truth_group) |>
  dplyr::summarise(
    avg_weight = mean(truth_value, na.rm = TRUE),
    .groups = "drop"
  )

# Extract and unnest annual catch-at-age in numbers
catch_agecomp_om <- truth_om |>
  dplyr::filter(
    truth_label == "catch",
    truth_type == "agecomp",
    truth_time_step == "yearly"
  ) |>
  tidyr::unnest(cols = c(truth_om)) |>
  dplyr::mutate(
    truth_value = truth_value * biomass_scalar,
    truth_unit = "mt"
  ) |>
  dplyr::left_join(
    weight_agecomp_om |>
      dplyr::select(-species_name, -truth_label, -truth_type, -truth_time_step, -truth_unit),
    by = c("truth_year", "truth_group"),
    suffix = c("_catch", "_weight")
  ) |>
  dplyr::mutate(
    truth_value = ceiling(truth_value_catch / truth_value_weight),
    truth_unit = "numbers"
  ) |>
  dplyr::select(-truth_value_catch, -truth_value_weight)

# Extract and unnest annual biomass
biomass_index_om <- truth_om |>
  dplyr::filter(
    truth_label == "biomass",
    truth_type == "index",
    truth_time_step == "yearly"
  ) |>
  tidyr::unnest(cols = c(truth_om)) |>
  dplyr::mutate(
    truth_value = truth_value * biomass_scalar,
    truth_unit = "mt"
  )

# Extract and unnest annual number-at-age
number_agecomp_om <- truth_om |>
  dplyr::filter(
    truth_label == "numbers",
    truth_type == "agecomp",
    truth_time_step == "yearly"
  ) |>
  tidyr::unnest(cols = c(truth_om)) |>
  dplyr::mutate(
    truth_value = ceiling(truth_value * biomass_scalar / weight_scalar),
    truth_unit = "numbers"
  )

# Extract and unnest annual natural mortality by age
natural_mortality_agecomp_om <- truth_om |>
  dplyr::filter(
    truth_label == "natural_mortality",
    truth_type == "agecomp",
    truth_time_step == "yearly"
  ) |>
  tidyr::unnest(cols = c(truth_om))

# Extract and unnest annual fishing mortality by age
fishing_mortality_agecomp_om <- truth_om |>
  dplyr::filter(
    truth_label == "fishing_mortality",
    truth_type == "agecomp",
    truth_time_step == "yearly"
  ) |>
  tidyr::unnest(cols = c(truth_om))

average_natural_mortality_agecomp_om <- natural_mortality_agecomp_om |>
  dplyr::group_by(species_name, truth_group) |>
  dplyr::summarise(
    avg_M = mean(truth_value, na.rm = TRUE),
    .groups = "drop"
  )

# Option 1 (not used): Estimate time-varying selectivity from fishing mortality-at-age
catch_selectivity <- ecosystemom::estimate_true_selectivity(
  data = fishing_mortality_agecomp_om,
  ages = ages,
  functional_form = "double_logistic"
) |>
  dplyr::mutate(fleet_name = fishing_fleet_name)

# Option 2: time-invariant double-logistic selectivity
catch_selectivity_inflection_point_asc <- 1.4
catch_selectivity_slope_asc <- 3.5
catch_selectivity_inflection_point_desc <- 3.2
catch_selectivity_slope_desc <- 1.8

selectivity_ascending  <- 1 / (1 + exp(-catch_selectivity_slope_asc * (ages - catch_selectivity_inflection_point_asc)))
selectivity_descending <- 1 / (1 + exp(-catch_selectivity_slope_desc * (ages - catch_selectivity_inflection_point_desc)))
selectivity_catch_unscaled <- selectivity_ascending * (1 - selectivity_descending)
s_max_fims <- max(selectivity_catch_unscaled)

# Extract and unnest annual fishing mortality: apical F
fishing_mortality_index_om <- truth_om |>
  dplyr::filter(
    truth_label == "fishing_mortality",
    truth_type == "index",
    truth_time_step == "yearly"
  ) |>
  tidyr::unnest(cols = c(truth_om))

#### ---------- Generate "Data" from OM for testing EMs ####

# Catch index with lognormal error (sd = 0.05)
catch_index_sampled <- catch_index_om |>
  dplyr::mutate(
    sampled_value = ecosystemom::sample_lognormal(
      x = truth_value,
      sd = catch_index_sd
    )
  )

# Catch age composition (multinomial sampling)
catch_agecomp_sampled <- catch_agecomp_om |>
  dplyr::group_by(truth_year) |>
  dplyr::mutate(
    sampled_value = ecosystemom::sample_multinomial(
      x = truth_value,
      sample_size = catch_agecomp_sample_size
    )
  ) |>
  dplyr::ungroup()

# Create survey (Fishery-independent survey)
survey_data <- number_agecomp_om |>
  dplyr::left_join(
    weight_agecomp_om |>
      dplyr::select(-species_name, -truth_label, -truth_type, -truth_time_step, -truth_unit),
    by = c("truth_year", "truth_group"),
    suffix = c("_number", "_weight")
  ) |>
  dplyr::mutate(
    selectivity = selectivity_survey[truth_group],
    truth_value_selected_number = ceiling(truth_value_number * selectivity * catchability_survey),
    truth_value_selected_biomass = truth_value_selected_number * truth_value_weight
  )

# Survey index
survey_index_sampled <- survey_data |>
  dplyr::select(
    -truth_value_number, -truth_value_weight, -selectivity, -truth_value_selected_number
  ) |>
  dplyr::mutate(
    truth_label = "biomass",
    truth_unit = "mt"
  ) |>
  # Aggregate all ages/groups into one annual value
  dplyr::group_by(truth_year) |>
  dplyr::summarise(
    truth_value = sum(truth_value_selected_biomass),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    species_name = "Menhaden",
    truth_label = "biomass",
    truth_type = "index",
    truth_time_step = "yearly",
    truth_unit = "mt"
  ) |>
  dplyr::mutate(
    sampled_value = ecosystemom::sample_lognormal(
      x = truth_value,
      sd = survey_index_sd
    )
  )

# Survey agecomp
survey_agecomp_sampled <- survey_data |>
  dplyr::group_by(truth_year) |>
  dplyr::mutate(
    sampled_value = ecosystemom::sample_multinomial(
      x = truth_value_selected_number,
      sample_size = survey_agecomp_sample_size
    )
  ) |>
  dplyr::ungroup() |>
  dplyr::select(
    -truth_value_number, -truth_value_weight, -selectivity,
    -truth_value_selected_biomass
  ) |>
  dplyr::rename(truth_value = truth_value_selected_number)

# PP index
pp_fleet_name <- "pp_fleet"
pp_index_sd <- 0.1
if (egg_production) {
  pp_index_sampled <- pp_annual |>
    dplyr::mutate(
      sampled_value = ecosystemom::sample_lognormal(
        x = pp_mean,
        sd = pp_index_sd
      )
    )
}

# YOY survey for age-0
yoy_fleet_name <- "yoy_fleet"
yoy_q <- 0.05
yoy_index_sd <- 0.1
yoy_selectivity <- c(1, 0, 0, 0, 0)
yoy_index_sampled <- number_agecomp_om |>
  dplyr::left_join(
    weight_agecomp_om |>
      dplyr::select(-species_name, -truth_label, -truth_type, -truth_time_step, -truth_unit),
    by = c("truth_year", "truth_group"),
    suffix = c("_number", "_weight")
  ) |>
  dplyr::filter(truth_group == "0yr") |>
  dplyr::mutate(
    truth_value_selected_number = ceiling(truth_value_number * yoy_q),
    truth_value_selected_biomass = truth_value_selected_number * truth_value_weight,
    sampled_value = ecosystemom::sample_lognormal(
      x = truth_value_selected_biomass,
      sd = yoy_index_sd
    )
  ) |>
  dplyr::select(
    -truth_value_number, -truth_value_weight,
    -truth_value_selected_number, -truth_value_selected_biomass
  ) |>
  dplyr::mutate(
    species_name = "menhaden",
    truth_label = "biomass",
    truth_type = "index",
    truth_time_step = "yearly",
    truth_unit = "mt"
  )
yoy_inflection_point_asc <- -1.0
yoy_slope_asc <- 10
yoy_inflection_point_desc <- 0.5
yoy_slope_desc <- 10
yoy_selectivity_ascending  <- 1 / (1 + exp(-yoy_slope_asc * (ages - yoy_inflection_point_asc)))
yoy_selectivity_descending <- 1 / (1 + exp(-yoy_slope_desc * (ages - yoy_inflection_point_desc)))
yoy_selectivity <- yoy_selectivity_ascending * (1 - yoy_selectivity_descending)


#### ---------- Run FIMS ####
source(file.path("Rscript", "prepare_fims_data.R"))
source(file.path("Rscript", "prepare_fims_parameters.R"))

# Initialize and fit each scenario without changing the full input data.
fit_fims_scenario <- function(params, scenario_name) {
  cat("#### Running scenario ", scenario_name, "\n")
  FIMS::clear()
  scenario_data <- data_fims@data |>
    dplyr::filter(fleet %in% params$fleet)
  if (scenario_name == "scenario_with_pp_partial_yoy") {
    scenario_data <- scenario_data |>
      dplyr::filter(!(fleet == yoy_fleet_name & timing %in% pp_years))
  }

  params |>
    FIMS::initialize_fims(data = FIMS::FIMSFrame(scenario_data)) |>
    FIMS::fit_fims(
      optimize = TRUE,
      control = list(eval.max = 50000, iter.max = 30000, trace = 0)
    ) 
}

fit_fims_fixed_effects_log_devs <- Map(
  fit_fims_scenario,
  parameters_fixed_effects_logdevs,
  names(parameters_fixed_effects_logdevs)
)
fit_fims_random_effects_log_devs <- Map(
  fit_fims_scenario,
  parameters_scenarios,
  names(parameters_scenarios)
)

# Combine fitted FIMS scenarios into a single list for further processing.
fit_scenarios <- c(
  stats::setNames(
    fit_fims_fixed_effects_log_devs,
    paste("Fixed effects", names(parameters_fixed_effects_logdevs), sep = ": ")
  ),
  stats::setNames(
    fit_fims_random_effects_log_devs,
    paste("Random effects", names(parameters_scenarios), sep = ": ")
  )
)

# FIMS exposes convergence diagnostics through get_convergence().
check_convergence <- function(fit) {
  diagnostics <- FIMS::get_convergence(fit)
  required_checks <- c(
    "optimizer", "max_gradient", "nll", "hessian",
    "standard_errors", "large_standard_errors",
    "condition_number"
  )
  all(required_checks %in% diagnostics$check) &&
    !anyNA(diagnostics$severity) &&
    all(diagnostics$severity[diagnostics$check %in% required_checks] == "OK") &&
    !any(diagnostics$severity == "FAIL")
}
scenario_converged <- vapply(fit_scenarios, check_convergence, logical(1))

# Extract estimates from each fitted FIMS scenario and combine them into a single data frame.
estimates_fims <- purrr::imap_dfr(fit_scenarios, function(fit, scenario) {
  estimates <- FIMS::get_estimates(fit) |>
    dplyr::mutate(
      scenario = scenario,
      uncertainty_label = "se",
      estimate = estimated
    )

  selectivity <- estimates |>
    dplyr::filter(
      module_name == "Selectivity",
      fleet == fishing_fleet_name,
      label %in% c(
        "inflection_point_asc", "slope_asc",
        "inflection_point_desc", "slope_desc"
      )
    )
  if (nrow(selectivity) != 4L || anyDuplicated(selectivity$label)) {
    stop("Expected four fishing-fleet selectivity parameters for ", scenario)
  }
  selectivity_parameters <- stats::setNames(
    selectivity$estimated, selectivity$label
  )
  s_asc <- stats::plogis(
    selectivity_parameters[["slope_asc"]] *
      (ages - selectivity_parameters[["inflection_point_asc"]])
  )
  s_desc <- stats::plogis(
    selectivity_parameters[["slope_desc"]] *
      (ages - selectivity_parameters[["inflection_point_desc"]])
  )
  s_max_estimated <- max(s_asc * (1 - s_desc))
  if (!is.finite(s_max_estimated) || s_max_estimated <= 0) {
    stop("Invalid peak fishing-fleet selectivity for ", scenario)
  }

  # Convert fishing-fleet F to apical F separately for each fitted scenario.
  estimates |>
    dplyr::mutate(
      estimate = dplyr::if_else(
        label == "log_Fmort" & fleet == fishing_fleet_name,
        estimate + log(s_max_estimated),
        estimate
      )
    )
})
FIMS::clear()

# Print combined FIMS estimates for fixed and random effects scenarios.
estimates_fims |>
  dplyr::filter(estimation_type == "fixed_effects" | estimation_type == "random_effects") |>
  dplyr::select(scenario, module_name, label, fleet, year, age, input, estimate, uncertainty) |>
  print(n = Inf)

comparison_om <- dplyr::bind_rows(
  biomass_index_om |>
    dplyr::transmute(
      year = truth_year,
      estimate = truth_value,
      label = "biomass"
    ),
  number_agecomp_om |>
    dplyr::filter(truth_group == "0yr") |>
    dplyr::transmute(
      year = truth_year,
      estimate = truth_value,
      label = "expected_recruitment"
    ),
  fishing_mortality_index_om |>
    dplyr::transmute(
      year = truth_year,
      estimate = log(truth_value),
      label = "log_Fmort"
    )
)

comparison_labels <- c(
  biomass = "Biomass (metric tons)",
  expected_recruitment = "Recruits (numbers)",
  log_Fmort = "Natural log of fishing mortality"
)

comparison_em <- purrr::map_dfr(names(comparison_labels), function(quantity_label) {
  stockplotr::filter_data(
    estimates_fims |>
      dplyr::filter(
        year %in% years,
        label == quantity_label,
        label != "log_Fmort" | fleet == fishing_fleet_name
      ) |>
      dplyr::mutate(
        uncertainty = dplyr::if_else(
          is.finite(uncertainty) & uncertainty >= 0, uncertainty, NA_real_
        )
      ),
    label_name = paste0("^", quantity_label, "$"),
    geom = "line",
    interactive = FALSE
  ) |>
    dplyr::mutate(
      label = quantity_label,
      estimate_lower = dplyr::if_else(
        label == "log_Fmort", estimate_lower, pmax(0, estimate_lower)
      )
    )
})

plot_scenario_comparison <- function(
  scenario_data, scenario_name, plot_warnings, show_scenario_title = TRUE
) {
  stockplotr::plot_timeseries(
    scenario_data,
    x = "year",
    y = "estimate"
  ) +
    ggplot2::aes(linetype = group_var) +
    ggplot2::facet_wrap(~quantity, ncol = 1, scales = "free_y") +
    ggplot2::scale_color_manual(
      name = "Model", values = c(EM = "black", OM = "#003087")
    ) +
    ggplot2::scale_fill_manual(
      values = c(EM = "black", OM = "#003087"), guide = "none"
    ) +
    ggplot2::scale_linetype_manual(
      name = "Model", values = c(EM = "solid", OM = "dashed")
    ) +
    ggplot2::guides(linetype = ggplot2::guide_legend()) +
    stockplotr::theme_noaa() +
    ggplot2::labs(
      title = if (show_scenario_title) gsub("_", " ", scenario_name) else NULL,
      subtitle = paste(plot_warnings, collapse = "; "),
      x = "Year",
      y = NULL
    ) +
    ggplot2::theme(
      plot.subtitle = ggplot2::element_text(color = "red", face = "bold"),
      legend.position = "bottom"
    )
}

scenario_results <- lapply(names(fit_scenarios), function(scenario_name) {
  scenario_estimates <- comparison_em |>
    dplyr::filter(scenario == scenario_name)
  if (
    nrow(scenario_estimates) != length(years) * length(comparison_labels) ||
      anyDuplicated(scenario_estimates[c("year", "label")]) ||
      any(!is.finite(scenario_estimates$estimate))
  ) {
    stop("Missing, duplicated, or non-finite comparison estimates for ", scenario_name)
  }

  uncertainty_available <- is.finite(scenario_estimates$estimate_lower) &
    is.finite(scenario_estimates$estimate_upper)
  if (any(!uncertainty_available)) {
    warning(
      "Uncertainty unavailable for some EM estimates in ", scenario_name,
      call. = FALSE
    )
    scenario_estimates$estimate_lower[!uncertainty_available] <- NA_real_
    scenario_estimates$estimate_upper[!uncertainty_available] <- NA_real_
  }
  plot_warnings <- c(
    if (!scenario_converged[[scenario_name]]) "EM not converged",
    if (any(!uncertainty_available)) "Uncertainty unavailable for some EM estimates"
  )

  scenario_data <- dplyr::bind_rows(
    scenario_estimates |>
      dplyr::mutate(group_var = "EM"),
    comparison_om |>
      dplyr::mutate(group_var = "OM")
  ) |>
    dplyr::mutate(
      model = "1",
      quantity = factor(
        label,
        levels = names(comparison_labels),
        labels = unname(comparison_labels)
      )
    )

  list(
    data = scenario_data,
    warnings = plot_warnings,
    figure = plot_scenario_comparison(
      scenario_data, scenario_name, plot_warnings
    )
  )
})
names(scenario_results) <- names(fit_scenarios)
scenario_figures <- purrr::map(scenario_results, "figure")

# One PDF page per scenario, with all three OM/EM comparisons.
fs::dir_create(figures_path)
purrr::iwalk(scenario_results, function(result, scenario_name) {
  scenario_file_name <- gsub("[^A-Za-z0-9_-]+", "_", scenario_name)
  purrr::walk(names(comparison_labels), function(quantity_name) {
    individual_data <- result$data |>
      dplyr::filter(label == quantity_name)
    individual_figure <- plot_scenario_comparison(
      individual_data, scenario_name, result$warnings,
      show_scenario_title = FALSE
    )
    ggplot2::ggsave(
      filename = paste0(
        "fims_scenario_comparison_", scenario_file_name, "_", quantity_name, ".png"
      ),
      path = figures_path,
      plot = individual_figure,
      width = 10,
      height = 6,
      dpi = 300
    )
  })
})

scenario_pdf <- file.path(figures_path, "fims_scenario_comparisons.pdf")
grDevices::pdf(scenario_pdf, width = 10, height = 11, onefile = TRUE)
tryCatch(
  invisible(lapply(scenario_figures, print)),
  finally = grDevices::dev.off()
)

# plot data and save the figure
plot(data_fims)
ggsave(file.path(figures_path, "data_fims_plot.png"))

#### ---------- Plot data ####
biomass_index_figure <- ggplot2::ggplot(
  biomass_index_om,
  ggplot2::aes(x = truth_year, y = truth_value)
) +
  ggplot2::geom_line(color = "darkgreen", linewidth = 1.2) +
  ggplot2::geom_point(color = "darkgreen") +
  ggplot2::theme_minimal() +
  ggplot2::labs(
    title = "Operating Model: True Biomass Trend",
    x = "Year",
    y = "Biomass"
  )
ggplot2::ggsave(
  filename = "biomass_index_om.png",
  path = figures_path,
  plot = biomass_index_figure,
  width = 8,
  height = 6,
  dpi = 1200
)

numbers_at_age_om <- truth_om |>
  dplyr::filter(
    truth_label == "numbers",
    truth_type == "agecomp",
    truth_time_step == "yearly") |>
  tidyr::unnest(cols = c(truth_om)) |>
  dplyr::group_by(truth_year) |>
  # Normalize both columns to proportions (0 to 1)
  dplyr::mutate(
    truth_prop = truth_value / sum(truth_value)
  ) |>
  dplyr::ungroup() |>
  dplyr::select(truth_year, truth_label, truth_group, truth_value, truth_prop)

numbers_at_age_figure <- ggplot2::ggplot(
  numbers_at_age_om, ggplot2::aes(x = truth_group, y = truth_prop)
) +
  ggplot2::geom_bar(stat = "identity", fill = "gray40", alpha = 0.8) +
  ggplot2::facet_wrap(~truth_year) +
  ggplot2::theme_minimal() +
  ggplot2::labs(
    title = "Operating Model: True Age Composition",
    x = "Age",
    y = "Proportion"
  )
ggplot2::ggsave(
  filename = "numbers_at_age_om.png",
  path = figures_path,
  plot = numbers_at_age_figure,
  width = 8,
  height = 6,
  dpi = 1200
)

plot_index_comparison(
  survey_index_sampled,
  file_path = figures_path,
  "Survey Biomass Index"
)
selectivity_data <- data.frame(
  Age = ages,
  Selectivity = selectivity_survey
)
selectivity_figure <- ggplot2::ggplot(
  selectivity_data, ggplot2::aes(x = Age, y = Selectivity)
) +
  ggplot2::geom_line(color = "steelblue", linewidth = 1) +
  ggplot2:: geom_area(fill = "steelblue", alpha = 0.2) +
  ggplot2::theme_minimal() +
  ggplot2::labs(
    title = "Survey Selectivity: Double Logistic",
    x = "Age",
    y = "Selectivity"
  ) +
  ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2))

ggplot2::ggsave(
  filename = "survey_selectivity.png",
  path = figures_path,
  plot = selectivity_figure,
  width = 8,
  height = 6,
  dpi = 1200
)

plot_age_comp_normalized(
  survey_agecomp_sampled,
  title = "Survey Age Composition",
  file_path = figures_path
)

plot_index_comparison(
  catch_index_sampled,
  file_path = figures_path,
  "Fishery Catch Index"
)
plot_age_comp_normalized(
  catch_agecomp_sampled,
  file_path = figures_path,
  title = "Catch Age Composition"
)
plot_weight_trends(
  weight_agecomp_om,
  file_path = figures_path
)
plot_survey_vs_catch(
  survey_index_sampled,
  catch_index_sampled,
  file_path = figures_path
)

#### ---------- DSEM analysis ####
if (egg_production) {
  # Generate candidate SEM lines and reshape time series data
  # diet_composition_threshold controls which trophic links are included.
  # It could be changed. The current implementation uses static diet composition
  # from Ecopth->input->Diet composition, but it actually
  # changes over time in the EwE model as prey become more/less available.
  # TODO: support time-varying diet composition from EwE outputs?
  sem <- ecosystemom::create_dsem_inputs(
    data = data_dsem,
    focal_functional_group = "Menhaden (0yr)",
    diet_composition_threshold = 0.1
  )

  # Fit Dynamic Structural Equation Model (DSEM)
  # TODO: improve model specification beyond simple linear relationships
  fit_dsem <- dsem::dsem(
    sem = sem[["sem_lines"]],
    tsdata = sem[["data_time_series_sem"]][[1]],
    control = dsem::dsem_control(quiet = TRUE)
  )

  # Display model summary
  fit_dsem |>
    summary() |>
    dplyr::select(path, Estimate, Std_Error, p_value) |>
    knitr::kable(digits = 3)
}

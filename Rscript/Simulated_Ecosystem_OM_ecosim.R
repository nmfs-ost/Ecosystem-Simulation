# This script creates the simulated ecosystem operating model and gets the "true"
# values from it

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
pak::pkg_install(required_packages)

# Source utility scripts
source(file.path("Rscript", "utils.R"))
source(file.path("Rscript", "plot_data.R"))

#### ----------Set up hard coded values ####

# Define model years
years <- 1980:2023

# Define ages
ages <- 0:4

# Define fishing fleet
# Define fleet name
fishing_fleet_name <- "fishing_fleet"
# Define the uncertainty of sampled catch observations
catch_index_sd <- 0.05
catch_agecomp_sample_size <- 200

# Define survey fleet
# Define fleet name
survey_fleet_name <- "survey_fleet"
# TODO:
# - Define realistic catchability and selectivity patterns
# Define survey catchability
catchability_survey <- 0.01

# Survey selectivity: Double logistic selectivity
selectivity_inflection_point_asc_survey <- -2
selectivity_slope_asc_survey <- 10
selectivity_inflection_point_desc_survey <- 1.5
selectivity_slope_desc_survey <- 2.0

selectivity_module_survey <- methods::new(FIMS::DoubleLogisticSelectivity)
selectivity_module_survey$inflection_point_asc[1]$value <- selectivity_inflection_point_asc_survey
selectivity_module_survey$slope_asc[1]$value <- selectivity_slope_asc_survey
selectivity_module_survey$inflection_point_desc[1]$value <- selectivity_inflection_point_desc_survey
selectivity_module_survey$slope_desc[1]$value <- selectivity_slope_desc_survey

selectivity_survey <- purrr::map_dbl(ages, ~selectivity_module_survey$evaluate(.x))
FIMS::clear()

# Define the uncertainty of sampled survey observations
survey_index_sd <- 0.1
survey_agecomp_sample_size <- 200

#### ----------Initializing input files and directories ####

# Local directory for downloaded data
data_destination <- file.path(
  getwd(), "data", "ecosim_sefsc"
)

# Download data only if directory is missing or empty
if (!dir.exists(data_destination) || length(list.files(data_destination)) == 0) {

  message("Data directory is empty. Starting Google Drive download...")

  # Authenticate with Google Drive (OAuth flow)
  googledrive::drive_auth(scopes = "https://www.googleapis.com/auth/drive")

  # Google Drive folder ID
  ecosim_sefsc_id <- googledrive::as_id("1dDj8RzHSDyaG19N9OgPV371vdRzZY7e8")

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

# Load environmental data (TEMPORARY)
# TODO: this generates fake SST data and should be removed later
data_sst <- generate_sst_data(years_vector = years)
data_environment <- ecosystemom::load_csv_environmental_data(
  file_path = file.path(data_destination, "simulated_sst.csv"),
  lag_months = 12,
  impacted_group = "Menhaden (0yr)"
)

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
  tidyr::unnest(cols = c(truth_om))

# Extract and unnest annual weight-at-age
weight_agecomp_om <- truth_om |>
  dplyr::filter(
    truth_label == "weight",
    truth_type == "agecomp",
    truth_time_step == "yearly"
  ) |> 
  tidyr::unnest(cols = c(truth_om)) |>
  # TODO: double check unit of weight
  dplyr::mutate(
    truth_value = truth_value / 1000,
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
    truth_value = truth_value, 
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
    truth_value = ceiling(truth_value / 1000),
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

average_natural_mortality_agecomp_om <- natural_mortality_agecomp_om |>
  dplyr::group_by(species_name, truth_group) |>
  dplyr::summarise(
    avg_M = mean(truth_value, na.rm = TRUE),
    .groups = "drop"
  )

# Extract and unnest annual fishing mortality by age
fishing_mortality_agecomp_om <- truth_om |>
  dplyr::filter(
    truth_label == "fishing_mortality",
    truth_type == "agecomp",
    truth_time_step == "yearly"
  ) |> 
  tidyr::unnest(cols = c(truth_om))

# Estimate selectivity from fishing mortality-at-age
catch_selectivity <- ecosystemom::estimate_true_selectivity(
  data = fishing_mortality_agecomp_om,
  ages = ages,
  functional_form = "double_logistic"
) |>
  dplyr::mutate(fleet_name = fishing_fleet_name)

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

# Create survey
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

#### ---------- Run FIMS ####
source(file.path("Rscript", "prepare_fims_data.R"))
source(file.path("Rscript", "prepare_fims_parameters.R"))

# Initialize and fit the FIMS estimation model
fit_fims <- parameters |>
  FIMS::initialize_fims((data = data_fims)) |>
  FIMS::fit_fims(optimize = TRUE)

# Extract estimates
estimates_fims <- FIMS::get_estimates(fit_fims)

FIMS::clear()

# Compare OM and FIMS
biomass_fims <- estimates_fims |>
  dplyr::filter(label == "biomass") |>
  dplyr::mutate(year = years[year_i]) |>
  dplyr::select(year, FIMS = estimated)

biomass_om <- biomass_index_om |>
  dplyr::select(year = truth_year, OM = truth_value)

ratio <- max(biomass_fims[["FIMS"]], na.rm = TRUE) / 
         max(biomass_om[["OM"]], na.rm = TRUE)

biomass_fims |> 
  dplyr::left_join(biomass_om, by = "year") |> 
  dplyr::filter(year %in% years) |>
  ggplot2::ggplot(ggplot2::aes(x = year)) +
  ggplot2::geom_line(ggplot2::aes(y = FIMS, color = "FIMS"), linewidth = 1.2) +
  ggplot2::geom_line(ggplot2::aes(y = OM * ratio, color = "OM"), linewidth = 1.2, linetype = "dashed") +
  ggplot2::scale_y_continuous(
    name = "FIMS Biomass",
    sec.axis = ggplot2::sec_axis(~ . / ratio, name = "OM Biomass")
  ) +
  ggplot2::scale_color_manual(values = c("FIMS" = "#1f77b4", "OM" = "#ff7f0e")) +
  ggplot2::theme_minimal(base_size = 14) +
  ggplot2::labs(
    x = "Model Year",
    color = "Source"
  )

recruitment_fims <- estimates_fims |>
  dplyr::filter(label == "expected_recruitment") |>
  dplyr::mutate(year = years[year_i]) |>
  dplyr::select(year, FIMS = estimated)

recruitment_om <- number_agecomp_om |>
  dplyr::filter(truth_group == "0yr") |>
  dplyr::select(year = truth_year, OM = truth_value)

ratio <- max(recruitment_fims[["FIMS"]], na.rm = TRUE) / 
         max(recruitment_om[["OM"]], na.rm = TRUE)

recruitment_fims |> 
  dplyr::left_join(recruitment_om, by = "year") |> 
  dplyr::filter(year %in% years) |>
  ggplot2::ggplot(ggplot2::aes(x = year)) +
  ggplot2::geom_line(ggplot2::aes(y = FIMS, color = "FIMS"), linewidth = 1.2) +
  ggplot2::geom_line(ggplot2::aes(y = OM * ratio, color = "OM"), linewidth = 1.2, linetype = "dashed") +
  ggplot2::scale_y_continuous(
    name = "FIMS Recruitment",
    sec.axis = ggplot2::sec_axis(~ . / ratio, name = "OM Recruitment")
  ) +
  ggplot2::scale_color_manual(values = c("FIMS" = "#1f77b4", "OM" = "#ff7f0e")) +
  ggplot2::theme_minimal(base_size = 14) +
  ggplot2::labs(
    x = "Model Year",
    color = "Source"
  )

f_fims <- estimates_fims |>
  dplyr::filter(label == "log_Fmort",  module_id == 1) |>
  dplyr::mutate(year = years[year_i]) |>
  dplyr::select(year, FIMS = estimated) |>
  dplyr::mutate(FIMS = exp(FIMS)) 

f_om <- fishing_mortality_index_om |>
  dplyr::select(year = truth_year, OM = truth_value)

ratio <- max(f_fims[["FIMS"]], na.rm = TRUE) / 
         max(f_om[["OM"]], na.rm = TRUE)

f_fims |> 
  dplyr::left_join(f_om, by = "year") |> 
  dplyr::filter(year %in% years) |>
  ggplot2::ggplot(ggplot2::aes(x = year)) +
  ggplot2::geom_line(ggplot2::aes(y = FIMS, color = "FIMS"), linewidth = 1.2) +
  ggplot2::geom_line(ggplot2::aes(y = OM * ratio, color = "OM"), linewidth = 1.2, linetype = "dashed") +
  ggplot2::scale_y_continuous(
    name = "FIMS Fishing Mortality",
    sec.axis = ggplot2::sec_axis(~ . / ratio, name = "OM Fishing Mortality")
  ) +
  ggplot2::scale_color_manual(values = c("FIMS" = "#1f77b4", "OM" = "#ff7f0e")) +
  ggplot2::theme_minimal(base_size = 14) +
  ggplot2::labs(
    x = "Model Year"
  )
#### ---------- Plot data ####
figures_path <- file.path(getwd(), "figures", "ecosim_sefsc")
fs::dir_create(figures_path)

biomass_index_om <- truth_om |>
  dplyr::filter(
    truth_label == "biomass",
    truth_type == "index",
    truth_time_step == "yearly"
  ) |>
  tidyr::unnest(cols = c(truth_om)) |>
  dplyr::select(truth_year, truth_label, truth_value)

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
# Generate candidate SEM lines and reshape time series data
# diet_composition_threshold controls which trophic links are included.
# It could be changed. The current implementation uses static diet composition
# from Ecopth->input->Diet composition, but it actually
# changes over time in the EwE model as prey become more/less available.
# TODO: support time-varying diet composition from EwE outputs?
sem <- ecosystemom::create_dsem_inputs(
  data = data_dsem,
  focal_functional_group = "Menhaden (0yr)",
  diet_composition_threshold = 0.05
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
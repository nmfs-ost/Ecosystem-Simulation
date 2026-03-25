# This script creates the simulated ecosystem operating model and gets the "true"
# values from it

#### ---------Setup, load packages ####
if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak")
}

# Required packages
# TODO: need to use the main branch of NOAA-FIMS/ecosystemdata later
required_packages <- c(
  "fs",
  "ggplot2",
  # For downloading EwE ouputs from a Google Drive folder
  "googledrive",
  "James-Thorson-NOAA/dsem",
  # For standardizing EwE output and simulating observations
  # TODO: Install main branch of {ecosystemdata} after merging the
  # add-dsem-example branch into main
  # "NOAA-FIMS/ecosystemdata",
  "NOAA-FIMS/ecosystemom",
  # For generating selectivity curves
  "NOAA-FIMS/FIMS",
  "purrr",
  # TODO: remove {remotes} after merging the add-dsem-example branch into main
  "remotes"
)

# Install required packages
pak::pkg_install(required_packages)
# TODO: delete the line below once merge the add-dsem-example
# branch into main
remotes::install_github("NOAA-FIMS/ecosystemdata@add-dsem-example", force = TRUE)

# Source utility scripts
source(file.path("Rscript", "utils.R"))
source(file.path("Rscript", "plot_data.R"))

#### ----------Set up hard coded values ####

# Define model years
years <- 1980:2023

# Define ages
ages <- 0:4

# Define survey catchability
catchability_survey <- 0.01

# Survey selectivity: Double logistic selectivity
selectivity_module_survey <- methods::new(FIMS::DoubleLogisticSelectivity)
selectivity_module_survey$inflection_point_asc[1]$value <- -2
selectivity_module_survey$slope_asc[1]$value <- 10
selectivity_module_survey$inflection_point_desc[1]$value <- 1.5
selectivity_module_survey$slope_desc[1]$value <- 2.0

selectivity_survey <- purrr::map_dbl(ages, ~selectivity_module_survey$evaluate(.x))
FIMS::clear()

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
functional_groups <- ecosystemdata::get_functional_groups(
  file_path = file.path(data_destination, "1-Basic estimates.csv")
)

functional_groups |> print(n = 100)

# TODO:
# - Add units for biomass, catch, and weight
# - Confirm whether discard data are included
# Load EwE model output
data_om <- ecosystemdata::load_model(
  directory = data_destination,
  type = "ewe",
  functional_groups = functional_groups
) |>
  # TODO: define year range
  dplyr::filter(year %in% years)

# Load environmental data (TEMPORARY)
# TODO: this generates fake SST data and should be removed later
data_sst <- generate_sst_data(years_vector = years)
data_environment <- ecosystemdata::load_csv_environmental_data(
  file_path = file.path(data_destination, "simulated_sst.csv"),
  lag_months = 12,
  impacted_group = "Menhaden (0yr)"
)

# Load diet composition data
data_diet_composition <- ecosystemdata::load_csv_diet_composition(
  file.path(data_destination, "1-Diet composition.csv")
)

# Combine all inputs into a single object for SEM
data <- tibble::tibble(
  data_om = list(data_om),
  data_environment = list(data_environment),
  data_diet_composition = list(data_diet_composition)
)

# Calculate truth for Menhaden
truth_om <- ecosystemom::calc_truth(
  data_om,
  species_name = "Menhaden"
)

# Catch index
catch_index_om <- truth_om |>
  dplyr::filter(
    truth_label == "catch",
    truth_type == "index",
    truth_time_step == "yearly") |>
  tidyr::unnest(cols = c(truth_om)) |>
  dplyr::select(truth_year, truth_label, truth_value)

# Catch age composition
catch_agecomp_om <- truth_om |>
  dplyr::filter(
    truth_label == "catch",
    truth_type == "agecomp",
    truth_time_step == "yearly") |>
  tidyr::unnest(cols = c(truth_om)) |>
  dplyr::select(truth_year, truth_label, truth_group, truth_value)

# Numbers index
numbers_om <- truth_om |>
  dplyr::filter(
    truth_label == "numbers",
    truth_type == "index",
    truth_time_step == "yearly"
  ) |>
  tidyr::unnest(cols = c(truth_om)) |>
  dplyr::select(truth_year, truth_label, truth_value)

# Weight-at-age
weight_om <- truth_om |>
  dplyr::filter(
  truth_label == "weight",
  truth_type == "agecomp",
  truth_time_step == "yearly"
) |>
  tidyr::unnest(cols = c(truth_om)) |>
  dplyr::select(truth_year, truth_label, truth_group, truth_value)

#### ---------- DSEM analysis ####
# Generate candidate SEM lines and reshape time series data
# diet_composition_threshold controls which trophic links are included.
# It could be changed. The current implementation uses static diet composition
# from Ecopth->input->Diet composition, but it actually
# changes over time in the EwE model as prey become more/less available.
# TODO: support time-varying diet composition from EwE outputs?
sem <- ecosystemdata::create_sem(
  data = data,
  focal_functional_group = "Menhaden (0yr)",
  diet_composition_threshold = 0.05
)

# Fit Dynamic Structural Equation Model (DSEM)
# TODO: improve model specification beyond simple linear relationships
fit <- dsem::dsem(
  sem = sem[["sem_lines"]],
  tsdata = sem[["data_time_series_sem"]][[1]],
  control = dsem::dsem_control(quiet = TRUE)
)

# Display model summary
fit |>
  summary() |>
  dplyr::select(path, Estimate, Std_Error, p_value) |>
  knitr::kable(digits = 3)

#### ---------- Generate "Data" from OM for testing EMs ####

# Catch index with lognormal error (sd = 0.05)
catch_lognomal <- catch_index_om |>
  dplyr::mutate(
    sampled_value = ecosystemom::sample_lognormal(
      x = truth_value,
      sd = 0.05
    )
  )


# Catch age composition (multinomial sampling)
catch_agecomp_multinomial <- catch_agecomp_om |>
  dplyr::group_by(truth_year) |>
  dplyr::mutate(
    sampled_value = ecosystemom::sample_multinomial(
      x = truth_value,
      sample_size = 200
    )
  ) |>
  dplyr::ungroup()

# Create survey
# TODO:
# - Define realistic catchability and selectivity patterns
# - Replace with ecosystemom::create_survey() wrapper
truth_group <- weight_om |>
  dplyr::pull(truth_group) |>
  unique()

# Build survey dataset
data_survey <- numbers_om |>
  tidyr::expand_grid(truth_group = truth_group) |>
  dplyr::left_join(
    weight_om |>
      dplyr::select(truth_year, truth_group, weight = truth_value),
    by = c("truth_year", "truth_group")
  ) |>
  dplyr::mutate(
    age = rep(ages, length.out = dplyr::n()),
    numbers = truth_value * selectivity_survey[age + 1] * catchability_survey,
    biomass = numbers * weight
  ) |>
  dplyr::select(-truth_value)

# Survey biomass index
biomass_lognomal_survey <- data_survey |>
  # Aggregate all ages/groups into one annual value
  dplyr::group_by(truth_year) |>
  dplyr::summarise(
    truth_value = sum(biomass),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    truth_label = "biomass",
    sampled_value = ecosystemom::sample_lognormal(x = truth_value, sd = 0.1)
  )

# Survey age composition
number_agecomp_multinomial_survey <- data_survey |>
  dplyr::group_by(truth_year) |>
  dplyr::mutate(
    sampled_value = ecosystemom::sample_multinomial(
      x = numbers,
      sample_size = 200
    )
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    truth_value = numbers
  ) |>
  dplyr::select(-c(weight, age, numbers, biomass))

#### ---------- Plot data ####
figures_path <- file.path(getwd(), "figures")
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
  path = file.path("figures"),
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
  path = file.path("figures"),
  plot = numbers_at_age_figure,
  width = 8,
  height = 6,
  dpi = 1200
)

plot_index_comparison(biomass_lognomal_survey, "Survey Biomass Index")
selectivity_data <- data.frame(
  Age = ages,
  Selectivity = selectivity_survey
)
selectivity_figure <- ggplot2::ggplot(
  selectivity_data, ggplot2::aes(x = Age, y = Selectivity)
) +
  ggplot2::geom_line(color = "steelblue", size = 1) +
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
  path = file.path("figures"),
  plot = selectivity_figure,
  width = 8,
  height = 6,
  dpi = 1200
)

plot_age_comp_normalized(
  number_agecomp_multinomial_survey,
  title = "Survey Age Composition"
)
plot_index_comparison(catch_lognomal, "Fishery Catch Index")
plot_age_comp_normalized(
  catch_agecomp_multinomial,
  title = "Catch Age Composition"
)
plot_weight_trends(weight_om)
plot_survey_vs_catch(biomass_lognomal_survey, catch_lognomal)

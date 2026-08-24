catch_data <- data.frame(
  type = "catch",
  fleet = fishing_fleet_name,
  age = NA,
  timing = years,
  observed = catch_index_sampled[["sampled_value"]],
  unit = "mt",
  uncertainty = paste(
    "~ dlnorm(meanlog = log_catch_expected, sdlog = ",
    catch_index_sd,
    ")"
  )
)

index_data <- rbind(
  data.frame(
    type = "index",
    fleet = yoy_fleet_name,
    age = NA,
    timing = years,
    observed = yoy_index_sampled[["sampled_value"]],
    unit = "mt",
    uncertainty = paste(
      "~ dlnorm(meanlog = log_index_expected, sdlog = ",
      yoy_index_sd,
      ")"
    )
  ),
  data.frame(
    type = "index",
    fleet = survey_fleet_name,
    age = NA,
    timing = years,
    observed = survey_index_sampled[["sampled_value"]],
    unit = "mt",
    uncertainty = paste(
      "~ dlnorm(meanlog = log_index_expected, sdlog = ",
      survey_index_sd,
      ")"
    )
  )
)

age_data <- rbind(
  data.frame(
    type = "age_comp",
    fleet = fishing_fleet_name,
    age = unname(ages[catch_agecomp_sampled[["truth_group"]]]),
    timing = catch_agecomp_sampled[["truth_year"]],
    observed = catch_agecomp_sampled[["sampled_value"]],
    unit = "number",
    uncertainty = paste(
      "~ dmultinom(prob = agecomp_proportion, size = ",
      catch_agecomp_sample_size,
      ")"
    )
  ),
  data.frame(
    type = "age_comp",
    fleet = survey_fleet_name,
    age = unname(ages[survey_agecomp_sampled[["truth_group"]]]),
    timing = survey_agecomp_sampled[["truth_year"]],
    observed = survey_agecomp_sampled[["sampled_value"]],
    unit = "number",
    uncertainty = paste(
      "~ dmultinom(prob = agecomp_proportion, size = ",
      survey_agecomp_sample_size,
      ")"
    )
  )
)

weight_at_age <- data.frame(
  type = "weight_at_age",
  fleet = fishing_fleet_name,
  age = unname(ages[weight_agecomp_om[["truth_group"]]]),
  timing = weight_agecomp_om[["truth_year"]],
  observed = weight_agecomp_om[["truth_value"]],
  unit = "mt",
  uncertainty = NA
)

weight_year_plus <- weight_at_age |>
  dplyr::filter(timing == max(years)) |>
  dplyr::mutate(timing = timing + 1)

weight_at_age_data <- dplyr::bind_rows(
  weight_at_age,
  weight_year_plus
)

data_fims <- rbind(catch_data, index_data, age_data, weight_at_age_data) |>
  dplyr::mutate(
    length = NA,
    .after = "age"
  ) |>
  FIMS::FIMSFrame()

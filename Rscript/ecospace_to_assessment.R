###############################################################################
## ecospace_to_assessment.R
##
## Turn Ecospace multi-stanza output into stock-assessment "truth":
##   1. Reconcile RegionAgeStructure (*_All_Weight / *_All_Number) with the
##      Ecospace annual biomass and catch files
##   2. Numbers-at-age and weight-at-age (start of year)
##   3. F-at-age (catch / biomass), Z-at-age (cohort decline), M = Z - F
##   4. Catch-at-age in numbers (Baranov) as a closure check against catch
##   5. M from the consumption matrix: M = M2 (predation, Q_pred,prey / B_prey)
##      + M0 (other mortality, Ecopath Z * (1 - EE)), compared with Z - F
##   Figures are written as PNGs and as one combined PDF.
##
## How the RegionAgeStructure files are laid out (worth knowing before using):
##   * ROWS are time snapshots, not months. Row "0 / Ecosim Base Values" is the
##     Ecopath/Ecosim initial age structure (different scaling, do not mix it
##     with the time rows). The remaining rows are written every 12 time steps
##     (Timestep 1, 13, 25, ...), i.e. one January snapshot per year.
##   * COLUMNS are MONTHLY AGE classes, not months of the year.
##     "Menhaden (0yr)_0 ... _11" are ages 0-11 months, "(1yr)_12 ... _23" are
##     ages 12-23 months, etc. The last column (here _101) is the plus-group
##     accumulator. That is why Menhaden (4yr) has 54 columns (months 48-101)
##     and Yellowedge grouper (0-3yr) has 36.
##   * Weight = body weight per individual relative to W-infinity (0-1).
##     Number = numbers per km2 scaled so that sum(W * N) over a stanza's
##     columns = stanza biomass in t/km2 (same units as Ecospace_*_Biomass.csv).
##     Weights by themselves therefore never add up to biomass or catch.
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

## ---- Settings ---------------------------------------------------------------
base_dir   <- file.path(getwd(), "data", "ecospace_sefsc") #"C:/Users/msw200/Documents/Ecopath/GIST/IBM/Base"
species    <- "Menhaden"          # prefix used in the group names / file names
region_tag <- "Region_All"        # which RegionAgeStructure file to use
Winf_g     <- 696                 # W-infinity used for the multi-stanza growth curve (g)
plus_age   <- 4                   # plus-group age (integer years) for the assessment
area_km2   <- NA                  # model area for totals (NA = per-km2 output only)
## Ecopath Basic Estimates for each stanza (current GIST_postcalibration model),
## used for M0 = Z * (1 - EE). Ecospace holds M0 constant unless M0 is forced.
ecopath_Z  <- c("Menhaden (0yr)" = 1.671,  "Menhaden (1yr)" = 1.5088,
                "Menhaden (2yr)" = 1.7264, "Menhaden (3yr)" = 1.5198,
                "Menhaden (4yr)" = 1.4169)
ecopath_EE <- c("Menhaden (0yr)" = 0.06093945, "Menhaden (1yr)" = 0.2074588,
                "Menhaden (2yr)" = 0.4250196,  "Menhaden (3yr)" = 0.323604,
                "Menhaden (4yr)" = 0.212864)
consumption_pattern <- "Ecospace_Annual_Average_(Region_0_)?Consumption\\.csv$"
out_dir    <- file.path(base_dir, "SA_outputs")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

## ---- Helpers ----------------------------------------------------------------
find_file <- function(pattern, dir = base_dir) {
  f <- list.files(dir, pattern = pattern, recursive = TRUE, full.names = TRUE)
  if (length(f) == 0) stop("No file matching '", pattern, "' under ", dir)
  if (length(f) > 1) message("Several matches for '", pattern, "', using: ", f[1],
                             "\n  (others: ", paste(f[-1], collapse = ", "), ")")
  f[1]
}

## EwE header block (key,value lines before "Timestep")
read_ewe_header <- function(lines, hdr_line) {
  h  <- lines[seq_len(hdr_line - 1)]
  kv <- strsplit(h[grepl(",", h)], ",", fixed = TRUE)
  vals <- vapply(kv, function(x) gsub('"', "", paste(x[-1], collapse = ",")), "")
  setNames(vals, gsub('"', "", vapply(kv, `[`, "", 1)))
}

## Read an AgeStructure_*_All_Weight/Number file into long format
read_agestructure <- function(file, value_name) {
  lines <- readLines(file, warn = FALSE)
  hdr   <- grep("^Timestep", lines)[1]
  meta  <- read_ewe_header(lines, hdr)
  dat   <- read.csv(text = lines[hdr:length(lines)], check.names = FALSE,
                    stringsAsFactors = FALSE, strip.white = TRUE)
  dat   <- dat[, nzchar(names(dat)), drop = FALSE]     # drop trailing empty col
  names(dat)[1:2] <- c("Timestep", "Region")
  long <- dat |>
    mutate(base_row = Region == "Ecosim Base Values") |>
    pivot_longer(-c(Timestep, Region, base_row), names_to = "col",
                 values_to = value_name) |>
    mutate(col    = gsub('"', "", col),
           stanza = sub("_[0-9]+$", "", col),
           age_m  = as.integer(sub(".*_", "", col)),
           !!value_name := as.numeric(.data[[value_name]])) |>
    select(-col)
  attr(long, "meta") <- meta
  long
}

## Read an Ecospace annual-average file (Biomass or Catch) into long format.
## Catch columns are "Fleet|Group": summed over fleets for each group.
read_annual <- function(file, value_name) {
  d <- read.csv(file, check.names = FALSE, stringsAsFactors = FALSE)
  d |>
    pivot_longer(-Year, names_to = "col", values_to = value_name) |>
    mutate(stanza = sub(".*\\|", "", col)) |>
    group_by(Year, stanza) |>
    summarise(!!value_name := sum(.data[[value_name]], na.rm = TRUE),
              .groups = "drop")
}

## ---- Read ---------------------------------------------------------------------
f_wt  <- find_file(paste0("AgeStructure_", species, "_", region_tag, "_Weight\\.csv$"))
f_num <- find_file(paste0("AgeStructure_", species, "_", region_tag, "_Number\\.csv$"))
f_bio <- find_file("Ecospace_Annual_Average_Biomass\\.csv$")
f_cat <- find_file("Ecospace_Annual_Average_Catch\\.csv$")

wt   <- read_agestructure(f_wt,  "W")
num  <- read_agestructure(f_num, "N")
meta <- attr(wt, "meta")
dt         <- as.numeric(meta["EcoSpaceTimeStepLength"])   # 1/12
start_year <- as.integer(meta["StartYear"])
steps_yr   <- round(1 / dt)

message("Age-structure run: ", meta["EcospaceScenario"], " | ", meta["ModelSource"],
        " | written ", meta["Date"])

ages <- wt |>
  inner_join(num, by = c("Timestep", "Region", "base_row", "stanza", "age_m")) |>
  filter(!base_row) |>
  mutate(step_in_year = (Timestep - 1) %% steps_yr,       # 0 = first step of year
         Year         = (Timestep - 1) %/% steps_yr + 1,  # model year index (1 = start_year)
         cal_year     = start_year + Year - 1,
         B            = W * N)                             # t/km2 per monthly-age class

if (any(ages$step_in_year != 0))
  message("Age structure is written more often than annually; using first step of each year.")
ages <- filter(ages, step_in_year == 0)

stanzas  <- unique(ages$stanza)                 # in column order = age order
max_m    <- max(ages$age_m)                     # plus accumulator column
stz_of_m <- ages |> distinct(age_m, stanza) |> arrange(age_m)

bio <- read_annual(f_bio, "Bavg") |> filter(stanza %in% stanzas)
cat <- read_annual(f_cat, "Catch") |> filter(stanza %in% stanzas)

## ---- 1. Reconciliation -----------------------------------------------------------
## Jan snapshot of sum(W*N) per stanza vs annual-average biomass (t/km2).
## Expect ratios near 1; deviations reflect snapshot-vs-annual-mean timing
## (largest for the age-0 stanza because of the seasonal recruitment pulse).
recon <- ages |>
  group_by(Timestep, Year, cal_year, stanza) |>
  summarise(B_snapshot = sum(B), N_snapshot = sum(N), .groups = "drop") |>
  left_join(bio, by = c("Year", "stanza")) |>
  mutate(ratio = Bavg / B_snapshot)

## Month-for-month: if the monthly Ecospace_Average_Biomass.csv is present,
## compare each snapshot with the biomass in the SAME time step. This removes
## the seasonal (snapshot vs annual mean) mismatch.
f_bio_m <- list.files(base_dir, "^Ecospace_Average_Biomass\\.csv$", recursive = TRUE,
                      full.names = TRUE)
if (length(f_bio_m)) {
  bio_m <- read.csv(f_bio_m[1], check.names = FALSE) |>
    pivot_longer(-TimeStep, names_to = "stanza", values_to = "Bmonth") |>
    filter(stanza %in% stanzas)
  recon <- recon |>
    left_join(bio_m, by = c("Timestep" = "TimeStep", "stanza")) |>
    mutate(ratio_month = Bmonth / B_snapshot)
}
recon <- mutate(recon, stanza = factor(stanza, levels = stanzas))

print(recon |> group_by(stanza) |>
        summarise(across(any_of(c("ratio", "ratio_month")),
                         list(mean = mean, max_dev = ~ max(abs(.x - 1)))),
                  .groups = "drop"))

## ---- 2. Numbers- and weight-at-age (start of year) ----------------------------------
ages <- ages |> mutate(age = pmin(age_m %/% 12, plus_age))

naa_waa <- ages |>
  group_by(Year, cal_year, age) |>
  summarise(N_model = sum(N),                  # model numbers per km2
            B_tkm2  = sum(B),                  # t/km2
            W_rel   = sum(B) / sum(N),         # N-weighted mean W/Winf
            .groups = "drop") |>
  mutate(W_g          = W_rel * Winf_g,
         N_per_km2    = N_model / (Winf_g / 1e6),   # real fish per km2
         N_total      = N_per_km2 * area_km2,
         B_total_t    = B_tkm2 * area_km2)

## ---- 3. Mortality ---------------------------------------------------------------------
## F by stanza: in EwE catch = F * B each step, so annual catch / annual-average
## biomass is the (biomass-weighted) annual instantaneous F. Values > 1 are fine.
F_stanza <- cat |>
  inner_join(bio, by = c("Year", "stanza")) |>
  mutate(F = Catch / Bavg)

Fmat <- F_stanza |> select(Year, stanza, F) |>
  pivot_wider(names_from = stanza, values_from = F) |> arrange(Year)

## N matrix: rows = model year (Jan snapshot), cols = monthly age 0..max_m
Nmat <- ages |> select(Year, age_m, N) |>
  pivot_wider(names_from = age_m, values_from = N) |> arrange(Year)
yrs  <- Nmat$Year
Nmat <- as.matrix(Nmat[, as.character(0:max_m)])

## F experienced by a fish aged m months at the start of year y, averaged over
## the 12 months it ages through (it can cross a stanza boundary during the year)
F_exposure <- function(y, m) {
  mm <- pmin(m + 0:(steps_yr - 1), max_m)
  st <- stz_of_m$stanza[match(mm, stz_of_m$age_m)]
  mean(unlist(Fmat[Fmat$Year == y, st]))
}

mort <- list()
for (i in seq_len(length(yrs) - 1)) {
  y <- yrs[i]
  if (yrs[i + 1] != y + 1 || !(y %in% Fmat$Year)) next
  N0 <- Nmat[i, ]; N1 <- Nmat[i + 1, ]
  for (a in 0:plus_age) {
    if (a < plus_age) {
      m0 <- 12 * a + 0:11                 # cohort at start of year y
      m1 <- m0 + 12                       # same cohort at start of y+1
      if (max(m1) >= max_m) stop("plus_age too high for the number of age columns")
      Z <- -log(sum(N1[m1 + 1]) / sum(N0[m0 + 1]))
    } else {
      m0 <- (12 * a):max_m                # plus group: everything >= a now
      Z  <- -log(sum(N1[(12 * (a + 1)):max_m + 1]) / sum(N0[m0 + 1]))
    }
    Fe <- sapply(m0, function(m) F_exposure(y, m))
    Fa <- sum(Fe * N0[m0 + 1]) / sum(N0[m0 + 1])
    mort[[length(mort) + 1]] <- data.frame(Year = y, age = a, Z = Z, F = Fa, M = Z - Fa)
  }
}
mort <- bind_rows(mort) |> mutate(cal_year = start_year + Year - 1)

if (any(mort$M < 0))
  warning(sum(mort$M < 0), " age-years with M < 0 (timing mismatch between annual ",
          "snapshots and annual-average F); monthly output would remove this.")

## ---- 4. Catch-at-age (Baranov) and closure check -----------------------------------
## Catch weight uses the mean of the cohort's weight now and next January,
## because fish (especially age 0 -> 1) grow a lot before they are caught.
nxt    <- naa_waa |> transmute(Year = Year - 1, age, W_next = W_g)
w_next <- bind_rows(
  nxt |> filter(age > 0) |> mutate(age = age - 1),  # cohort ages a -> a+1
  nxt |> filter(age == plus_age)                    # plus group stays plus
)
caa <- naa_waa |>
  inner_join(mort, by = c("Year", "cal_year", "age")) |>
  left_join(w_next, by = c("Year", "age")) |>
  mutate(W_catch_g     = (W_g + W_next) / 2,
         C_num_per_km2 = F / Z * N_per_km2 * (1 - exp(-Z)),
         C_t_per_km2   = C_num_per_km2 * W_catch_g / 1e6)

closure <- caa |> group_by(Year, cal_year) |>
  summarise(C_baranov = sum(C_t_per_km2), .groups = "drop") |>
  left_join(cat |> group_by(Year) |> summarise(C_ecospace = sum(Catch)), by = "Year") |>
  mutate(ratio = C_ecospace / C_baranov)
print(summary(closure$ratio))

## ---- Other-mortality file sanity check -----------------------------------------------
f_om <- list.files(base_dir, "Ecospace_Annual_Average_OtherMortalityLoss\\.csv$",
                   recursive = TRUE, full.names = TRUE)
if (length(f_om)) {
  om <- read.csv(f_om[1], check.names = FALSE)
  if (all(om[, -1] == 0))
    message("OtherMortalityLoss is all zeros: Ecospace does not populate it, ",
            "so M here comes from Z - F (includes predation M2 and other M0).")
}

## ---- 5. M from the consumption matrix -------------------------------------------------
## Consumption columns are "Predator|Prey" (t/km2/yr). Predation mortality on a
## stanza is M2 = sum over predators of Q(pred, prey) / B(prey). Adding Ecopath's
## M0 = Z * (1 - EE) gives total natural mortality by stanza, independent of the
## age-structure snapshots. It is then mapped to integer ages with the same
## cohort-exposure weighting used for F, so it is comparable with Z - F.
f_con <- list.files(base_dir, consumption_pattern, recursive = TRUE, full.names = TRUE)
M_cons <- NULL; M_compare <- NULL; M2_pred <- NULL
if (length(f_con)) {
  con <- read.csv(f_con[1], check.names = FALSE)
  con_long <- con |>
    pivot_longer(-Year, names_to = "col", values_to = "Q") |>
    mutate(pred = sub("\\|.*", "", col), stanza = sub(".*\\|", "", col)) |>
    filter(stanza %in% stanzas)

  M2_pred <- con_long |>                                  # M2 split by predator
    inner_join(bio, by = c("Year", "stanza")) |>
    mutate(M2 = Q / Bavg) |>
    select(Year, stanza, pred, M2)

  M_stanza <- M2_pred |>
    group_by(Year, stanza) |>
    summarise(M2 = sum(M2), .groups = "drop") |>
    mutate(M0 = (ecopath_Z * (1 - ecopath_EE))[stanza],
           M  = M2 + M0)
  if (any(is.na(M_stanza$M0)))
    warning("Missing ecopath_Z / ecopath_EE for: ",
            paste(unique(M_stanza$stanza[is.na(M_stanza$M0)]), collapse = ", "))

  Mmat <- M_stanza |> select(Year, stanza, M) |>
    pivot_wider(names_from = stanza, values_from = M) |> arrange(Year)
  M_exposure <- function(y, m) {
    mm <- pmin(m + 0:(steps_yr - 1), max_m)
    st <- stz_of_m$stanza[match(mm, stz_of_m$age_m)]
    mean(unlist(Mmat[Mmat$Year == y, st]))
  }
  M_cons <- list()
  for (i in seq_len(length(yrs) - 1)) {
    y <- yrs[i]
    if (!(y %in% Mmat$Year)) next
    N0 <- Nmat[i, ]
    for (a in 0:plus_age) {
      m0 <- if (a < plus_age) 12 * a + 0:11 else (12 * a):max_m
      Me <- sapply(m0, function(m) M_exposure(y, m))
      M_cons[[length(M_cons) + 1]] <-
        data.frame(Year = y, age = a, M_consumption = sum(Me * N0[m0 + 1]) / sum(N0[m0 + 1]))
    }
  }
  M_cons <- bind_rows(M_cons) |> mutate(cal_year = start_year + Year - 1)
  M_compare <- mort |> select(Year, cal_year, age, M_ZminusF = M) |>
    inner_join(M_cons, by = c("Year", "cal_year", "age"))

  print(M_compare |> group_by(age) |>
          summarise(M_ZminusF = mean(M_ZminusF), M_consumption = mean(M_consumption)))
  write.csv(M_stanza,  file.path(out_dir, paste0(species, "_M_by_stanza_consumption.csv")), row.names = FALSE)
  write.csv(M_compare, file.path(out_dir, paste0(species, "_M_comparison.csv")),            row.names = FALSE)
  write.csv(M2_pred,   file.path(out_dir, paste0(species, "_M2_by_predator.csv")),          row.names = FALSE)
} else {
  message("No consumption file found; skipping consumption-based M.")
}

## ---- Write --------------------------------------------------------------------------
write.csv(recon,    file.path(out_dir, paste0(species, "_reconciliation.csv")), row.names = FALSE)
write.csv(naa_waa,  file.path(out_dir, paste0(species, "_naa_waa.csv")),        row.names = FALSE)
write.csv(F_stanza, file.path(out_dir, paste0(species, "_F_by_stanza.csv")),    row.names = FALSE)
write.csv(mort,     file.path(out_dir, paste0(species, "_ZFM_at_age.csv")),     row.names = FALSE)
write.csv(caa,      file.path(out_dir, paste0(species, "_caa.csv")),            row.names = FALSE)
write.csv(closure,  file.path(out_dir, paste0(species, "_catch_closure.csv")),  row.names = FALSE)

## ---- Figures (manuscript style) ---------------------------------------------------------
## Colour-blind-checked categorical palette, assigned in fixed order; "Other" is grey.
pal_cat   <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300")
col_other <- "#a3a29c"
ink       <- "#1f1f1e"; ink2 <- "#52514e"; grid_col <- "#e6e5e0"

theme_ms <- function(base_size = 10) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title.position = "plot",
      plot.title    = element_text(face = "bold", size = rel(1.25), colour = ink,
                                   margin = margin(b = 4)),
      plot.subtitle = element_text(size = rel(0.95), colour = ink2, margin = margin(b = 10)),
      plot.caption  = element_text(size = rel(0.8), colour = ink2, hjust = 0,
                                   margin = margin(t = 8)),
      plot.caption.position = "plot",
      axis.title    = element_text(colour = ink2),
      axis.text     = element_text(colour = ink2),
      axis.line.x   = element_line(colour = "#bdbcb6", linewidth = 0.4),
      axis.ticks.x  = element_line(colour = "#bdbcb6", linewidth = 0.4),
      panel.grid.major.y = element_line(colour = grid_col, linewidth = 0.35),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.spacing = unit(1.1, "lines"),
      strip.text    = element_text(face = "bold", colour = ink, hjust = 0,
                                   size = rel(0.95), margin = margin(b = 4)),
      legend.position = "top",
      legend.justification = "left",
      legend.title  = element_text(colour = ink2, size = rel(0.9)),
      legend.text   = element_text(colour = ink, size = rel(0.9)),
      legend.key.width = unit(1.6, "lines"),
      plot.background = element_rect(fill = "white", colour = NA),
      plot.margin   = margin(12, 14, 10, 10)
    )
}

stanza_lab <- function(s) {                      # "Menhaden (2yr)" -> "Age-2 stanza (2yr)"
  inner <- sub("^.*\\((.*)\\)$", "\\1", s)
  factor(paste0(inner, " stanza"), levels = paste0(sub("^.*\\((.*)\\)$", "\\1", stanzas), " stanza"))
}
age_lab   <- function(a) factor(ifelse(a == plus_age, paste0(a, "+"), a),
                                levels = c(0:(plus_age - 1), paste0(plus_age, "+")))
yr_breaks <- scale_x_continuous(breaks = seq(1980, 2040, 10), expand = expansion(mult = c(0.01, 0.02)))
first_yr  <- min(naa_waa$cal_year); last_yr <- max(naa_waa$cal_year)
period    <- paste0(first_yr, "-", last_yr)

plots <- list()

## Reconciliation figures: one against the annual average, one month-for-month
recon_fig <- function(eco_col, eco_label, subtitle, caption = NULL) {
  d <- recon |>
    transmute(cal_year, stanza = stanza_lab(as.character(stanza)),
              ecospace = .data[[eco_col]], rebuilt = B_snapshot) |>
    pivot_longer(c(ecospace, rebuilt), names_to = "series") |>
    mutate(series = factor(ifelse(series == "ecospace", eco_label,
                                  "Sum of weight x number (age-structure file)"),
                           levels = c(eco_label, "Sum of weight x number (age-structure file)")))
  ggplot(d, aes(cal_year, value, colour = series, linetype = series)) +
    geom_line(linewidth = 0.7) +
    facet_wrap(~stanza, scales = "free_y", ncol = 3) +
    scale_colour_manual(values = pal_cat[1:2], name = NULL) +
    scale_linetype_manual(values = c("solid", "22"), name = NULL) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.08))) +
    yr_breaks +
    labs(title = paste(species, "age-structure output vs. Ecospace stanza biomass"),
         subtitle = subtitle, x = NULL, y = expression("Biomass (t km"^-2*")"),
         caption = caption) +
    theme_ms() + guides(colour = guide_legend(nrow = 1))
}

## Combined figure (original file name): both Ecospace comparisons on one plot
recon_cols <- c(`Ecospace annual-average biomass` = "Bavg",
                `Ecospace biomass, same time step` = "Bmonth",
                `Sum of weight x number (age-structure file)` = "B_snapshot")
recon_cols <- recon_cols[recon_cols %in% names(recon)]
recon_all <- recon |>
  transmute(cal_year, stanza = stanza_lab(as.character(stanza)), !!!rlang::syms(recon_cols)) |>
  pivot_longer(-c(cal_year, stanza), names_to = "series") |>
  mutate(series = factor(series, levels = names(recon_cols)))
plots$reconciliation <- ggplot(recon_all, aes(cal_year, value, colour = series, linetype = series)) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~stanza, scales = "free_y", ncol = 3) +
  scale_colour_manual(values = pal_cat[seq_along(recon_cols)], name = NULL) +
  scale_linetype_manual(values = c("solid", "42", "12")[seq_along(recon_cols)], name = NULL) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.08))) +
  yr_breaks +
  labs(title = paste(species, "age-structure output vs. Ecospace stanza biomass"),
       subtitle = "Annual-average and same-month Ecospace biomass vs. biomass rebuilt from the January age structure",
       x = NULL, y = expression("Biomass (t km"^-2*")")) +
  theme_ms() + guides(colour = guide_legend(nrow = length(recon_cols)))

plots$reconciliation_annual <- recon_fig(
  "Bavg", "Ecospace annual-average biomass",
  "(a) Annual average vs. January snapshot of the age-structure file",
  "Age-0 differs by ~1.4x because the January snapshot precedes the seasonal recruitment pulse.")

if ("Bmonth" %in% names(recon))
  plots$reconciliation_monthly <- recon_fig(
    "Bmonth", "Ecospace biomass, same time step",
    "(b) Month-for-month: both taken from the same January time step")

plots$ZFM <- mort |>
  pivot_longer(c(Z, F, M), names_to = "rate") |>
  mutate(rate = factor(recode(rate, Z = "Total mortality (Z)", F = "Fishing mortality (F)",
                              M = "Natural mortality (M = Z - F)"),
                       levels = c("Total mortality (Z)", "Fishing mortality (F)",
                                  "Natural mortality (M = Z - F)")),
         age = age_lab(age)) |>
  ggplot(aes(cal_year, value, colour = age)) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~rate, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = pal_cat, name = "Age") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.08))) +
  yr_breaks +
  labs(title = paste0(species, " mortality by age, ", min(mort$cal_year), "-", max(mort$cal_year)),
       subtitle = "Z from cohort decline between January snapshots; F from catch / biomass, weighted by stanza exposure",
       x = NULL, y = expression("Instantaneous rate (yr"^-1*")")) +
  theme_ms() + guides(colour = guide_legend(nrow = 1))

plots$waa <- naa_waa |>
  mutate(age = age_lab(age)) |>
  ggplot(aes(cal_year, W_g, colour = age)) +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = pal_cat, name = "Age") +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.06))) +
  yr_breaks +
  labs(title = paste(species, "weight-at-age"),
       subtitle = paste0("Number-weighted mean body weight on 1 January (W-infinity = ", Winf_g, " g)"),
       x = NULL, y = "Mean weight (g)") +
  theme_ms() + guides(colour = guide_legend(nrow = 1))

if (!is.null(M_compare)) {
  plots$M_comparison <- M_compare |>
    pivot_longer(c(M_consumption, M_ZminusF), names_to = "method") |>
    mutate(method = factor(recode(method,
                                  M_consumption = "Consumption-based (M2 + M0)",
                                  M_ZminusF     = "Cohort decline (Z - F)"),
                           levels = c("Consumption-based (M2 + M0)", "Cohort decline (Z - F)")),
           age = paste("Age", age_lab(age))) |>
    ggplot(aes(cal_year, value, colour = method, linetype = method)) +
    geom_line(linewidth = 0.7) +
    facet_wrap(~age, ncol = 3) +
    scale_colour_manual(values = pal_cat[1:2], name = NULL) +
    scale_linetype_manual(values = c("solid", "22"), name = NULL) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.08))) +
    yr_breaks +
    labs(title = paste(species, "natural mortality: two independent estimates"),
         subtitle = "M2 from the Ecospace consumption matrix plus Ecopath M0, vs. M derived from cohort decline",
         x = NULL, y = expression("Natural mortality (yr"^-1*")")) +
    theme_ms() + guides(colour = guide_legend(nrow = 1))

  pred_rank <- M2_pred |> group_by(pred) |> summarise(M2 = mean(M2)) |> arrange(desc(M2))
  top_pred  <- head(pred_rank$pred, length(pal_cat))
  pred_lvls <- c(top_pred, "Other")
  plots$M2_by_predator <- M2_pred |>
    mutate(pred = factor(ifelse(pred %in% top_pred, pred, "Other"), levels = pred_lvls),
           cal_year = start_year + Year - 1,
           stanza = stanza_lab(stanza)) |>
    group_by(cal_year, stanza, pred) |> summarise(M2 = sum(M2), .groups = "drop") |>
    ggplot(aes(cal_year, M2, fill = pred)) +
    geom_area(colour = "white", linewidth = 0.25) +
    facet_wrap(~stanza, scales = "free_y", ncol = 3) +
    scale_fill_manual(values = setNames(c(pal_cat, col_other), pred_lvls), name = NULL) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.06))) +
    yr_breaks +
    labs(title = paste("Predation mortality on", tolower(species), "by predator"),
         subtitle = "M2 = predator consumption of each stanza / stanza biomass (Ecospace annual averages)",
         x = NULL, y = expression("Predation mortality, M2 (yr"^-1*")")) +
    theme_ms() + guides(fill = guide_legend(nrow = 2))
}

fig_size <- list(reconciliation = c(7.2, 5.6), reconciliation_annual = c(7.2, 5.2), reconciliation_monthly = c(7.2, 5.2), ZFM = c(7.2, 7.5), waa = c(7.2, 4.2),
                 M_comparison = c(7.2, 5.2), M2_by_predator = c(7.2, 5.6))
for (nm in names(plots))
  ggsave(file.path(out_dir, paste0(species, "_", nm, ".png")), plots[[nm]],
         width = fig_size[[nm]][1], height = fig_size[[nm]][2], dpi = 300, bg = "white")

grDevices::cairo_pdf(file.path(out_dir, paste0(species, "_figures.pdf")),
                     width = 7.2, height = 7.5, onefile = TRUE)
for (p in plots) print(p)
invisible(dev.off())
message("Outputs written to ", out_dir)

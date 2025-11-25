#' PLSC536 Final Paper Replication and Extension Code 
#' Signal Scarcity and Screening Gates: Rethinking the Fiscal Logic of Infrastructure-Based Promotion in China
#' Weijia Chen
#' This is the extension code based on the dataset and STATA code from Lei & Zhou (2021)

rm(list = ls())

# ==============================================================================
# 0. SETUP & PACKAGES ----
# ==============================================================================

# List all required packages
pkgs <- c(
  "tidyverse", "fixest", "haven", "glue", "gt", "scales", "modelsummary",
  "lfe", "rstanarm", "stargazer", "flextable", "did", "officer", 
  "car", "ggplot2", "patchwork", "kableExtra", "webshot2", "grid", "gridExtra",
  "broom"
)

# Install packages if missing
install_if_missing <- function(packages) {
  missing <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(missing) > 0) {
    install.packages(missing, dependencies = TRUE)
  }
}
install_if_missing(pkgs)

### Data manipulation 
library(tidyverse)
library(haven)
library(glue)
library(scales)

### Regression / econometrics 
library(fixest)
library(did)
library(lfe)
library(lme4)
library(rstanarm)
library(car)

### Tables / reporting 
library(modelsummary)
library(stargazer)
library(gt)
library(flextable)
library(kableExtra)
library(officer)

### Graphics 
library(ggplot2)
library(patchwork)
library(grid)
library(gridExtra)
library(webshot2)


# Set Theme
theme_set(theme_classic() + theme(
  axis.text = element_text(color = "black", size = 11),
  plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
  panel.grid.major.y = element_blank(),
  panel.grid.minor = element_blank()
))

# Set working directory - UPDATE THIS PATH IF NEEDED


# ==============================================================================
# 1. DATA PREPARATION ----
# ==============================================================================

# Read main analysis data
df_main <- read_dta("subway_analysis_use.dta")

# Define control variable sets
mayor_controls <- c("gender2", "race6", "Mayor_age", "Mayor_c_edu", 
                    "Mayor_c_central_exp", "Mayor_c_prov_exp", 
                    "Mayor_c_county_exp", "Mayor_c_soe_exp", 
                    "Mayor_c_univ_exp", "Mayor_c_league", 
                    "Mayor_connection_work")

base_controls <- c("lpop_1", "lgdp_1", "lrev_1", "GRP_growth_1")

# Filter to main sample (Prefecture-level cities)
df_analysis <- df_main |>
  filter(fsj2 == 0) |>
  arrange(City_Code, Year)

### Add Two Variables the extension paper focuses ###

# Helper: safe mean for a numeric vector
safe_mean <- function(x) {
  if (length(x) == 0) return(NA_real_)
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

# Add year numeric and raw ratios at city-year level 

df_analysis <- df_analysis |>
  mutate(
    # numeric year for window calculations
    Year_num = as.numeric(Year),
    
    # Land finance dependence: land revenue share in fiscal revenue
    # (land_per and rev_per are both per capita)
    land_share = land_per / rev_per,
    
    # Fiscal capacity: fiscal revenue relative to local economy
    # Here defined as per-capita fiscal revenue over per-capita GRP
    fiscal_capacity_raw = rev_per / GRP_per
  )

# Identify the first year of each mayor's tenure 

mayor_start <- df_analysis |>
  group_by(City_Code, Mayor_leaderindex) |>
  summarise(
    start_year = min(Year_num, na.rm = TRUE),
    .groups    = "drop"
  )

# Attach start_year to df_analysis
df_analysis <- df_analysis |>
  left_join(mayor_start, by = c("City_Code", "Mayor_leaderindex"))

# Compute pre-tenure baseline per mayor  
# Use all available years in [start_year - 3, start_year - 1];
# if there are none, fall back to using start_year itself.

K_window <- 3  # max pre-tenure window length (years)

# Pre-tenure years: strictly before start_year and within window
df_pre_tenure <- df_analysis |>
  filter(
    !is.na(start_year),
    Year_num < start_year,
    Year_num >= start_year - K_window
  )

# Baseline from pre-tenure years
baseline_mayor <- df_pre_tenure |>
  group_by(City_Code, Mayor_leaderindex) |>
  summarise(
    land_dep_pre   = safe_mean(land_share),
    fiscal_cap_pre = safe_mean(fiscal_capacity_raw),
    .groups        = "drop"
  )

# Mayors with no pre-tenure data
all_mayors <- df_analysis |>
  distinct(City_Code, Mayor_leaderindex, start_year)

missing_baseline <- all_mayors |>
  anti_join(baseline_mayor, by = c("City_Code", "Mayor_leaderindex"))

# Fallback: if no pre-tenure years, use start_year itself
if (nrow(missing_baseline) > 0) {
  baseline_fallback <- df_analysis |>
    semi_join(missing_baseline, by = c("City_Code", "Mayor_leaderindex")) |>
    filter(Year_num == start_year) |>
    group_by(City_Code, Mayor_leaderindex) |>
    summarise(
      land_dep_pre   = safe_mean(land_share),
      fiscal_cap_pre = safe_mean(fiscal_capacity_raw),
      .groups        = "drop"
    )
  
  baseline_mayor <- bind_rows(baseline_mayor, baseline_fallback)
}

# Define High/Low groups at mayor level (based on medians) 

median_land   <- median(baseline_mayor$land_dep_pre,   na.rm = TRUE)
median_fiscal <- median(baseline_mayor$fiscal_cap_pre, na.rm = TRUE)

baseline_mayor <- baseline_mayor |>
  mutate(
    High_Land_Group   = if_else(land_dep_pre   >= median_land,   1L, 0L),
    Low_Fiscal_Group = if_else(fiscal_cap_pre < median_fiscal, 1L, 0L)
  )

# Merge back into df_analysis 

df_analysis <- df_analysis |>
  left_join(
    baseline_mayor,
    by = c("City_Code", "Mayor_leaderindex")
  ) |>
  mutate(
    # Final baseline variables (pre-tenure fiscal conditions)
    land_dependence = land_dep_pre,
    fiscal_capacity = fiscal_cap_pre
  )

# Define Extension analysis sample and verify groups 

df_final <- df_analysis |>
  filter(
    !is.na(land_dependence),
    !is.na(fiscal_capacity),
    !is.na(High_Land_Group),
    !is.na(Low_Fiscal_Group)
  )





# ==============================================================================
# 2. TABLE 1: SUMMARY STATISTICS (Based on Original Table 1) ----
# ==============================================================================


# Define variables list (Original + New Extension Variables)
table1_vars <- c(
  "Mayor_promotion3y", "Mayor_connection_work", "Mayor_age", "Per_pop",
  "gdp", "rev", "GRP_growth", "Mayor_plan", "inv1_per",
  "GRP_per", "land_per", "rev_per",
  "land_dep_pre", "fiscal_cap_pre" # NEW VARIABLES ADDED HERE
)

# Calculate statistics
table1_stats <- data.frame()

for (var in table1_vars) {
  if (var %in% names(df_final)) {
    var_data <- df_final[[var]]
    stats <- c(
      N = sum(!is.na(var_data)),
      Mean = mean(var_data, na.rm = TRUE),
      Min = min(var_data, na.rm = TRUE),
      Max = max(var_data, na.rm = TRUE)
    )
    table1_stats <- rbind(table1_stats, stats)
  }
}

rownames(table1_stats) <- table1_vars
colnames(table1_stats) <- c("N", "Mean", "Min", "Max")

# Create formatted data frame
# Note: Carefully matching the format of the original table
table1_formatted <- data.frame(
  Variable = c(
    "Mayor promoted within three years", 
    "Mayor connection", 
    "Mayor age",
    "City population", 
    "City GDP (billion ÃÂ¥)", 
    "City fiscal revenue (billion ÃÂ¥)",
    "City GDP growth rate (%)", 
    "Mayor obtaining subway approval",
    "City investment in infrastructure per capita (ÃÂ¥)", 
    "City GDP per capita (ÃÂ¥)",
    "City land sales revenue per capita (ÃÂ¥)", 
    "City fiscal revenue per capita (ÃÂ¥)",
    # NEW ROWS 
    "Pre-tenure Land Finance Dependence (Ratio)",
    "Pre-tenure Fiscal Capacity (Ratio)"
  ),
  
  N = formatC(table1_stats$N, format = "d", big.mark = ","),
  
  Mean = c(
    sprintf("%.3f", table1_stats["Mayor_promotion3y", "Mean"]),
    sprintf("%.3f", table1_stats["Mayor_connection_work", "Mean"]),
    sprintf("%.2f", table1_stats["Mayor_age", "Mean"]),
    sprintf("%.2f", table1_stats["Per_pop", "Mean"]),
    sprintf("%.2f", table1_stats["gdp", "Mean"]),
    sprintf("%.2f", table1_stats["rev", "Mean"]),
    sprintf("%.2f", table1_stats["GRP_growth", "Mean"]),
    sprintf("%.3f", table1_stats["Mayor_plan", "Mean"]),
    formatC(table1_stats["inv1_per", "Mean"], format = "f", digits = 2, big.mark = ","),
    formatC(table1_stats["GRP_per", "Mean"], format = "f", digits = 2, big.mark = ","),
    formatC(table1_stats["land_per", "Mean"], format = "f", digits = 2, big.mark = ","),
    formatC(table1_stats["rev_per", "Mean"], format = "f", digits = 2, big.mark = ","),
    #  NEW ROWS 
    sprintf("%.3f", table1_stats["land_dep_pre", "Mean"]),
    sprintf("%.3f", table1_stats["fiscal_cap_pre", "Mean"])
  ),
  
  Min = c(
    sprintf("%.2f", table1_stats["Mayor_promotion3y", "Min"]),
    sprintf("%.2f", table1_stats["Mayor_connection_work", "Min"]),
    sprintf("%.0f", table1_stats["Mayor_age", "Min"]),
    sprintf("%.2f", table1_stats["Per_pop", "Min"]),
    sprintf("%.3f", table1_stats["gdp", "Min"]),
    sprintf("%.2f", table1_stats["rev", "Min"]),
    sprintf("%.2f", table1_stats["GRP_growth", "Min"]),
    sprintf("%.2f", table1_stats["Mayor_plan", "Min"]),
    sprintf("%.2f", table1_stats["inv1_per", "Min"]),
    sprintf("%.2f", table1_stats["GRP_per", "Min"]),
    sprintf("%.2f", table1_stats["land_per", "Min"]),
    sprintf("%.2f", table1_stats["rev_per", "Min"]),
    # NEW ROWS 
    sprintf("%.3f", table1_stats["land_dep_pre", "Min"]),
    sprintf("%.3f", table1_stats["fiscal_cap_pre", "Min"])
  ),
  
  Max = c(
    sprintf("%.2f", table1_stats["Mayor_promotion3y", "Max"]),
    sprintf("%.2f", table1_stats["Mayor_connection_work", "Max"]),
    sprintf("%.0f", table1_stats["Mayor_age", "Max"]),
    formatC(table1_stats["Per_pop", "Max"], format = "f", digits = 2, big.mark = ","),
    formatC(table1_stats["gdp", "Max"], format = "f", digits = 2, big.mark = ","),
    sprintf("%.2f", table1_stats["rev", "Max"]),
    sprintf("%.2f", table1_stats["GRP_growth", "Max"]),
    sprintf("%.2f", table1_stats["Mayor_plan", "Max"]),
    formatC(table1_stats["inv1_per", "Max"], format = "f", digits = 2, big.mark = ","),
    formatC(table1_stats["GRP_per", "Max"], format = "f", digits = 2, big.mark = ","),
    formatC(table1_stats["land_per", "Max"], format = "f", digits = 2, big.mark = ","),
    formatC(table1_stats["rev_per", "Max"], format = "f", digits = 2, big.mark = ","),
    # NEW ROWS 
    sprintf("%.3f", table1_stats["land_dep_pre", "Max"]),
    sprintf("%.3f", table1_stats["fiscal_cap_pre", "Max"])
  )
)

# Create GT table
table1_gt <- gt(table1_formatted) |>
  tab_header(
    title = md("**Summary Statistics**")
  ) |>
  cols_label(
    Variable = "",
    N = "N",
    Mean = "Mean",
    Min = "Min",
    Max = "Max"
  ) |>
  cols_align(
    align = "left",
    columns = Variable
  ) |>
  cols_align(
    align = "right",
    columns = c(N, Mean, Min, Max)
  ) |>
  # Styling to match original look
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_column_labels(everything())
  ) |>
  tab_style(
    style = list(cell_borders(sides = c("top", "bottom"), weight = px(1.5), color = "black")),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = list(cell_borders(sides = "bottom", weight = px(1.5), color = "black")),
    locations = cells_body(rows = nrow(table1_formatted))
  ) |>
  # Highlight the new rows (optional, remove if you want them to blend in perfectly)
  tab_style(
    style = list(cell_fill(color = "gray95")),
    locations = cells_body(rows = (nrow(table1_formatted)-1):nrow(table1_formatted))
  ) |>
  tab_options(
    table.font.size = px(11),
    column_labels.font.weight = "bold",
    table.width = pct(100),
    data_row.padding = px(3)
  )

# Save
gtsave(table1_gt, "table_1.png", vwidth = 800, vheight = 700)


# ==============================================================================
# 3. TABLE 2: HETEROGENEITY ANALYSIS BY LAND FINANCE DEPENDENCE ----
# Modified version: Model 1 = Original Table 2 Model 3 specification
# ==============================================================================


# DATA PREPARATION


df_ext <- df_analysis |>
  select(-any_of(c("High_Land_Group", "Low_Fiscal_Group", 
                   "land_dep_pre", "fiscal_cap_pre",
                   "land_dependence"))) |>
  left_join(
    baseline_mayor |>
      select(City_Code, Mayor_leaderindex, 
             land_dep_pre, fiscal_cap_pre, 
             High_Land_Group, Low_Fiscal_Group),
    by = c("City_Code", "Mayor_leaderindex")
  )


# RUN FOUR REGRESSION MODELS
# Using Original Table 2 Model 3 specification


# NEW Base formula: Mayor controls + Base controls, City + Year FE
formula_base_model3 <- paste("Mayor_promotion3y ~ Mayor_plan +", 
                             paste(c(mayor_controls, base_controls), collapse = " + "),  # Both control sets
                             "| City_Code + Year")  # City and Year FE (not province-year)

# Model 1: Baseline (Original Table 2 Model 3)
cat("\nRunning Model 1: Baseline (Original Table 2 Model 3 spec)...\n")
new_model1 <- feols(
  as.formula(formula_base_model3), 
  data = df_ext, 
  cluster = ~City_Code
)
cat("  N =", new_model1$nobs, "\n")

# Model 2: With Interaction Term (same controls as Model 1)
cat("\nRunning Model 2: Interaction model...\n")
formula_interaction_model3 <- paste("Mayor_promotion3y ~ Mayor_plan * High_Land_Group +", 
                                    paste(c(mayor_controls, base_controls), collapse = " + "),  # Both control sets
                                    "| City_Code + Year")  # City and Year FE

new_model2 <- feols(
  as.formula(formula_interaction_model3), 
  data = df_ext, 
  cluster = ~City_Code
)
cat("  N =", new_model2$nobs, "\n")

# Model 3: Subsample - High Land Dependence (same controls as Model 1)
cat("\nRunning Model 3: High land dependence subsample...\n")
df_high <- df_ext |> 
  filter(High_Land_Group == 1)

new_model3 <- feols(
  as.formula(formula_base_model3),  # Same as Model 1
  data = df_high, 
  cluster = ~City_Code
)
cat("  N =", new_model3$nobs, "\n")

# Model 4: Subsample - Low Land Dependence (same controls as Model 1)
cat("\nRunning Model 4: Low land dependence subsample...\n")
df_low <- df_ext |> 
  filter(High_Land_Group == 0)

new_model4 <- feols(
  as.formula(formula_base_model3),  # Same as Model 1
  data = df_low, 
  cluster = ~City_Code
)
cat("  N =", new_model4$nobs, "\n")


# EXTRACT RESULTS


extract_coef_robust <- function(model, var_name) {
  if (!(var_name %in% names(coef(model)))) {
    if (grepl(":", var_name)) {
      parts <- strsplit(var_name, ":")[[1]]
      alt_name <- paste(rev(parts), collapse = ":")
      if (alt_name %in% names(coef(model))) {
        var_name <- alt_name
      } else {
        return(list(coef = "", se = ""))
      }
    } else {
      return(list(coef = "", se = ""))
    }
  }
  
  coef_val <- coef(model)[var_name]
  se_val <- model$se[var_name]
  t_stat <- abs(coef_val / se_val)
  
  stars <- case_when(
    t_stat > 2.576 ~ "***",
    t_stat > 1.96 ~ "**",
    t_stat > 1.645 ~ "*",
    TRUE ~ ""
  )
  
  coef_str <- sprintf("%.3f%s", coef_val, stars)
  se_str <- sprintf("(%.3f)", se_val)
  
  return(list(coef = coef_str, se = se_str))
}

results_new <- list(
  model1_subway = extract_coef_robust(new_model1, "Mayor_plan"),
  model2_subway = extract_coef_robust(new_model2, "Mayor_plan"),
  model2_interaction = extract_coef_robust(new_model2, "Mayor_plan:High_Land_Group"),
  model3_subway = extract_coef_robust(new_model3, "Mayor_plan"),
  model4_subway = extract_coef_robust(new_model4, "Mayor_plan")
)

get_dep_mean <- function(model_data, var = "Mayor_promotion3y") {
  sprintf("%.3f", mean(model_data[[var]], na.rm = TRUE))
}

stats1 <- list(
  mean = get_dep_mean(df_ext),
  r2 = sprintf("%.3f", r2(new_model1)["r2"])
)

stats2 <- list(
  mean = get_dep_mean(df_ext),
  r2 = sprintf("%.3f", r2(new_model2)["r2"])
)

stats3 <- list(
  mean = get_dep_mean(df_high),
  r2 = sprintf("%.3f", r2(new_model3)["r2"])
)

stats4 <- list(
  mean = get_dep_mean(df_low),
  r2 = sprintf("%.3f", r2(new_model4)["r2"])
)


# CREATE TABLE


new_table2_data <- data.frame(
  Variable = c(
    "Subway approval", 
    "",
    "Subway Ã High Land Dependence",
    "",
    "",
    "City FE", 
    "Year FE",
    "Mayor controls",
    "City controls",
    "Province-year FE",
    "",
    "Sample",
    "Dependent variable mean",
    "R-squared", 
    "Observations"
  ),
  
  `(1) Baseline` = c(
    results_new$model1_subway$coef, 
    results_new$model1_subway$se,
    "", "",
    "",
    "Yes", "Yes", "Yes", "Yes", "No",
    "",
    "Full",
    stats1$mean,
    stats1$r2,
    format(new_model1$nobs, big.mark = ",")
  ),
  
  `(2) Interaction` = c(
    results_new$model2_subway$coef,
    results_new$model2_subway$se,
    results_new$model2_interaction$coef,
    results_new$model2_interaction$se,
    "",
    "Yes", "Yes", "Yes", "Yes", "No",
    "",
    "Full",
    stats2$mean,
    stats2$r2,
    format(new_model2$nobs, big.mark = ",")
  ),
  
  `(3) High Land` = c(
    results_new$model3_subway$coef,
    results_new$model3_subway$se,
    "", "",
    "",
    "Yes", "Yes", "Yes", "Yes", "No",
    "",
    "High Land",
    stats3$mean,
    stats3$r2,
    format(new_model3$nobs, big.mark = ",")
  ),
  
  `(4) Low Land` = c(
    results_new$model4_subway$coef,
    results_new$model4_subway$se,
    "", "",
    "",
    "Yes", "Yes", "Yes", "Yes", "No",
    "",
    "Low Land",
    stats4$mean,
    stats4$r2,
    format(new_model4$nobs, big.mark = ",")
  ),
  
  check.names = FALSE
)

# Create GT table
new_table2_gt <- gt(new_table2_data) |>
  tab_header(
    title = md("**Heterogeneous Effects by Land Finance Dependence**"),
    subtitle = md("*Dependent Variable: Mayor Promoted within Three Years*")
  ) |>
  cols_align(
    align = "center",
    columns = 2:5
  ) |>
  cols_align(
    align = "left",
    columns = 1
  ) |>
  tab_style(
    style = list(
      cell_borders(
        sides = c("top", "bottom"),
        weight = px(1.5),
        color = "black"
      )
    ),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = list(
      cell_borders(
        sides = "top",
        weight = px(0.5),
        color = "gray50"
      )
    ),
    locations = cells_body(rows = 6)
  ) |>
  tab_style(
    style = list(
      cell_borders(
        sides = "top",
        weight = px(1),
        color = "black"
      )
    ),
    locations = cells_body(rows = 12)
  ) |>
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        weight = px(1.5),
        color = "black"
      )
    ),
    locations = cells_body(rows = nrow(new_table2_data))
  ) |>
  tab_footnote(
    footnote = md("Notes: Standard errors clustered at the city level in parentheses. All models include city fixed effects, year fixed effects, mayor controls (gender, ethnicity, age, education, political connection, work experience), and city controls (lagged population, GDP, GDP growth, fiscal revenue). Province-year fixed effects are not included. High Land group includes mayors whose pre-tenure land finance dependence was above the median. Low Land group includes mayors whose pre-tenure land finance dependence was below the median."),
    locations = cells_body(columns = 1, rows = 1)
  ) |>
  tab_source_note(
    source_note = md("\\* p < 0.1, ** p < 0.05, *** p < 0.01")
  ) |>
  tab_options(
    table.font.size = px(11),
    column_labels.font.weight = "bold",
    footnotes.font.size = px(9),
    source_notes.font.size = px(9),
    table.width = pct(100),
    data_row.padding = px(3)
  )

# Save table
gtsave(new_table2_gt, "table_2.png", 
       vwidth = 900, 
       vheight = 750)



# ==============================================================================
# 4. TABLE 3: FISCAL CAPACITY INTERACTION ANALYSIS (RAW vs CENTERED) ----
# Models 1-2: Original specifications (Basic, Mayor controls)
# Models 3-4: With raw fiscal capacity interaction
# Models 5-6: With centered fiscal capacity interaction
# ==============================================================================

# REATE CENTERED FISCAL CAPACITY VARIABLE


mean_fiscal_capacity <- mean(df_ext$fiscal_capacity, na.rm = TRUE)
sd_fiscal_capacity <- sd(df_ext$fiscal_capacity, na.rm = TRUE)

df_ext <- df_ext |>
  mutate(
    fiscal_capacity_centered = fiscal_capacity - mean_fiscal_capacity
  )

# RUN SIX REGRESSION MODELS


# Model 1: Basic specification (no controls)
cat("\nRunning Model 1: Basic specification...\n")
table3_model1 <- feols(
  Mayor_promotion3y ~ Mayor_plan | City_Code + Year,
  data = df_ext,
  cluster = ~City_Code
)
cat("  N =", table3_model1$nobs, "\n")

# Model 2: Mayor controls only
cat("\nRunning Model 2: Mayor controls only...\n")
formula_model2 <- paste("Mayor_promotion3y ~ Mayor_plan +",
                        paste(mayor_controls, collapse = " + "),
                        "| City_Code + Year")
table3_model2 <- feols(
  as.formula(formula_model2),
  data = df_ext,
  cluster = ~City_Code
)
cat("  N =", table3_model2$nobs, "\n")

# Model 3: Basic + FC Interaction (RAW)
cat("\nRunning Model 3: Basic + FC interaction (raw)...\n")
table3_model3 <- feols(
  Mayor_promotion3y ~ Mayor_plan * fiscal_capacity | City_Code + Year,
  data = df_ext,
  cluster = ~City_Code
)
cat("  N =", table3_model3$nobs, "\n")

# Model 4: Mayor controls + FC Interaction (RAW)
cat("\nRunning Model 4: Mayor controls + FC interaction (raw)...\n")
formula_model4 <- paste("Mayor_promotion3y ~ Mayor_plan * fiscal_capacity +",
                        paste(mayor_controls, collapse = " + "),
                        "| City_Code + Year")
table3_model4 <- feols(
  as.formula(formula_model4),
  data = df_ext,
  cluster = ~City_Code
)
cat("  N =", table3_model4$nobs, "\n")

# Model 5: Basic + FC Interaction (CENTERED)
cat("\nRunning Model 5: Basic + FC interaction (centered)...\n")
table3_model5 <- feols(
  Mayor_promotion3y ~ Mayor_plan * fiscal_capacity_centered | City_Code + Year,
  data = df_ext,
  cluster = ~City_Code
)
cat("  N =", table3_model5$nobs, "\n")

# Model 6: Mayor controls + FC Interaction (CENTERED)
cat("\nRunning Model 6: Mayor controls + FC interaction (centered)...\n")
formula_model6 <- paste("Mayor_promotion3y ~ Mayor_plan * fiscal_capacity_centered +",
                        paste(mayor_controls, collapse = " + "),
                        "| City_Code + Year")
table3_model6 <- feols(
  as.formula(formula_model6),
  data = df_ext,
  cluster = ~City_Code
)
cat("  N =", table3_model6$nobs, "\n")


# EXTRACT RESULTS 

extract_coef_robust <- function(model, var_name) {
  if (!(var_name %in% names(coef(model)))) {
    if (grepl(":", var_name)) {
      parts <- strsplit(var_name, ":")[[1]]
      alt_name <- paste(rev(parts), collapse = ":")
      if (alt_name %in% names(coef(model))) {
        var_name <- alt_name
      } else {
        return(list(coef = "", se = ""))
      }
    } else {
      return(list(coef = "", se = ""))
    }
  }
  
  coef_val <- coef(model)[var_name]
  se_val <- model$se[var_name]
  t_stat <- abs(coef_val / se_val)
  
  # Modified: Only show ** and *** (removed * for p < 0.1)
  stars <- case_when(
    t_stat > 2.576 ~ "***",  # p < 0.01
    t_stat > 1.96 ~ "**",     # p < 0.05
    TRUE ~ ""                 # No star for p >= 0.05
  )
  
  coef_str <- sprintf("%.3f%s", coef_val, stars)
  se_str <- sprintf("(%.3f)", se_val)
  
  return(list(coef = coef_str, se = se_str))
}

# Extract all results
results_table3 <- list(
  model1_subway = extract_coef_robust(table3_model1, "Mayor_plan"),
  model2_subway = extract_coef_robust(table3_model2, "Mayor_plan"),
  
  model3_subway = extract_coef_robust(table3_model3, "Mayor_plan"),
  model3_fiscal = extract_coef_robust(table3_model3, "fiscal_capacity"),
  model3_interaction = extract_coef_robust(table3_model3, "Mayor_plan:fiscal_capacity"),
  
  model4_subway = extract_coef_robust(table3_model4, "Mayor_plan"),
  model4_fiscal = extract_coef_robust(table3_model4, "fiscal_capacity"),
  model4_interaction = extract_coef_robust(table3_model4, "Mayor_plan:fiscal_capacity"),
  
  model5_subway = extract_coef_robust(table3_model5, "Mayor_plan"),
  model5_fiscal = extract_coef_robust(table3_model5, "fiscal_capacity_centered"),
  model5_interaction = extract_coef_robust(table3_model5, "Mayor_plan:fiscal_capacity_centered"),
  
  model6_subway = extract_coef_robust(table3_model6, "Mayor_plan"),
  model6_fiscal = extract_coef_robust(table3_model6, "fiscal_capacity_centered"),
  model6_interaction = extract_coef_robust(table3_model6, "Mayor_plan:fiscal_capacity_centered")
)

get_dep_mean <- function(model_data, var = "Mayor_promotion3y") {
  sprintf("%.3f", mean(model_data[[var]], na.rm = TRUE))
}

dep_mean <- get_dep_mean(df_ext)


# CREATE TABLE

table3_data <- data.frame(
  Variable = c(
    "Subway approval",
    "",
    "Fiscal capacity",
    "",
    "Subway Ã Fiscal capacity",
    "",
    "",
    "City FE",
    "Year FE", 
    "Mayor controls",
    "",
    "Dependent variable mean",
    "Observations"
  ),
  
  `(1) Basic` = c(
    results_table3$model1_subway$coef,
    results_table3$model1_subway$se,
    "", "", "", "",
    "",
    "Yes", "Yes", "No",
    "",
    dep_mean,
    format(table3_model1$nobs, big.mark = ",")
  ),
  
  `(2) Mayor` = c(
    results_table3$model2_subway$coef,
    results_table3$model2_subway$se,
    "", "", "", "",
    "",
    "Yes", "Yes", "Yes",
    "",
    dep_mean,
    format(table3_model2$nobs, big.mark = ",")
  ),
  
  `(3) Basic+FC` = c(
    results_table3$model3_subway$coef,
    results_table3$model3_subway$se,
    results_table3$model3_fiscal$coef,
    results_table3$model3_fiscal$se,
    results_table3$model3_interaction$coef,
    results_table3$model3_interaction$se,
    "",
    "Yes", "Yes", "No",
    "",
    dep_mean,
    format(table3_model3$nobs, big.mark = ",")
  ),
  
  `(4) Mayor+FC` = c(
    results_table3$model4_subway$coef,
    results_table3$model4_subway$se,
    results_table3$model4_fiscal$coef,
    results_table3$model4_fiscal$se,
    results_table3$model4_interaction$coef,
    results_table3$model4_interaction$se,
    "",
    "Yes", "Yes", "Yes",
    "",
    dep_mean,
    format(table3_model4$nobs, big.mark = ",")
  ),
  
  `(5) Basic+FC_c` = c(
    results_table3$model5_subway$coef,
    results_table3$model5_subway$se,
    results_table3$model5_fiscal$coef,
    results_table3$model5_fiscal$se,
    results_table3$model5_interaction$coef,
    results_table3$model5_interaction$se,
    "",
    "Yes", "Yes", "No",
    "",
    dep_mean,
    format(table3_model5$nobs, big.mark = ",")
  ),
  
  `(6) Mayor+FC_c` = c(
    results_table3$model6_subway$coef,
    results_table3$model6_subway$se,
    results_table3$model6_fiscal$coef,
    results_table3$model6_fiscal$se,
    results_table3$model6_interaction$coef,
    results_table3$model6_interaction$se,
    "",
    "Yes", "Yes", "Yes",
    "",
    dep_mean,
    format(table3_model6$nobs, big.mark = ",")
  ),
  
  check.names = FALSE
)

# Create GT table
table3_gt <- gt(table3_data) |>
  tab_header(
    title = md("**Fiscal Capacity Interaction Analysis**"),
    subtitle = md("*Dependent Variable: Mayor Promoted within Three Years*")
  ) |>
  cols_align(
    align = "center",
    columns = 2:7
  ) |>
  cols_align(
    align = "left",
    columns = 1
  ) |>
  tab_spanner(
    label = "Baseline",
    columns = c(`(1) Basic`, `(2) Mayor`)
  ) |>
  tab_spanner(
    label = "Raw FC Interaction",
    columns = c(`(3) Basic+FC`, `(4) Mayor+FC`)
  ) |>
  tab_spanner(
    label = "Centered FC Interaction",
    columns = c(`(5) Basic+FC_c`, `(6) Mayor+FC_c`)
  ) |>
  tab_style(
    style = list(
      cell_borders(
        sides = c("top", "bottom"),
        weight = px(1.5),
        color = "black"
      )
    ),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = list(
      cell_borders(
        sides = "top",
        weight = px(0.5),
        color = "gray50"
      )
    ),
    locations = cells_body(rows = 8)
  ) |>
  tab_style(
    style = list(
      cell_borders(
        sides = "top",
        weight = px(1),
        color = "black"
      )
    ),
    locations = cells_body(rows = 12)
  ) |>
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        weight = px(1.5),
        color = "black"
      )
    ),
    locations = cells_body(rows = nrow(table3_data))
  ) |>
  tab_footnote(
    footnote = md(sprintf("Notes: Standard errors clustered at the city level in parentheses. Models 1-2 are baseline specifications. Models 3-4 include interaction with raw fiscal capacity. Models 5-6 include interaction with centered fiscal capacity (mean = %.4f). Mayor controls include gender, ethnicity, age, education, political connection, and work experience. FC_c denotes centered fiscal capacity.", mean_fiscal_capacity)),
    locations = cells_body(columns = 1, rows = 1)
  ) |>
  tab_source_note(
    source_note = md("** p < 0.05, *** p < 0.01")  
  ) |>
  tab_options(
    table.font.size = px(11),
    column_labels.font.weight = "bold",
    footnotes.font.size = px(9),
    source_notes.font.size = px(9),
    table.width = pct(100),
    data_row.padding = px(3)
  )

# Save table
gtsave(table3_gt, "table_3.png", 
       vwidth = 1100,  
       vheight = 700)

# INTERPRETATION 

# Extract coefficients for comparison
coef_raw_basic <- coef(table3_model3)["Mayor_plan"]
coef_raw_interaction <- coef(table3_model3)["Mayor_plan:fiscal_capacity"]

coef_centered_basic <- coef(table3_model5)["Mayor_plan"]
coef_centered_interaction <- coef(table3_model5)["Mayor_plan:fiscal_capacity_centered"]

cat("\n1. RAW FISCAL CAPACITY (Model 3):\n")
cat("===================================\n")
cat(sprintf("Main effect: %.3f\n", coef_raw_basic))
cat("â Effect of subway when fiscal capacity = 0 (unrealistic!)\n")
cat(sprintf("Interaction: %.3f\n", coef_raw_interaction))
cat("â Change in subway effect per unit increase in fiscal capacity\n\n")

cat("Example: At mean fiscal capacity (%.4f):\n", mean_fiscal_capacity)
cat(sprintf("Effect = %.3f + %.3f Ã %.4f = %.3f\n", 
            coef_raw_basic, coef_raw_interaction, mean_fiscal_capacity,
            coef_raw_basic + coef_raw_interaction * mean_fiscal_capacity))
cat(sprintf("â Promotion increases by %.1f pp\n\n", 
            (coef_raw_basic + coef_raw_interaction * mean_fiscal_capacity) * 100))

cat("\n2. CENTERED FISCAL CAPACITY (Model 5):\n")
cat("========================================\n")
cat(sprintf("Main effect: %.3f\n", coef_centered_basic))
cat("â Effect of subway at MEAN fiscal capacity (interpretable!)\n")
cat(sprintf("Interaction: %.3f\n", coef_centered_interaction))
cat("â Same interpretation as raw\n\n")

cat("Verification: Effect at mean should be identical:\n")
cat(sprintf("Raw model at mean: %.3f\n", coef_raw_basic + coef_raw_interaction * mean_fiscal_capacity))
cat(sprintf("Centered model at mean: %.3f (main effect directly)\n\n", coef_centered_basic))

cat("\n3. MARGINAL EFFECTS AT DIFFERENT LEVELS:\n")
cat("==========================================\n")

# Define levels of interest
levels <- data.frame(
  name = c("Low (-1 SD)", "Mean", "High (+1 SD)"),
  raw_value = c(
    mean_fiscal_capacity - sd_fiscal_capacity,
    mean_fiscal_capacity,
    mean_fiscal_capacity + sd_fiscal_capacity
  ),
  centered_value = c(-sd_fiscal_capacity, 0, sd_fiscal_capacity)
)

cat("\nUsing Basic specification (Models 3 & 5):\n")
cat("------------------------------------------\n")

for (i in 1:nrow(levels)) {
  # Raw model calculation
  effect_raw <- coef_raw_basic + coef_raw_interaction * levels$raw_value[i]
  
  # Centered model calculation
  effect_centered <- coef_centered_basic + coef_centered_interaction * levels$centered_value[i]
  
  cat(sprintf("\n%s (FC = %.4f):\n", levels$name[i], levels$raw_value[i]))
  cat(sprintf("  Raw model: %.3f + %.3f Ã %.4f = %.3f\n", 
              coef_raw_basic, coef_raw_interaction, levels$raw_value[i], effect_raw))
  cat(sprintf("  Centered model: %.3f + %.3f Ã %.4f = %.3f\n", 
              coef_centered_basic, coef_centered_interaction, levels$centered_value[i], effect_centered))
  cat(sprintf("  â Subway effect: %.1f pp increase in promotion\n", effect_centered * 100))
}


# ==============================================================================
# Appendix TABLE 1: EXTRA ANALYSIS - SUBWAY APPROVALS BY FISCAL CAPACITY GROUP ----
# ==============================================================================

# Create summary data (excluding NA)
table_data <- df_ext |>
  filter(!is.na(Low_Fiscal_Group)) |>  # Remove NA values
  group_by(Low_Fiscal_Group, Mayor_plan) |>
  summarise(
    count = n(),
    .groups = "drop"
  ) |>
  mutate(
    Fiscal_Group = factor(
      Low_Fiscal_Group,
      levels = c(0, 1),
      labels = c("High Fiscal Capacity", "Low Fiscal Capacity")
    ),
    Approval = factor(
      Mayor_plan,
      levels = c(1, 0),  # Reorder to show Approval first
      labels = c("Subway Approval", "No Subway Approval")
    )
  ) |>
  group_by(Fiscal_Group) |>
  mutate(
    total = sum(count),
    percentage = (count / total) * 100
  ) |>
  ungroup() |>
  arrange(Fiscal_Group, Approval) |>
  select(Fiscal_Group, Approval, count, percentage, total)

# CREATE FORMATTED TABLE

# Reshape for better presentation
presentation_table <- table_data |>
  mutate(
    count_pct = sprintf("%d (%.1f%%)", count, percentage)
  ) |>
  select(Fiscal_Group, Approval, count_pct, total) |>
  pivot_wider(
    names_from = Approval,
    values_from = count_pct,
    id_cols = c(Fiscal_Group, total)
  ) |>
  rename(
    `Total Observations` = total
  )

# Create GT table
fiscal_table_gt <- presentation_table |>
  gt() |>
  tab_header(
    title = md("**Distribution of Subway Approvals by Fiscal Capacity Group**"),
    subtitle = md("*Mayor-year observations (2003-2016)*")
  ) |>
  cols_label(
    Fiscal_Group = md("**Fiscal Capacity Group**"),
    `Subway Approval` = md("**Subway Approval**"),
    `No Subway Approval` = md("**No Subway Approval**"),
    `Total Observations` = md("**Total**")
  ) |>
  tab_style(
    style = list(
      cell_fill(color = "#f0f0f0"),
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = list(
      cell_borders(
        sides = c("top", "bottom"),
        weight = px(2),
        color = "black"
      )
    ),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        weight = px(1.5),
        color = "black"
      )
    ),
    locations = cells_body(rows = nrow(presentation_table))
  ) |>
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(columns = -Fiscal_Group)
  ) |>
  tab_footnote(
    footnote = md("Note: Percentages shown in parentheses are calculated within each fiscal capacity group. Fiscal capacity groups are determined by median split of pre-tenure fiscal revenue to GDP ratio."),
    locations = cells_column_labels(columns = 1)
  ) |>
  tab_options(
    table.font.size = px(12),
    column_labels.font.weight = "bold",
    table.width = pct(90),
    data_row.padding = px(5)
  )

# Save the table
gtsave(fiscal_table_gt, "app_table_1.png",
       vwidth = 800, 
       vheight = 400)


# ==============================================================================
# APPENDIX TABLE 2: FISCAL CAPACITY INTERACTION ANALYSIS BY DUMMY GROUP ----
# Models 1-3: Original Table 2 Specifications 1, 2, 3
# Models 4-6: Same specs with Fiscal Capacity interaction
# ==============================================================================



# RUN SIX REGRESSION MODELS; LAST THREE WITH Low_Fiscal_Group Dummy

# Model 1: Original Table 2 Spec 1 (Basic - no controls)
cat("\nRunning Model 1: Basic specification...\n")
table3_model1 <- feols(
  Mayor_promotion3y ~ Mayor_plan | City_Code + Year,
  data = df_ext,
  cluster = ~City_Code
)
cat("  N =", table3_model1$nobs, "\n")

# Model 2: Original Table 2 Spec 2 (Mayor controls only)
cat("\nRunning Model 2: Mayor controls only...\n")
formula_model2 <- paste("Mayor_promotion3y ~ Mayor_plan +",
                        paste(mayor_controls, collapse = " + "),
                        "| City_Code + Year")
table3_model2 <- feols(
  as.formula(formula_model2),
  data = df_ext,
  cluster = ~City_Code
)
cat("  N =", table3_model2$nobs, "\n")

# Model 3: Original Table 2 Spec 3 (Full controls)
cat("\nRunning Model 3: Full controls...\n")
formula_model3 <- paste("Mayor_promotion3y ~ Mayor_plan +",
                        paste(c(mayor_controls, base_controls), collapse = " + "),
                        "| City_Code + Year")
table3_model3 <- feols(
  as.formula(formula_model3),
  data = df_ext,
  cluster = ~City_Code
)
cat("  N =", table3_model3$nobs, "\n")

# Model 4: Spec 1 + Fiscal Capacity Interaction
cat("\nRunning Model 4: Basic + Fiscal Capacity interaction...\n")
table3_model4 <- feols(
  Mayor_promotion3y ~ Mayor_plan * Low_Fiscal_Group | City_Code + Year,
  data = df_ext,
  cluster = ~City_Code
)
cat("  N =", table3_model4$nobs, "\n")

# Model 5: Spec 2 + Fiscal Capacity Interaction
cat("\nRunning Model 5: Mayor controls + Fiscal Capacity interaction...\n")
formula_model5 <- paste("Mayor_promotion3y ~ Mayor_plan * Low_Fiscal_Group +",
                        paste(mayor_controls, collapse = " + "),
                        "| City_Code + Year")
table3_model5 <- feols(
  as.formula(formula_model5),
  data = df_ext,
  cluster = ~City_Code
)
cat("  N =", table3_model5$nobs, "\n")

# Model 6: Spec 3 + Fiscal Capacity Interaction
cat("\nRunning Model 6: Full controls + Fiscal Capacity interaction...\n")
formula_model6 <- paste("Mayor_promotion3y ~ Mayor_plan * Low_Fiscal_Group +",
                        paste(c(mayor_controls, base_controls), collapse = " + "),
                        "| City_Code + Year")
table3_model6 <- feols(
  as.formula(formula_model6),
  data = df_ext,
  cluster = ~City_Code
)
cat("  N =", table3_model6$nobs, "\n")


# EXTRACT RESULTS


# Extract coefficients function (same as before)
extract_coef_robust <- function(model, var_name) {
  if (!(var_name %in% names(coef(model)))) {
    if (grepl(":", var_name)) {
      parts <- strsplit(var_name, ":")[[1]]
      alt_name <- paste(rev(parts), collapse = ":")
      if (alt_name %in% names(coef(model))) {
        var_name <- alt_name
      } else {
        return(list(coef = "", se = ""))
      }
    } else {
      return(list(coef = "", se = ""))
    }
  }
  
  coef_val <- coef(model)[var_name]
  se_val <- model$se[var_name]
  t_stat <- abs(coef_val / se_val)
  
  stars <- case_when(
    t_stat > 2.576 ~ "***",
    t_stat > 1.96 ~ "**",
    t_stat > 1.645 ~ "*",
    TRUE ~ ""
  )
  
  coef_str <- sprintf("%.3f%s", coef_val, stars)
  se_str <- sprintf("(%.3f)", se_val)
  
  return(list(coef = coef_str, se = se_str))
}

# Extract all results
results_table3 <- list(
  model1_subway = extract_coef_robust(table3_model1, "Mayor_plan"),
  model2_subway = extract_coef_robust(table3_model2, "Mayor_plan"),
  model3_subway = extract_coef_robust(table3_model3, "Mayor_plan"),
  model4_subway = extract_coef_robust(table3_model4, "Mayor_plan"),
  model4_interaction = extract_coef_robust(table3_model4, "Mayor_plan:Low_Fiscal_Group"),
  model5_subway = extract_coef_robust(table3_model5, "Mayor_plan"),
  model5_interaction = extract_coef_robust(table3_model5, "Mayor_plan:Low_Fiscal_Group"),
  model6_subway = extract_coef_robust(table3_model6, "Mayor_plan"),
  model6_interaction = extract_coef_robust(table3_model6, "Mayor_plan:Low_Fiscal_Group")
)

# Get dependent variable means
get_dep_mean <- function(model_data, var = "Mayor_promotion3y") {
  sprintf("%.3f", mean(model_data[[var]], na.rm = TRUE))
}

dep_mean <- get_dep_mean(df_ext)


# CREATE TABLE

table3_data <- data.frame(
  Variable = c(
    "Subway approval",
    "",
    "Subway Ã Low Fiscal Capacity",
    "",
    "",
    "City FE",
    "Year FE", 
    "Mayor controls",
    "City controls",
    "",
    "Dependent variable mean",
    "Observations"
  ),
  
  `(1) Basic` = c(
    results_table3$model1_subway$coef,
    results_table3$model1_subway$se,
    "", "",
    "",
    "Yes", "Yes", "No", "No",
    "",
    dep_mean,
    format(table3_model1$nobs, big.mark = ",")
  ),
  
  `(2) Mayor` = c(
    results_table3$model2_subway$coef,
    results_table3$model2_subway$se,
    "", "",
    "",
    "Yes", "Yes", "Yes", "No",
    "",
    dep_mean,
    format(table3_model2$nobs, big.mark = ",")
  ),
  
  `(3) Full` = c(
    results_table3$model3_subway$coef,
    results_table3$model3_subway$se,
    "", "",
    "",
    "Yes", "Yes", "Yes", "Yes",
    "",
    dep_mean,
    format(table3_model3$nobs, big.mark = ",")
  ),
  
  `(4) Basic+FC` = c(
    results_table3$model4_subway$coef,
    results_table3$model4_subway$se,
    results_table3$model4_interaction$coef,
    results_table3$model4_interaction$se,
    "",
    "Yes", "Yes", "No", "No",
    "",
    dep_mean,
    format(table3_model4$nobs, big.mark = ",")
  ),
  
  `(5) Mayor+FC` = c(
    results_table3$model5_subway$coef,
    results_table3$model5_subway$se,
    results_table3$model5_interaction$coef,
    results_table3$model5_interaction$se,
    "",
    "Yes", "Yes", "Yes", "No",
    "",
    dep_mean,
    format(table3_model5$nobs, big.mark = ",")
  ),
  
  `(6) Full+FC` = c(
    results_table3$model6_subway$coef,
    results_table3$model6_subway$se,
    results_table3$model6_interaction$coef,
    results_table3$model6_interaction$se,
    "",
    "Yes", "Yes", "Yes", "Yes",
    "",
    dep_mean,
    format(table3_model6$nobs, big.mark = ",")
  ),
  
  check.names = FALSE
)

# Create GT table
table3_gt <- gt(table3_data) |>
  tab_header(
    title = md("**Fiscal Capacity Interaction Analysis with Dummy Fiscal Capacity **"),
    subtitle = md("*Dependent Variable: Mayor Promoted within Three Years*")
  ) |>
  cols_align(
    align = "center",
    columns = 2:7
  ) |>
  cols_align(
    align = "left",
    columns = 1
  ) |>
  tab_spanner(
    label = "Original Specifications",
    columns = c(`(1) Basic`, `(2) Mayor`, `(3) Full`)
  ) |>
  tab_spanner(
    label = "With Fiscal Capacity Interaction",
    columns = c(`(4) Basic+FC`, `(5) Mayor+FC`, `(6) Full+FC`)
  ) |>
  tab_style(
    style = list(
      cell_borders(
        sides = c("top", "bottom"),
        weight = px(1.5),
        color = "black"
      )
    ),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = list(
      cell_borders(
        sides = "top",
        weight = px(0.5),
        color = "gray50"
      )
    ),
    locations = cells_body(rows = 6)
  ) |>
  tab_style(
    style = list(
      cell_borders(
        sides = "top",
        weight = px(1),
        color = "black"
      )
    ),
    locations = cells_body(rows = 11)
  ) |>
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        weight = px(1.5),
        color = "black"
      )
    ),
    locations = cells_body(rows = nrow(table3_data))
  ) |>
  tab_footnote(
    footnote = md("Notes: Standard errors clustered at the city level in parentheses. Models 1-3 replicate original Table 2 specifications. Models 4-6 add interaction with Low Fiscal Capacity indicator. Low Fiscal Capacity group includes mayors whose pre-tenure fiscal capacity (fiscal revenue/GDP ratio) was below the median. Mayor controls include gender, ethnicity, age, education, political connection, and work experience. City controls include lagged population, GDP, GDP growth, and fiscal revenue."),
    locations = cells_body(columns = 1, rows = 1)
  ) |>
  tab_source_note(
    source_note = md("\\* p < 0.1, ** p < 0.05, *** p < 0.01")
  ) |>
  tab_options(
    table.font.size = px(11),
    column_labels.font.weight = "bold",
    footnotes.font.size = px(9),
    source_notes.font.size = px(9),
    table.width = pct(100),
    data_row.padding = px(3)
  )

# Save table
gtsave(table3_gt, "app_table_2.png", 
       vwidth = 1000,  # Wider for 6 columns
       vheight = 700)



# ==============================================================================
# 5. DATA PREPARATION FOR DYNAMIC ANALYSIS [Based on Original Figure 2 and Table A14] ----
# ==============================================================================
# We continue using df_analysis but process it into df_reg for event study

# Helper for safe lags/leads
safe_lag <- function(x, n=1) lag(x, n, default = 0)
safe_lead <- function(x, n=1) lead(x, n, default = 0)

# Initialize grouping
df_dyn <- df_analysis |> group_by(City_Code)

# 1. Treatment Start (mpconn0)
df_dyn <- df_dyn |>
  mutate(
    lag_plan = safe_lag(Mayor_plan),
    lag_leader = safe_lag(Mayor_leaderindex), 
    mpconn0 = case_when(
      lag_plan == 0 & Mayor_plan == 1 ~ 1,
      lag_plan == 1 & Mayor_plan == 1 & lag_leader != Mayor_leaderindex ~ 1,
      TRUE ~ 0
    )
  )

# 2. Tenure Effect (mpconn1-13)
for(i in 1:13) {
  curr <- paste0("mpconn", i)
  prev <- paste0("mpconn", i-1)
  df_dyn <- df_dyn |>
    mutate(!!curr := if_else(safe_lag(get(prev)) == 1 & 
                               safe_lag(Mayor_leaderindex) == Mayor_leaderindex & 
                               Mayor_plan == 1, 1, 0))
}

# 3. Pre-trend (mpprior1-13)
for(i in 1:13) {
  curr <- paste0("mpprior", i)
  df_dyn <- df_dyn |>
    mutate(lead_target = safe_lead(Mayor_plan, i)) |>
    mutate(!!curr := if_else(Mayor_plan == 0 & lead_target == 1, 1, 0))
  
  if(i > 1) {
    for(x in 1:(i-1)) {
      df_dyn <- df_dyn |>
        mutate(!!curr := if_else(safe_lead(Mayor_plan, x) == 1, 0, get(curr)))
    }
  }
}

# 4. Post-tenure (mppost1-13)
for(i in 1:13) {
  curr <- paste0("mppost", i)
  df_dyn <- df_dyn |>
    mutate(lag_target = safe_lag(Mayor_plan, i)) |>
    mutate(!!curr := if_else(Mayor_plan == 0 & lag_target == 1, 1, 0))
  
  if(i > 1) {
    for(x in 1:(i-1)) {
      df_dyn <- df_dyn |>
        mutate(!!curr := if_else(safe_lag(Mayor_plan, x) == 1, 0, get(curr)))
    }
  }
}

# 5. Binning (Combining Tails)
row_sum_regex <- function(d, pat) as.integer(rowSums(d[, grep(pat, names(d))]) > 0)

df_reg <- df_dyn |> ungroup() |>
  mutate(
    # Pre-trend: Combine 5-13
    mpprior5 = row_sum_regex(cur_data(), "mpprior([5-9]|1[0-3])"),
    # Tenure: Combine 4-13
    mpconn4  = row_sum_regex(cur_data(), "mpconn([4-9]|1[0-3])"),
    # Post: Combine 4-13
    mppost4  = row_sum_regex(cur_data(), "mppost([4-9]|1[0-3])"),
    # Any Post (for Fig 2 & Col 1/2)
    mppost_any = row_sum_regex(cur_data(), "mppost[0-9]")
  )

# Clean NAs
vars_clean <- c(paste0("mpprior", 1:5), 
                paste0("mpconn", 0:4), 
                paste0("mppost", 1:4), 
                "mppost_any")
df_reg[vars_clean][is.na(df_reg[vars_clean])] <- 0

# Ensure Province-Year Interaction exists in this dataframe (created earlier in df_analysis, but double check)
df_reg$prov_year <- interaction(df_reg$Year, df_reg$pro_code, drop=TRUE)


# ==============================================================================
# 6. REPLICATE FIGURE 2  ----
# ==============================================================================

# Model: No controls, mppost_any included but not plotted
f_fig2 <- as.formula(paste(
  "Mayor_promotion3y ~ mpprior5 + mpprior4 + mpprior3 + mpprior2 +",
  "mpconn0 + mpconn1 + mpconn2 + mpconn3 + mpconn4 + mppost_any",
  "| Year + City_Code"
))

m_fig2 <- felm(f_fig2, data = df_reg)

# Extract Data
coefs95 <- summary(m_fig2)$coefficients
se <- coefs95[, 2]
est <- coefs95[, 1]

plot_vars <- c("mpprior5", "mpprior4", "mpprior3", "mpprior2",
               "mpconn0", "mpconn1", "mpconn2", "mpconn3", "mpconn4")

df_plot <- data.frame(
  term = rownames(coefs95),
  estimate = est,
  se = se
) |> filter(term %in% plot_vars)

# Add Reference
df_plot <- rbind(df_plot, data.frame(term = "mpprior1", estimate = 0, se = 0))

# Coordinates
x_map <- c("mpprior5"=-5, "mpprior4"=-4, "mpprior3"=-3, "mpprior2"=-2, "mpprior1"=-1,
           "mpconn0"=0, "mpconn1"=1, "mpconn2"=2, "mpconn3"=3, "mpconn4"=4)
df_plot$x <- x_map[df_plot$term]

# CIs
df_plot$ci95_lo <- df_plot$estimate - 1.96 * df_plot$se
df_plot$ci95_hi <- df_plot$estimate + 1.96 * df_plot$se
df_plot$ci90_lo <- df_plot$estimate - 1.645 * df_plot$se
df_plot$ci90_hi <- df_plot$estimate + 1.645 * df_plot$se

# Plot (Matching Original Scale: Y from -0.4 to 1.2)
p2 <- ggplot(df_plot, aes(x = x, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + # Dashed line at 0
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "black") + # Separation
  # 95% CI (Thin line)
  geom_errorbar(aes(ymin = ci95_lo, ymax = ci95_hi), width = 0, size = 0.5) +
  # 90% CI (Thick line)
  geom_errorbar(aes(ymin = ci90_lo, ymax = ci90_hi), width = 0, size = 1.2) +
  # Point (Circle)
  geom_point(fill = "white", shape = 21, size = 3, stroke = 1) +
  scale_x_continuous(
    breaks = -5:4, 
    labels = c("=<-5", "-4", "-3", "-2", "-1", "0", "1", "2", "3", ">=4")
  ) + 
  scale_y_continuous(limits = c(-0.4, 1.2), breaks = seq(-0.4, 1.2, 0.2)) +
  labs(
    title = "Figure 2. Dynamic effects of subway approvals on mayor promotion",
    y = "Effect of Approval on Mayoral Promotion in 3 Years", x = ""
  ) +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Figure_2_Replication_Base.png", plot = p2, width = 8, height = 6)


# ==============================================================================
# 7. REPLICATE TABLE A14 ----
# ==============================================================================

# --- Models ---
m1 <- m_fig2

f2 <- as.formula(paste(
  "Mayor_promotion3y ~ mpprior5 + mpprior4 + mpprior3 + mpprior2 +",
  "mpconn0 + mpconn1 + mpconn2 + mpconn3 + mpconn4 + mppost_any +",
  paste(c(mayor_controls, base_controls), collapse = " + "),
  "| prov_year + City_Code"
))
m2 <- felm(f2, data = df_reg)

f3 <- as.formula(paste(
  "Mayor_promotion3y ~ mpprior5 + mpprior4 + mpprior3 + mpprior2 +",
  "mpconn0 + mpconn1 + mpconn2 + mpconn3 + mpconn4 +",
  "mppost1 + mppost2 + mppost3 + mppost4",
  "| Year + City_Code"
))
m3 <- felm(f3, data = df_reg)

f4 <- as.formula(paste(
  "Mayor_promotion3y ~ mpprior5 + mpprior4 + mpprior3 + mpprior2 +",
  "mpconn0 + mpconn1 + mpconn2 + mpconn3 + mpconn4 +",
  "mppost1 + mppost2 + mppost3 + mppost4 +",
  paste(c(mayor_controls, base_controls), collapse = " + "),
  "| prov_year + City_Code"
))
m4 <- felm(f4, data = df_reg)

# --- Formatting ---

# Custom labels
labels_a14 <- c(
  "To be approved in 5 or more years", "To be approved in 4 years", 
  "To be approved in 3 years", "To be approved in 2 years",
  "Approval year", "Approved for 1 year", "Approved for 2 years", 
  "Approved for 3 years", "Approved for 4 or more years",
  "Mayor left (Any year)", 
  "Mayor left for 1 year", "Mayor left for 2 years", 
  "Mayor left for 3 years", "Mayor left for 4 or more years"
)

# Custom footer lines including the Baseline Reference row
lines_a14 <- list(
  c("City FE", "Yes", "Yes", "Yes", "Yes"),
  c("Year FE", "Yes", "YeS", "Yes", "YeS"),
  c("Province-year FE", "No", "Yes", "No", "Yes"),
  c("Baseline controls", "No", "Yes", "No", "Yes"),
  c("Observations", m1$N, m2$N, m3$N, m4$N)
)

# Capture Output
tab_text_a14 <- capture.output(
  stargazer(m1, m2, m3, m4,
            type = "text",
            title = "Table A14. Test Parallel Trends Assumption",
            keep = c("mpprior", "mpconn", "mppost"),
            covariate.labels = labels_a14,
            add.lines = lines_a14,
            digits = 3,
            star.cutoffs = c(0.1, 0.05, 0.01),
            omit.stat = c("n", "f", "ser", "adj.rsq"),
            model.names = FALSE, 
            model.numbers = FALSE, # Remove auto numbering
            column.labels = c("(1)", "(2)", "(3)", "(4)"), # Explicit labels
            dep.var.labels.include = FALSE, # Clean header
            notes = "Note. Standard errors clustered at the city level are reported in parentheses.",
            notes.append = FALSE, # Replace default notes
            notes.align = "l"
  )
)

# Save PNG
png("Table_A14_Replication_Base.png", height = 1300, width = 1000)
plot.new()
text(0.5, 0.5, paste(tab_text_a14, collapse = "\n"), cex = 0.85, family = "mono")
dev.off()




# ==============================================================================
# 8. DYNAMIC ANALYSIS BY LAND DEPENDENCE SUBGROUPS ----
# ==============================================================================

# 8.1 PREPARE DATA WITH LAND DEPENDENCE GROUPS 

# Merge High_Land_Group into df_reg if not already there
if(!"High_Land_Group" %in% names(df_reg)) {
  df_reg <- df_reg |>
    left_join(
      baseline_mayor |> select(City_Code, Mayor_leaderindex, High_Land_Group),
      by = c("City_Code", "Mayor_leaderindex")
    )
}

# Filter for Low Land Dependence Group
df_reg_low <- df_reg |> 
  filter(High_Land_Group == 0)

# Filter for High Land Dependence Group  
df_reg_high <- df_reg |> 
  filter(High_Land_Group == 1)

cat("\nSample sizes:\n")
cat("Low Land Dependence Group: N =", nrow(df_reg_low), "\n")
cat("High Land Dependence Group: N =", nrow(df_reg_high), "\n\n")

# 8.2 FIGURE FOR LOW LAND DEPENDENCE GROUP


cat("Creating Figure for Low Land Dependence Group...\n")

# Model: No controls, mppost_any included but not plotted
f_fig_low <- as.formula(paste(
  "Mayor_promotion3y ~ mpprior5 + mpprior4 + mpprior3 + mpprior2 +",
  "mpconn0 + mpconn1 + mpconn2 + mpconn3 + mpconn4 + mppost_any",
  "| Year + City_Code"
))

m_fig_low <- felm(f_fig_low, data = df_reg_low)

# Extract Data
coefs_low <- summary(m_fig_low)$coefficients
se_low <- coefs_low[, 2]
est_low <- coefs_low[, 1]

plot_vars <- c("mpprior5", "mpprior4", "mpprior3", "mpprior2",
               "mpconn0", "mpconn1", "mpconn2", "mpconn3", "mpconn4")

df_plot_low <- data.frame(
  term = rownames(coefs_low),
  estimate = est_low,
  se = se_low
) |> filter(term %in% plot_vars)

# Add Reference
df_plot_low <- rbind(df_plot_low, data.frame(term = "mpprior1", estimate = 0, se = 0))

# Coordinates
x_map <- c("mpprior5"=-5, "mpprior4"=-4, "mpprior3"=-3, "mpprior2"=-2, "mpprior1"=-1,
           "mpconn0"=0, "mpconn1"=1, "mpconn2"=2, "mpconn3"=3, "mpconn4"=4)
df_plot_low$x <- x_map[df_plot_low$term]

# CIs
df_plot_low$ci95_lo <- df_plot_low$estimate - 1.96 * df_plot_low$se
df_plot_low$ci95_hi <- df_plot_low$estimate + 1.96 * df_plot_low$se
df_plot_low$ci90_lo <- df_plot_low$estimate - 1.645 * df_plot_low$se
df_plot_low$ci90_hi <- df_plot_low$estimate + 1.645 * df_plot_low$se

# Plot
p_low <- ggplot(df_plot_low, aes(x = x, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "black") +
  geom_errorbar(aes(ymin = ci95_lo, ymax = ci95_hi), width = 0, size = 0.5) +
  geom_errorbar(aes(ymin = ci90_lo, ymax = ci90_hi), width = 0, size = 1.2) +
  geom_point(fill = "white", shape = 21, size = 3, stroke = 1) +
  scale_x_continuous(
    breaks = -5:4, 
    labels = c("â¤-5", "-4", "-3", "-2", "-1", "0", "1", "2", "3", ">=4")
  ) + 
  scale_y_continuous(limits = c(-0.4, 2.2), breaks = seq(-0.4, 2.0, 0.4)) +
  labs(
    title = "Dynamic Effects: Low Land Finance Dependence Group",
    subtitle = "Effect of subway approval on mayor promotion",
    y = "Effect on Promotion within 3 Years", 
    x = "Years Relative to Subway Approval"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  )

ggsave("Figure_1_low.png", plot = p_low, width = 8, height = 6, dpi = 300)

# 8.3 FIGURE FOR HIGH LAND DEPENDENCE GROUP

cat("Creating Figure for High Land Dependence Group...\n")

# Model: No controls, mppost_any included but not plotted
f_fig_high <- as.formula(paste(
  "Mayor_promotion3y ~ mpprior5 + mpprior4 + mpprior3 + mpprior2 +",
  "mpconn0 + mpconn1 + mpconn2 + mpconn3 + mpconn4 + mppost_any",
  "| Year + City_Code"
))

m_fig_high <- felm(f_fig_high, data = df_reg_high)

# Extract Data
coefs_high <- summary(m_fig_high)$coefficients
se_high <- coefs_high[, 2]
est_high <- coefs_high[, 1]

df_plot_high <- data.frame(
  term = rownames(coefs_high),
  estimate = est_high,
  se = se_high
) |> filter(term %in% plot_vars)

# Add Reference
df_plot_high <- rbind(df_plot_high, data.frame(term = "mpprior1", estimate = 0, se = 0))

# Coordinates
df_plot_high$x <- x_map[df_plot_high$term]

# CIs
df_plot_high$ci95_lo <- df_plot_high$estimate - 1.96 * df_plot_high$se
df_plot_high$ci95_hi <- df_plot_high$estimate + 1.96 * df_plot_high$se
df_plot_high$ci90_lo <- df_plot_high$estimate - 1.645 * df_plot_high$se
df_plot_high$ci90_hi <- df_plot_high$estimate + 1.645 * df_plot_high$se

# Plot
p_high <- ggplot(df_plot_high, aes(x = x, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "black") +
  geom_errorbar(aes(ymin = ci95_lo, ymax = ci95_hi), width = 0, size = 0.5) +
  geom_errorbar(aes(ymin = ci90_lo, ymax = ci90_hi), width = 0, size = 1.2) +
  geom_point(fill = "white", shape = 21, size = 3, stroke = 1) +
  scale_x_continuous(
    breaks = -5:4, 
    labels = c("â¤-5", "-4", "-3", "-2", "-1", "0", "1", "2", "3", ">=4")
  ) + 
  scale_y_continuous(limits = c(-0.5, 2.2), breaks = seq(-0.4, 2.0, 0.4)) +
  labs(
    title = "Dynamic Effects: High Land Finance Dependence Group",
    subtitle = "Effect of subway approval on mayor promotion",
    y = "Effect on Promotion within 3 Years", 
    x = "Years Relative to Subway Approval"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  )

ggsave("figure_1_high.png", plot = p_high, width = 8, height = 6, dpi = 300)


# -----------------------------------------------------------------------------
# 8.4 APPENDIX - TABLE 3 REPLICATED A14 FOR LOW LAND DEPENDENCE GROUP ----
# -----------------------------------------------------------------------------

cat("\nCreating Table A14 for Low Land Dependence Group...\n")

# Model 1: Basic
m1_low <- m_fig_low

# Model 2: Full controls + Province-Year FE
f2_low <- as.formula(paste(
  "Mayor_promotion3y ~ mpprior5 + mpprior4 + mpprior3 + mpprior2 +",
  "mpconn0 + mpconn1 + mpconn2 + mpconn3 + mpconn4 + mppost_any +",
  paste(c(mayor_controls, base_controls), collapse = " + "),
  "| prov_year + City_Code"
))
m2_low <- felm(f2_low, data = df_reg_low)

# Model 3: Post disaggregated
f3_low <- as.formula(paste(
  "Mayor_promotion3y ~ mpprior5 + mpprior4 + mpprior3 + mpprior2 +",
  "mpconn0 + mpconn1 + mpconn2 + mpconn3 + mpconn4 +",
  "mppost1 + mppost2 + mppost3 + mppost4",
  "| Year + City_Code"
))
m3_low <- felm(f3_low, data = df_reg_low)

# Model 4: Full with disaggregated post
f4_low <- as.formula(paste(
  "Mayor_promotion3y ~ mpprior5 + mpprior4 + mpprior3 + mpprior2 +",
  "mpconn0 + mpconn1 + mpconn2 + mpconn3 + mpconn4 +",
  "mppost1 + mppost2 + mppost3 + mppost4 +",
  paste(c(mayor_controls, base_controls), collapse = " + "),
  "| prov_year + City_Code"
))
m4_low <- felm(f4_low, data = df_reg_low)

# Custom labels
labels_a14 <- c(
  "To be approved in 5 or more years", "To be approved in 4 years", 
  "To be approved in 3 years", "To be approved in 2 years",
  "Approval year", "Approved for 1 year", "Approved for 2 years", 
  "Approved for 3 years", "Approved for 4 or more years",
  "Mayor left (Any year)", 
  "Mayor left for 1 year", "Mayor left for 2 years", 
  "Mayor left for 3 years", "Mayor left for 4 or more years"
)

# Custom footer lines (CORRECTED - All Year FE = Yes)
lines_low <- list(
  c("City FE", "Yes", "Yes", "Yes", "Yes"),
  c("Year FE", "Yes", "Yes", "Yes", "Yes"),  # CORRECTED: All Yes
  c("Province-year FE", "No", "Yes", "No", "Yes"),
  c("Baseline controls", "No", "Yes", "No", "Yes"),
  c("Observations", m1_low$N, m2_low$N, m3_low$N, m4_low$N)
)

# Capture Output
tab_text_low <- capture.output(
  stargazer(m1_low, m2_low, m3_low, m4_low,
            type = "text",
            title = "Table: Parallel Trends Test - Low Land Finance Dependence Group",
            keep = c("mpprior", "mpconn", "mppost"),
            covariate.labels = labels_a14,
            add.lines = lines_low,
            digits = 3,
            star.cutoffs = c(0.1, 0.05, 0.01),
            omit.stat = c("n", "f", "ser", "adj.rsq"),
            model.names = FALSE, 
            model.numbers = FALSE,
            column.labels = c("(1)", "(2)", "(3)", "(4)"),
            dep.var.labels.include = FALSE,
            notes = c("Standard errors clustered at city level. Low land dependence = below median pre-tenure land finance ratio.",
                      "This table reports the regression results for Figure_low. FE = fixed effects."),
            notes.append = FALSE,
            notes.align = "l"
  )
)

# Save PNG
png("app_table_3.png", height = 1300, width = 1000)
plot.new()
text(0.5, 0.5, paste(tab_text_low, collapse = "\n"), cex = 0.85, family = "mono")
dev.off()

# -----------------------------------------------------------------------------
# 8.5 APPENDIX - TABLE 4 REPLICATED A14 FOR LOW LAND DEPENDENCE GROUP ----
# -----------------------------------------------------------------------------

cat("Creating Table A14 for High Land Dependence Group...\n")

# Model 1: Basic
m1_high <- m_fig_high

# Model 2: Full controls + Province-Year FE
f2_high <- as.formula(paste(
  "Mayor_promotion3y ~ mpprior5 + mpprior4 + mpprior3 + mpprior2 +",
  "mpconn0 + mpconn1 + mpconn2 + mpconn3 + mpconn4 + mppost_any +",
  paste(c(mayor_controls, base_controls), collapse = " + "),
  "| prov_year + City_Code"
))
m2_high <- felm(f2_high, data = df_reg_high)

# Model 3: Post disaggregated
f3_high <- as.formula(paste(
  "Mayor_promotion3y ~ mpprior5 + mpprior4 + mpprior3 + mpprior2 +",
  "mpconn0 + mpconn1 + mpconn2 + mpconn3 + mpconn4 +",
  "mppost1 + mppost2 + mppost3 + mppost4",
  "| Year + City_Code"
))
m3_high <- felm(f3_high, data = df_reg_high)

# Model 4: Full with disaggregated post
f4_high <- as.formula(paste(
  "Mayor_promotion3y ~ mpprior5 + mpprior4 + mpprior3 + mpprior2 +",
  "mpconn0 + mpconn1 + mpconn2 + mpconn3 + mpconn4 +",
  "mppost1 + mppost2 + mppost3 + mppost4 +",
  paste(c(mayor_controls, base_controls), collapse = " + "),
  "| prov_year + City_Code"
))
m4_high <- felm(f4_high, data = df_reg_high)

# Custom footer lines (CORRECTED - All Year FE = Yes)
lines_high <- list(
  c("City FE", "Yes", "Yes", "Yes", "Yes"),
  c("Year FE", "Yes", "Yes", "Yes", "Yes"),  # CORRECTED: All Yes
  c("Province-year FE", "No", "Yes", "No", "Yes"),
  c("Baseline controls", "No", "Yes", "No", "Yes"),
  c("Observations", m1_high$N, m2_high$N, m3_high$N, m4_high$N)
)

# Capture Output
tab_text_high <- capture.output(
  stargazer(m1_high, m2_high, m3_high, m4_high,
            type = "text",
            title = "Table: Parallel Trends Test - High Land Finance Dependence Group",
            keep = c("mpprior", "mpconn", "mppost"),
            covariate.labels = labels_a14,
            add.lines = lines_high,
            digits = 3,
            star.cutoffs = c(0.1, 0.05, 0.01),
            omit.stat = c("n", "f", "ser", "adj.rsq"),
            model.names = FALSE, 
            model.numbers = FALSE,
            column.labels = c("(1)", "(2)", "(3)", "(4)"),
            dep.var.labels.include = FALSE,
            notes = c("Standard errors clustered at city level. High land dependence = above median pre-tenure land finance ratio.",
                      "This table reports the regression results for Figure_high. FE = fixed effects."),
            notes.append = FALSE,
            notes.align = "l"
  )
)

# Save PNG
png("app_table_4.png", height = 1300, width = 1000)
plot.new()
text(0.5, 0.5, paste(tab_text_high, collapse = "\n"), cex = 0.85, family = "mono")
dev.off()


# Extract key coefficients for comparison
get_coef_se <- function(model, var) {
  coef <- summary(model)$coefficients
  if(var %in% rownames(coef)) {
    return(list(est = coef[var, 1], se = coef[var, 2]))
  } else {
    return(list(est = NA, se = NA))
  }
}

# Compare approval year effects
approval_low <- get_coef_se(m1_low, "mpconn0")
approval_high <- get_coef_se(m1_high, "mpconn0")

cat("\nApproval Year Effect (mpconn0):\n")
cat("--------------------------------\n")
cat(sprintf("Low Land Dependence: %.3f (SE: %.3f)\n", approval_low$est, approval_low$se))
cat(sprintf("High Land Dependence: %.3f (SE: %.3f)\n", approval_high$est, approval_high$se))
cat(sprintf("Difference: %.3f\n", approval_high$est - approval_low$est))

# Year 1 effect
year1_low <- get_coef_se(m1_low, "mpconn1")
year1_high <- get_coef_se(m1_high, "mpconn1")

cat("\nYear 1 After Approval (mpconn1):\n")
cat("---------------------------------\n")
cat(sprintf("Low Land Dependence: %.3f (SE: %.3f)\n", year1_low$est, year1_low$se))
cat(sprintf("High Land Dependence: %.3f (SE: %.3f)\n", year1_high$est, year1_high$se))
cat(sprintf("Difference: %.3f\n", year1_high$est - year1_low$est))



# -----------------------------------------------------------------------------
# 8.6 F-TESTS FOR JOINT SIGNIFICANCE OF PRE-TRENDS AND TREATMENT EFFECTS ----
# -----------------------------------------------------------------------------

# Function to perform F-test for felm models
perform_f_test <- function(model, vars_to_test, description) {
  # Get coefficient names
  coef_names <- names(coef(model))
  
  # Find matching variables
  test_vars <- coef_names[coef_names %in% vars_to_test]
  
  if(length(test_vars) > 0) {
    # Create hypothesis string for linearHypothesis
    hyp_string <- paste(test_vars, "= 0")
    
    # Perform F-test using linearHypothesis
    f_test <- linearHypothesis(model, hyp_string, test = "F")
    
    cat(sprintf("\n%s:\n", description))
    cat(sprintf("  Variables tested: %s\n", paste(test_vars, collapse = ", ")))
    cat(sprintf("  F-statistic: %.3f\n", f_test$F[2]))
    cat(sprintf("  Df: %d\n", f_test$Df[2]))
    cat(sprintf("  p-value: %.4f\n", f_test$`Pr(>F)`[2]))
    cat(sprintf("  Significance: %s\n", 
                ifelse(f_test$`Pr(>F)`[2] < 0.01, "***",
                       ifelse(f_test$`Pr(>F)`[2] < 0.05, "**",
                              ifelse(f_test$`Pr(>F)`[2] < 0.1, "*", "ns")))))
    return(f_test)
  } else {
    cat(sprintf("\n%s: Variables not found in model\n", description))
    return(NULL)
  }
}

# Variables to test
pre_trend_vars <- c("mpprior5", "mpprior4", "mpprior3", "mpprior2")

# LOW LAND DEPENDENCE GROUP

f_test_low_pre <- perform_f_test(m1_low, pre_trend_vars, 
                                 "Joint test: Pre-trends (t=-5,-4,-3,-2) = 0")

# HIGH LAND DEPENDENCE GROUP  

f_test_high_pre <- perform_f_test(m1_high, pre_trend_vars,
                                  "Joint test: Pre-trends (t=-5,-4,-3,-2) = 0")

# COMPARATIVE SUMMARY

cat("PARALLEL TRENDS ASSUMPTION:")
cat("   Low Land Group:  F-stat = ", round(f_test_low_pre$F[2], 3), 
    ", p-value = ", round(f_test_low_pre$`Pr(>F)`[2], 4), "\n")
cat("   High Land Group: F-stat = ", round(f_test_high_pre$F[2], 3),
    ", p-value = ", round(f_test_high_pre$`Pr(>F)`[2], 4), "\n")




# ==============================================================================
# 9. COMBINED FIGURE: ORIGINAL + SUBGROUP DYNAMIC ANALYSIS ----
# ==============================================================================



# 9.1 RECREATE FIGURE 2 (PANEL A)


p_original <- ggplot(df_plot, aes(x = x, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "black") +
  geom_errorbar(aes(ymin = ci95_lo, ymax = ci95_hi), width = 0, size = 0.5) +
  geom_errorbar(aes(ymin = ci90_lo, ymax = ci90_hi), width = 0, size = 1.2) +
  geom_point(fill = "white", shape = 21, size = 3, stroke = 1) +
  scale_x_continuous(
    breaks = -5:4, 
    labels = c("â¤-5", "-4", "-3", "-2", "-1", "0", "1", "2", "3", ">=4")
  ) + 
  scale_y_continuous(limits = c(-0.5, 2.2), breaks = seq(-0.4, 2.0, 0.4)) +
  labs(
    title = "(A) Full Sample",
    subtitle = "All mayors (N = 3,647 observations)",
    y = "Effect on Promotion within 3 Years", 
    x = ""
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0, face = "bold", size = 13),
    plot.subtitle = element_text(hjust = 0, size = 11, color = "gray40"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11)
  )


# 9.2 RECREATE FIGURE LOW (PANEL B) 

p_low_panel <- ggplot(df_plot_low, aes(x = x, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "black") +
  geom_errorbar(aes(ymin = ci95_lo, ymax = ci95_hi), width = 0, size = 0.5) +
  geom_errorbar(aes(ymin = ci90_lo, ymax = ci90_hi), width = 0, size = 1.2) +
  geom_point(fill = "white", shape = 21, size = 3, stroke = 1) +
  scale_x_continuous(
    breaks = -5:4, 
    labels = c("â¤-5", "-4", "-3", "-2", "-1", "0", "1", "2", "3", ">=4")
  ) + 
  scale_y_continuous(limits = c(-0.5, 2.2), breaks = seq(-0.4, 2.0, 0.4)) +
  labs(
    title = "(B) Low Land Finance Dependence",
    subtitle = paste0("Below median pre-tenure land finance ratio (N = ", 
                      format(nrow(df_reg_low), big.mark = ","), " observations)"),
    y = "Effect on Promotion within 3 Years", 
    x = ""
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0, face = "bold", size = 13),
    plot.subtitle = element_text(hjust = 0, size = 11, color = "gray40"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11)
  )


# 9.3 RECREATE FIGURE HIGH (PANEL C) 

p_high_panel <- ggplot(df_plot_high, aes(x = x, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "black") +
  geom_errorbar(aes(ymin = ci95_lo, ymax = ci95_hi), width = 0, size = 0.5) +
  geom_errorbar(aes(ymin = ci90_lo, ymax = ci90_hi), width = 0, size = 1.2) +
  geom_point(fill = "white", shape = 21, size = 3, stroke = 1) +
  scale_x_continuous(
    breaks = -5:4, 
    labels = c("â¤-5", "-4", "-3", "-2", "-1", "0", "1", "2", "3", ">=4")
  ) + 
  scale_y_continuous(limits = c(-0.5, 2.2), breaks = seq(-0.4, 2.0, 0.4)) +
  labs(
    title = "(C) High Land Finance Dependence",
    subtitle = paste0("Above median pre-tenure land finance ratio (N = ", 
                      format(nrow(df_reg_high), big.mark = ","), " observations)"),
    y = "Effect on Promotion within 3 Years", 
    x = "Years Relative to Subway Approval"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0, face = "bold", size = 13),
    plot.subtitle = element_text(hjust = 0, size = 11, color = "gray40"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11)
  )

# 9.4 COMBINE ALL THREE PANELS 


# Combine using patchwork
combined_figure <- p_original / p_low_panel / p_high_panel + 
  plot_layout(heights = c(1, 1, 1)) +
  plot_annotation(
    title = "Dynamic Effects of Subway Approvals on Mayor Promotion",
    subtitle = "By Pre-tenure Land Finance Dependence",
    caption = paste(
      "Notes: Point estimates and confidence intervals from event study regressions. Vertical dashed line separates pre- and post-approval periods.",
      "Horizontal dashed line indicates zero effect. Thick lines show 90% confidence intervals; thin lines show 95% confidence intervals.",
      "All models include city and year fixed effects. The omitted category is one year before approval (t = -1).",
      "Land finance dependence is measured as the ratio of land revenue to fiscal revenue in the three years before mayor's tenure.",
      "High/Low groups are defined by the median split of pre-tenure land finance dependence.",
      sep = "\n"
    ),
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray30"),
      plot.caption = element_text(size = 9, hjust = 0, lineheight = 1.2, color = "gray40")
    )
  )

# Save combined figure
ggsave("figure_1.png", 
       plot = combined_figure, 
       width = 10, 
       height = 14, 
       dpi = 300,
       bg = "white")


# ==============================================================================
# 10. COVARIATE/COLLINEARITY TEST: LAND DEPENDENCE VS FISCAL CAPACITY ----
# ==============================================================================


# 10.1 CORRELATION ANALYSIS 

# Get unique mayor-level data
mayor_level_data <- baseline_mayor |>
  filter(!is.na(land_dep_pre) & !is.na(fiscal_cap_pre))

# Calculate correlation
cor_test <- cor.test(mayor_level_data$land_dep_pre, 
                     mayor_level_data$fiscal_cap_pre)

cat("\n--- CORRELATION ANALYSIS ---\n")
cat(sprintf("Pearson correlation coefficient: %.4f\n", cor_test$estimate))



# 10.2 COLLINEARITY DIAGNOSTICS 

# Prepare data with both variables
collin_data <- df_ext |>
  filter(!is.na(land_dep_pre) & !is.na(fiscal_cap_pre))

# Model with both variables
collin_model <- lm(Mayor_promotion3y ~ Mayor_plan + High_Land_Group + 
                     fiscal_capacity + Mayor_plan:High_Land_Group + 
                     Mayor_plan:fiscal_capacity + High_Land_Group:fiscal_capacity,
                   data = collin_data)

# Calculate VIF
vif_values <- vif(collin_model)
cat("\n--- VARIANCE INFLATION FACTORS (VIF) ---\n")
print(vif_values)

# Create VIF table
vif_table_data <- data.frame(
  Variable = c("Subway Approval",
               "High Land Dependence", 
               "Fiscal Capacity",
               "Subway Ã High Land",
               "Subway Ã Fiscal Capacity",
               "High Land Ã Fiscal Capacity",
               "",
               "Maximum VIF",
               "Mean VIF",
               "Multicollinearity Assessment"),
  VIF = c(sprintf("%.3f", vif_values[1]),
          sprintf("%.3f", vif_values[2]),
          sprintf("%.3f", vif_values[3]),
          sprintf("%.3f", vif_values[4]),
          sprintf("%.3f", vif_values[5]),
          sprintf("%.3f", vif_values[6]),
          "",
          sprintf("%.3f", max(vif_values)),
          sprintf("%.3f", mean(vif_values)),
          ifelse(max(vif_values) > 10, "Severe",
                 ifelse(max(vif_values) > 5, "Moderate", "Low")))
)

# Create GT table for VIF
vif_table_gt <- gt(vif_table_data) |>
  tab_header(
    title = md("**Land & Fiscal - Variance Inflation Factors (VIF)**"),
    subtitle = md("*Multicollinearity Diagnostic Test*")
  ) |>
  cols_label(Variable = md("**Variable**"),
             VIF = md("**VIF Value**")) |>
  cols_align(align = "left", columns = 1) |>
  cols_align(align = "right", columns = 2) |>
  tab_style(
    style = list(cell_borders(sides = c("top", "bottom"), weight = px(1.5), color = "black")),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = list(cell_borders(sides = "top", weight = px(1), color = "gray50")),
    locations = cells_body(rows = 7)
  ) |>
  tab_style(
    style = list(cell_borders(sides = "bottom", weight = px(1.5), color = "black")),
    locations = cells_body(rows = nrow(vif_table_data))
  ) |>
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = 2, rows = 8:10)
  ) |>
  tab_footnote(
    footnote = md("Notes: VIF values from auxiliary regression including main effects and two-way interactions. Rule of thumb: VIF > 10 indicates severe multicollinearity; VIF > 5 indicates moderate multicollinearity; VIF < 5 indicates low multicollinearity. Higher VIF values suggest that the coefficient estimates may be unstable."),
    locations = cells_body(columns = 1, rows = 1)
  ) |>
  tab_options(
    table.font.size = px(11),
    column_labels.font.weight = "bold",
    footnotes.font.size = px(9),
    table.width = pct(100),
    data_row.padding = px(3)
  )

# Save VIF table
gtsave(vif_table_gt, "app_table_5.png", vwidth = 650, vheight = 500)

# 10.3 VISUALIZATION 

# Create scatter plot with enhanced details
scatter_plot <- ggplot(mayor_level_data, aes(x = land_dep_pre, y = fiscal_cap_pre)) +
  geom_point(alpha = 0.4, size = 2, color = "gray30") +
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "lightblue", alpha = 0.3) +
  # Add correlation info to plot
  annotate("text", 
           x = max(mayor_level_data$land_dep_pre) * 0.75,
           y = min(mayor_level_data$fiscal_cap_pre) + 
             (max(mayor_level_data$fiscal_cap_pre) - min(mayor_level_data$fiscal_cap_pre)) * 0.1,
           label = sprintf("r = %.3f\np < %.4f", cor_test$estimate, cor_test$p.value),
           hjust = 0, vjust = 0, size = 4, fontface = "bold") +
  labs(
    title = "Relationship between Land Finance Dependence and Fiscal Capacity",
    subtitle = sprintf("Mayor-level pre-tenure averages (N = %d mayors)", nrow(mayor_level_data)),
    x = "Land Finance Dependence\n(Land Revenue / Fiscal Revenue)",
    y = "Fiscal Capacity\n(Fiscal Revenue / GDP)",
    caption = paste(
      "Notes: Each point represents a mayor's pre-tenure average fiscal conditions.",
      sprintf("Pearson correlation r = %.3f (95%% CI: [%.3f, %.3f]).", 
              cor_test$estimate, cor_test$conf.int[1], cor_test$conf.int[2]),
      ifelse(cor_test$p.value < 0.001, "Correlation is statistically significant at p < 0.001 level.",
             ifelse(cor_test$p.value < 0.05, sprintf("Correlation is statistically significant (p = %.4f).",
                                                     cor_test$p.value),
                    sprintf("Correlation is not statistically significant (p = %.4f).", 
                            cor_test$p.value))),
      sep = "\n"
    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
    plot.caption = element_text(size = 9, hjust = 0, lineheight = 1.1, color = "gray40"),
    axis.title = element_text(size = 11),
    panel.grid.minor = element_blank()
  )

# Save scatter plot
ggsave("app_figure_1.png", scatter_plot, width = 9, height = 7, dpi = 300)



# ==============================================================================
# 11. APPENDIX TABLE 6: TRIPLE INTERACTION MODEL ----
# ==============================================================================


# 11.1 PREPARE DATA 

# Center fiscal capacity if not already done
if(!"fiscal_capacity_centered" %in% names(df_ext)) {
  mean_fiscal_capacity <- mean(df_ext$fiscal_capacity, na.rm = TRUE)
  df_ext <- df_ext |>
    mutate(fiscal_capacity_centered = fiscal_capacity - mean_fiscal_capacity)
}

# 11.2 RUN REGRESSION MODELS 

# Model 1: Baseline (subway only with mayor controls)
cat("\nRunning Model 1: Baseline...\n")
formula_baseline <- paste("Mayor_promotion3y ~ Mayor_plan +",
                          paste(mayor_controls, collapse = " + "),
                          "| City_Code + Year")
appendix5_model1 <- feols(
  as.formula(formula_baseline),
  data = df_ext,
  cluster = ~City_Code
)
cat("  N =", appendix5_model1$nobs, "\n")
cat("  Baseline coefficient:", round(coef(appendix5_model1)["Mayor_plan"], 3), "\n")

# Model 2: Full triple interaction
cat("\nRunning Model 2: Triple interaction...\n")
formula_triple <- paste("Mayor_promotion3y ~ Mayor_plan * High_Land_Group * fiscal_capacity_centered +",
                        paste(mayor_controls, collapse = " + "),
                        "| City_Code + Year")
appendix5_model2 <- feols(
  as.formula(formula_triple),
  data = df_ext,
  cluster = ~City_Code
)
cat("  N =", appendix5_model2$nobs, "\n")

# 11.3 EXTRACT RESULTS 

# Extract coefficients
results_app5 <- list(
  # Model 1
  m1_subway = extract_coef_robust(appendix5_model1, "Mayor_plan"),
  
  # Model 2 - all terms
  m2_subway = extract_coef_robust(appendix5_model2, "Mayor_plan"),
  m2_land = extract_coef_robust(appendix5_model2, "High_Land_Group"),
  m2_fiscal = extract_coef_robust(appendix5_model2, "fiscal_capacity_centered"),
  m2_subway_land = extract_coef_robust(appendix5_model2, "Mayor_plan:High_Land_Group"),
  m2_subway_fiscal = extract_coef_robust(appendix5_model2, "Mayor_plan:fiscal_capacity_centered"),
  m2_land_fiscal = extract_coef_robust(appendix5_model2, "High_Land_Group:fiscal_capacity_centered"),
  m2_triple = extract_coef_robust(appendix5_model2, "Mayor_plan:High_Land_Group:fiscal_capacity_centered")
)

# 11.4 CREATE TABLE 

appendix5_data <- data.frame(
  Variable = c(
    "Subway approval",
    "",
    "High Land Dependence", 
    "",
    "Fiscal capacity (centered)",
    "",
    "Subway Ã High Land",
    "",
    "Subway Ã Fiscal capacity",
    "",
    "High Land Ã Fiscal capacity",
    "",
    "Subway Ã High Land Ã Fiscal",
    "",
    "",
    "City FE",
    "Year FE",
    "Mayor controls",
    "",
    "Dependent variable mean",
    "R-squared",
    "Observations"
  ),
  
  `(1) Baseline` = c(
    results_app5$m1_subway$coef,
    results_app5$m1_subway$se,
    rep("", 12),
    "",
    "Yes", "Yes", "Yes",
    "",
    sprintf("%.3f", mean(df_ext$Mayor_promotion3y, na.rm = TRUE)),
    sprintf("%.3f", r2(appendix5_model1)["r2"]),
    format(appendix5_model1$nobs, big.mark = ",")
  ),
  
  `(2) Triple Interaction` = c(
    results_app5$m2_subway$coef,
    results_app5$m2_subway$se,
    results_app5$m2_land$coef,
    results_app5$m2_land$se,
    results_app5$m2_fiscal$coef,
    results_app5$m2_fiscal$se,
    results_app5$m2_subway_land$coef,
    results_app5$m2_subway_land$se,
    results_app5$m2_subway_fiscal$coef,
    results_app5$m2_subway_fiscal$se,
    results_app5$m2_land_fiscal$coef,
    results_app5$m2_land_fiscal$se,
    results_app5$m2_triple$coef,
    results_app5$m2_triple$se,
    "",
    "Yes", "Yes", "Yes",
    "",
    sprintf("%.3f", mean(df_ext$Mayor_promotion3y, na.rm = TRUE)),
    sprintf("%.3f", r2(appendix5_model2)["r2"]),
    format(appendix5_model2$nobs, big.mark = ",")
  ),
  
  check.names = FALSE
)

# Create GT table
appendix5_gt <- gt(appendix5_data) |>
  tab_header(
    title = md("**Triple Interaction Analysis**"),
    subtitle = md("*Dependent Variable: Mayor Promoted within Three Years*")
  ) |>
  cols_align(align = "center", columns = 2:3) |>
  cols_align(align = "left", columns = 1) |>
  tab_style(
    style = list(cell_borders(sides = c("top", "bottom"), weight = px(1.5), color = "black")),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = list(cell_borders(sides = "top", weight = px(0.5), color = "gray50")),
    locations = cells_body(rows = 16)
  ) |>
  tab_style(
    style = list(cell_borders(sides = "top", weight = px(1), color = "black")),
    locations = cells_body(rows = 20)
  ) |>
  tab_style(
    style = list(cell_borders(sides = "bottom", weight = px(1.5), color = "black")),
    locations = cells_body(rows = nrow(appendix5_data))
  ) |>
  tab_footnote(
    footnote = md(sprintf("Notes: Standard errors clustered at city level in parentheses. High Land Dependence = above median pre-tenure land finance ratio. Fiscal capacity centered at mean = %.4f. Model 1 includes subway approval and mayor controls. Model 2 includes all interactions up to the triple interaction term. Mayor controls include gender, ethnicity, age, education, political connection, and work experience.", mean_fiscal_capacity)),
    locations = cells_body(columns = 1, rows = 1)
  ) |>
  tab_source_note(source_note = md("\\* p < 0.1, ** p < 0.05, *** p < 0.01")) |>
  tab_options(
    table.font.size = px(11),
    column_labels.font.weight = "bold",
    footnotes.font.size = px(9),
    source_notes.font.size = px(9),
    table.width = pct(100),
    data_row.padding = px(3)
  )

# Save table
gtsave(appendix5_gt, "app_table_6.png", vwidth = 700, vheight = 850)



# The R File Ends here. ----

print("DONE")




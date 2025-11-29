#' =============================================================================
#' Robustness Check: Time-Varying Land Finance Dependency Analysis of the Extension Paper
#' Signal Scarcity and Screening Gates: Rethinking the Fiscal Logic of 
#' Infrastructure-Based Promotion in China
#' PLSC536 Final Paper
#' By: Weijia Chen
#' =============================================================================
#'
#' This code addresses the concern that the High_Land_Group binary indicator
#' is time-varying at the mayor level (i.e., the same city may have different
#' status across different mayor tenures).
#'
#' Analysis includes:
#' 1. Descriptive statistics on cities that switch High/Low status
#' 2. Visualization of switching patterns over time
#' 3. Regression comparison: Full sample vs. stable (never-switching) cities
#'
#' =============================================================================

rm(list = ls())

# ==============================================================================
# 0. SETUP & PACKAGES ----
# ==============================================================================

library(tidyverse)
library(fixest)
library(haven)
library(gt)
library(scales)

theme_set(theme_classic() + theme(
  axis.text = element_text(color = "black", size = 11),
  plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
  panel.grid.major.y = element_blank(),
  panel.grid.minor = element_blank()
))

# ==============================================================================
# 1. DATA PREPARATION ----
# ==============================================================================

# This section is repeated what we did in the main R running file, so most notes have been deleted.
# If any question about these codes pops up, please refer back to the main R file. 
df_main <- read_dta("subway_analysis_use.dta")

mayor_controls <- c("gender2", "race6", "Mayor_age", "Mayor_c_edu", 
                    "Mayor_c_central_exp", "Mayor_c_prov_exp", 
                    "Mayor_c_county_exp", "Mayor_c_soe_exp", 
                    "Mayor_c_univ_exp", "Mayor_c_league", 
                    "Mayor_connection_work")

base_controls <- c("lpop_1", "lgdp_1", "lrev_1", "GRP_growth_1")

safe_mean <- function(x) {
  if (length(x) == 0) return(NA_real_)
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

df_analysis <- df_main |>
  filter(fsj2 == 0) |>
  arrange(City_Code, Year) |>
  mutate(
    Year_num = as.numeric(Year),
    land_share = land_per / rev_per,
    fiscal_capacity_raw = rev_per / GRP_per
  )

mayor_start <- df_analysis |>
  group_by(City_Code, Mayor_leaderindex) |>
  summarise(start_year = min(Year_num, na.rm = TRUE), .groups = "drop")

df_analysis <- df_analysis |>
  left_join(mayor_start, by = c("City_Code", "Mayor_leaderindex"))

K_window <- 3

df_pre_tenure <- df_analysis |>
  filter(!is.na(start_year), Year_num < start_year, Year_num >= start_year - K_window)

baseline_mayor <- df_pre_tenure |>
  group_by(City_Code, Mayor_leaderindex) |>
  summarise(
    land_dep_pre = safe_mean(land_share),
    fiscal_cap_pre = safe_mean(fiscal_capacity_raw),
    .groups = "drop"
  )

all_mayors <- df_analysis |> distinct(City_Code, Mayor_leaderindex, start_year)
missing_baseline <- all_mayors |> anti_join(baseline_mayor, by = c("City_Code", "Mayor_leaderindex"))

if (nrow(missing_baseline) > 0) {
  baseline_fallback <- df_analysis |>
    semi_join(missing_baseline, by = c("City_Code", "Mayor_leaderindex")) |>
    filter(Year_num == start_year) |>
    group_by(City_Code, Mayor_leaderindex) |>
    summarise(
      land_dep_pre = safe_mean(land_share),
      fiscal_cap_pre = safe_mean(fiscal_capacity_raw),
      .groups = "drop"
    )
  baseline_mayor <- bind_rows(baseline_mayor, baseline_fallback)
}

median_land <- median(baseline_mayor$land_dep_pre, na.rm = TRUE)
median_fiscal <- median(baseline_mayor$fiscal_cap_pre, na.rm = TRUE)

baseline_mayor <- baseline_mayor |>
  mutate(
    High_Land_Group = if_else(land_dep_pre >= median_land, 1L, 0L),
    Low_Fiscal_Group = if_else(fiscal_cap_pre < median_fiscal, 1L, 0L)
  )

df_analysis <- df_analysis |>
  left_join(baseline_mayor, by = c("City_Code", "Mayor_leaderindex")) |>
  mutate(
    land_dependence = land_dep_pre,
    fiscal_capacity = fiscal_cap_pre
  )

df_final <- df_analysis |>
  filter(!is.na(High_Land_Group), !is.na(Low_Fiscal_Group))

# ==============================================================================
# 2. IDENTIFY SWITCHING BEHAVIOR ----
# ==============================================================================

city_status_summary <- df_final |>
  group_by(City_Code) |>
  summarise(
    n_mayors = n_distinct(Mayor_leaderindex),
    n_status_values = n_distinct(High_Land_Group),
    first_year = min(Year),
    last_year = max(Year),
    n_obs = n(),
    .groups = "drop"
  ) |>
  mutate(
    is_switcher = n_status_values > 1,
    status_type = if_else(is_switcher, "Switcher", "Never Switcher")
  )

switcher_codes <- city_status_summary |> 
  filter(is_switcher) |> 
  pull(City_Code)

never_switcher_codes <- city_status_summary |> 
  filter(!is_switcher) |> 
  pull(City_Code)

# Print descriptive statistics
print("==============================================================================")
print("DESCRIPTIVE STATISTICS: CITY SWITCHING BEHAVIOR")
print("==============================================================================")
print(sprintf("Total cities in sample: %d", nrow(city_status_summary)))
print(sprintf("Cities that switched status: %d (%.1f%%)", 
              length(switcher_codes), 
              100 * length(switcher_codes) / nrow(city_status_summary)))
print(sprintf("Cities that never switched: %d (%.1f%%)", 
              length(never_switcher_codes),
              100 * length(never_switcher_codes) / nrow(city_status_summary)))

# ==============================================================================
# 3. FIGURES: SWITCHING PATTERNS OVER TIME ----
# ==============================================================================

df_plot_switchers <- df_final |>
  filter(City_Code %in% switcher_codes) |>
  select(City_Code, Year, High_Land_Group, Mayor_leaderindex) |>
  mutate(
    City_Label = factor(City_Code),
    Status_Label = factor(High_Land_Group, 
                          levels = c(0, 1), 
                          labels = c("Low Land Dep.", "High Land Dep."))
  )

# Figure 1: Tile Plot
p_tile <- ggplot(df_plot_switchers, 
                 aes(x = Year, y = reorder(City_Label, -as.numeric(City_Label)), fill = Status_Label)) +
  geom_tile(color = "white", linewidth = 0.15) +
  scale_fill_manual(values = c("Low Land Dep." = "#3498db", 
                               "High Land Dep." = "#e74c3c")) +
  scale_x_continuous(breaks = seq(2003, 2016, 2), expand = c(0.01, 0.01)) +
  labs(
    title = "Time-Varying Land Finance Status: Switcher Cities",
    subtitle = sprintf("Cities that changed High/Low status at least once (N = %d)", length(switcher_codes)),
    x = "Year",
    y = "City Code",
    fill = "Status"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 5, color = "gray30"),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 10),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 10),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "gray95", color = NA)
  )

ggsave("app_h_figure1.png", plot = p_tile, width = 10, height = 14, dpi = 300)

# Figure 2: Area Chart
yearly_status <- df_plot_switchers |>
  group_by(Year, Status_Label) |>
  summarise(n = n_distinct(City_Code), .groups = "drop") |>
  group_by(Year) |>
  mutate(prop = n / sum(n))

p_area <- ggplot(yearly_status, aes(x = Year, y = prop, fill = Status_Label)) +
  geom_area(alpha = 0.85) +
  geom_line(aes(color = Status_Label), position = "stack", linewidth = 0.3) +
  scale_fill_manual(values = c("Low Land Dep." = "#3498db", 
                               "High Land Dep." = "#e74c3c")) +
  scale_color_manual(values = c("Low Land Dep." = "#2980b9", 
                                "High Land Dep." = "#c0392b"), guide = "none") +
  scale_x_continuous(breaks = seq(2003, 2016, 2)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0)) +
  labs(
    title = "Composition of Switcher Cities Over Time",
    subtitle = sprintf("Proportion in High vs. Low Land Dependence (N = %d cities)", length(switcher_codes)),
    x = "Year",
    y = "Proportion",
    fill = "Status"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 10)
  )

ggsave("app_h_figure2.png", plot = p_area, width = 10, height = 6, dpi = 300)

print("Figures saved: app_h_figure1.png (Tile), app_h_figure2.png (Area)")

# ==============================================================================
# 4. NEVER-SWITCHING CITIES BREAKDOWN ----
# ==============================================================================

never_switcher_status <- city_status_summary |>
  filter(!is_switcher) |>
  left_join(
    df_final |>
      group_by(City_Code) |>
      summarise(stable_status = first(High_Land_Group), .groups = "drop"),
    by = "City_Code"
  )

always_high <- never_switcher_status |> filter(stable_status == 1)
always_low <- never_switcher_status |> filter(stable_status == 0)

print("==============================================================================")
print("NEVER-SWITCHING CITIES BREAKDOWN")
print("==============================================================================")
print(sprintf("Total never-switching cities: %d", nrow(never_switcher_status)))
print(sprintf("Always High Land Dependence: %d cities", nrow(always_high)))
print(sprintf("Always Low Land Dependence: %d cities", nrow(always_low)))

# ==============================================================================
# 5. REGRESSION COMPARISON: FULL SAMPLE VS STABLE CITIES ----
# ==============================================================================

df_stable <- df_final |> filter(City_Code %in% never_switcher_codes)
df_stable_high <- df_stable |> filter(High_Land_Group == 1)
df_stable_low <- df_stable |> filter(High_Land_Group == 0)

df_full_high <- df_final |> filter(High_Land_Group == 1)
df_full_low <- df_final |> filter(High_Land_Group == 0)

formula_base <- paste("Mayor_promotion3y ~ Mayor_plan +", 
                      paste(c(mayor_controls, base_controls), collapse = " + "), 
                      "| City_Code + Year")

model_full_high <- feols(as.formula(formula_base), data = df_full_high, cluster = ~City_Code)
model_full_low <- feols(as.formula(formula_base), data = df_full_low, cluster = ~City_Code)
model_stable_high <- feols(as.formula(formula_base), data = df_stable_high, cluster = ~City_Code)
model_stable_low <- feols(as.formula(formula_base), data = df_stable_low, cluster = ~City_Code)


extract_coef_robust <- function(model, var_name) {
  if (!(var_name %in% names(coef(model)))) {
    return(list(coef = "", se = ""))
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
  return(list(coef = sprintf("%.3f%s", coef_val, stars), 
              se = sprintf("(%.3f)", se_val)))
}

res_full_high <- extract_coef_robust(model_full_high, "Mayor_plan")
res_full_low <- extract_coef_robust(model_full_low, "Mayor_plan")
res_stable_high <- extract_coef_robust(model_stable_high, "Mayor_plan")
res_stable_low <- extract_coef_robust(model_stable_low, "Mayor_plan")

get_dep_mean <- function(data, var = "Mayor_promotion3y") {
  sprintf("%.3f", mean(data[[var]], na.rm = TRUE))
}

# ==============================================================================
# 6. CREATE COMPARISON TABLE ----
# ==============================================================================

table_comparison_data <- data.frame(
  Variable = c(
    "Subway approval",
    "",
    "",
    "City FE",
    "Year FE",
    "Mayor controls",
    "City controls",
    "",
    "Sample",
    "Dependent variable mean",
    "R-squared",
    "Observations"
  ),
  
  `(1) High Land` = c(
    res_full_high$coef, res_full_high$se, "",
    "Yes", "Yes", "Yes", "Yes", "",
    "Full Sample",
    get_dep_mean(df_full_high),
    sprintf("%.3f", r2(model_full_high)["r2"]),
    format(model_full_high$nobs, big.mark = ",")
  ),
  
  `(2) Low Land` = c(
    res_full_low$coef, res_full_low$se, "",
    "Yes", "Yes", "Yes", "Yes", "",
    "Full Sample",
    get_dep_mean(df_full_low),
    sprintf("%.3f", r2(model_full_low)["r2"]),
    format(model_full_low$nobs, big.mark = ",")
  ),
  
  `(3) High Land` = c(
    res_stable_high$coef, res_stable_high$se, "",
    "Yes", "Yes", "Yes", "Yes", "",
    "Stable Only",
    get_dep_mean(df_stable_high),
    sprintf("%.3f", r2(model_stable_high)["r2"]),
    format(model_stable_high$nobs, big.mark = ",")
  ),
  
  `(4) Low Land` = c(
    res_stable_low$coef, res_stable_low$se, "",
    "Yes", "Yes", "Yes", "Yes", "",
    "Stable Only",
    get_dep_mean(df_stable_low),
    sprintf("%.3f", r2(model_stable_low)["r2"]),
    format(model_stable_low$nobs, big.mark = ",")
  ),
  
  check.names = FALSE
)

table_comparison_gt <- gt(table_comparison_data) |>
  tab_header(
    title = md("**Heterogeneous Effects: Full Sample vs. Stable Cities**"),
    subtitle = md("*Dependent Variable: Mayor Promoted within Three Years*")
  ) |>
  tab_spanner(label = "Full Sample", columns = c("(1) High Land", "(2) Low Land")) |>
  tab_spanner(label = "Stable Cities Only", columns = c("(3) High Land", "(4) Low Land")) |>
  cols_align(align = "center", columns = 2:5) |>
  cols_align(align = "left", columns = 1) |>
  cols_width(Variable ~ px(180), everything() ~ px(110)) |>
  tab_style(
    style = list(cell_borders(sides = c("top", "bottom"), weight = px(1.5), color = "black")),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = list(cell_borders(sides = c("bottom"), weight = px(1), color = "black")),
    locations = cells_column_spanners()
  ) |>
  tab_style(
    style = list(cell_borders(sides = "top", weight = px(0.5), color = "gray50")),
    locations = cells_body(rows = 4)
  ) |>
  tab_style(
    style = list(cell_borders(sides = "top", weight = px(1), color = "black")),
    locations = cells_body(rows = 9)
  ) |>
  tab_style(
    style = list(cell_borders(sides = "bottom", weight = px(1.5), color = "black")),
    locations = cells_body(rows = nrow(table_comparison_data))
  ) |>
  tab_footnote(
    footnote = md("Notes: Standard errors clustered at city level. Columns 1-2 use full sample. Columns 3-4 restrict to cities whose High/Low status never changed across mayor tenures."),
    locations = cells_body(columns = 1, rows = 1)
  ) |>
  tab_source_note(source_note = md("* p < 0.1, ** p < 0.05, *** p < 0.01")) |>
  tab_options(
    table.font.size = px(11),
    column_labels.font.weight = "bold",
    table.width = pct(80)
  )

gtsave(table_comparison_gt, "app_h_table1.png", vwidth = 700, vheight = 500)


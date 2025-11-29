#' =============================================================================
#' Robustness Check for Dynamic Event Study Analysis of the Extension Paper Based on Lei & Zhou 2022
#' Signal Scarcity and Screening Gates: Rethinking the Fiscal Logic of Infrastructure-Based Promotion in China
#' PLSC536 Final Paper 
#' By: Weijia Chen
#' =============================================================================
#' 
#' This code performs robustness checks for the dynamic event study analysis particularly related 
#' to the Figure 2 and Table A14 of the original paper and my paper's related parts too:
#' 
#' PART I (Appendix F): Boundary Handling Methods Comparison
#'   - Method 0: Original (Binning + default=0)
#'   - Method 1: Direct Truncation (No Binning)
#'   - Method 2: Balanced Sample (Exclude Boundary Cities)
#'   - Method 3: Simple Lag/Lead with NA (Following Rosa's approach)
#'
#' PART II (Appendix G): Control Variable and Sample Variation
#'   - Effect of including/excluding mppost_any control
#'   - Verification of sub-provincial city exclusion (Shenyang & Shenzhen)
#'
#' =============================================================================

rm(list = ls())

# ==============================================================================
# 0. SETUP & PACKAGES ----
# ==============================================================================

library(tidyverse)
library(haven)
library(lfe)
library(fixest)
library(stargazer)
library(ggplot2)
library(gridExtra)

theme_set(theme_classic() + theme(
  axis.text = element_text(color = "black", size = 11),
  plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
  panel.grid.major.y = element_blank(),
  panel.grid.minor = element_blank()
))

# ==============================================================================
# 1. DATA PREPARATION ----
# ==============================================================================

df_main <- read_dta("subway_analysis_use.dta")

# Main analysis sample (Prefecture-level cities, fsj2 == 0)
df_analysis <- df_main |>
  filter(fsj2 == 0) |>
  arrange(City_Code, Year)

# NOTE: Data Summary
# Observations: 3710, Cities: 265, Year range: 2003-2016

# ==============================================================================
# 2. HELPER FUNCTIONS ----
# ==============================================================================

safe_lag <- function(x, n=1) lag(x, n, default = 0)
safe_lead <- function(x, n=1) lead(x, n, default = 0)

row_sum_regex <- function(d, pat) {
  cols <- grep(pat, names(d), value = TRUE)
  if(length(cols) == 0) return(rep(0L, nrow(d)))
  as.integer(rowSums(d[, cols, drop=FALSE]) > 0)
}

# Function to create event study variables with mayor-level tracking
create_event_vars <- function(df, max_prior = 13, max_conn = 13, max_post = 13) {
  
  df_dyn <- df |> group_by(City_Code)
  
  # mpconn0: treatment start (with mayor tracking)
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
  
  # mpconn1-N: tenure effect indicators
  for(i in 1:max_conn) {
    curr <- paste0("mpconn", i)
    prev <- paste0("mpconn", i-1)
    df_dyn <- df_dyn |>
      mutate(!!curr := if_else(safe_lag(get(prev)) == 1 & 
                                 safe_lag(Mayor_leaderindex) == Mayor_leaderindex & 
                                 Mayor_plan == 1, 1, 0))
  }
  
  # mpprior1-N: pre-trend indicators
  for(i in 1:max_prior) {
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
  
  # mppost1-N: post-tenure indicators
  for(i in 1:max_post) {
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
  
  df_dyn |> ungroup()
}

# Variables to clean
vars_clean <- c(paste0("mpprior", 1:5), paste0("mpconn", 0:4), 
                paste0("mppost", 1:4), "mppost_any")

# Labels matching Table A14 format
labels_a14 <- c(
  "To be approved in 5 or more years", 
  "To be approved in 4 years", 
  "To be approved in 3 years", 
  "To be approved in 2 years",
  "Approval year", 
  "Approved for 1 year", 
  "Approved for 2 years", 
  "Approved for 3 years", 
  "Approved for 4 or more years",
  "Mayor left (Any year)"
)


# ==============================================================================
# PART I: APPENDIX F - BOUNDARY HANDLING METHODS COMPARISON
# ==============================================================================


# ==============================================================================
# 3. METHOD 0: ORIGINAL (Binning + default=0) ----
# ==============================================================================

df_m0 <- create_event_vars(df_analysis)

df_m0 <- df_m0 |>
  mutate(
    mpprior5 = row_sum_regex(df_m0, "mpprior([5-9]|1[0-3])"),
    mpconn4  = row_sum_regex(df_m0, "mpconn([4-9]|1[0-3])"),
    mppost4  = row_sum_regex(df_m0, "mppost([4-9]|1[0-3])"),
    mppost_any = row_sum_regex(df_m0, "mppost[0-9]")
  )

df_m0[vars_clean][is.na(df_m0[vars_clean])] <- 0

# NOTE: Method 0 preserves full sample with binning


# ==============================================================================
# 4. METHOD 1: DIRECT TRUNCATION (No Binning) ----
# ==============================================================================

df_m1 <- create_event_vars(df_analysis, max_prior = 5, max_conn = 4, max_post = 4)

df_m1 <- df_m1 |>
  mutate(mppost_any = row_sum_regex(df_m1, "mppost[1-4]"))

df_m1[vars_clean][is.na(df_m1[vars_clean])] <- 0

# NOTE: Method 1 uses direct truncation without binning


# ==============================================================================
# 5. METHOD 2: BALANCED SAMPLE ----
# ==============================================================================

first_approval <- df_analysis |>
  filter(Mayor_plan == 1) |>
  group_by(City_Code) |>
  summarise(first_year = min(Year), .groups = 'drop')

safe_cities <- first_approval |>
  filter(first_year >= 2008 & first_year <= 2012)

df_balanced <- df_analysis |>
  semi_join(safe_cities, by = "City_Code")

df_m2 <- create_event_vars(df_balanced)

df_m2 <- df_m2 |>
  mutate(
    mpprior5 = row_sum_regex(df_m2, "mpprior([5-9]|1[0-3])"),
    mpconn4  = row_sum_regex(df_m2, "mpconn([4-9]|1[0-3])"),
    mppost4  = row_sum_regex(df_m2, "mppost([4-9]|1[0-3])"),
    mppost_any = row_sum_regex(df_m2, "mppost[0-9]")
  )

df_m2[vars_clean][is.na(df_m2[vars_clean])] <- 0

# NOTE: Method 2 restricts to cities with approval 2008-2012
# Advantages: Complete event windows, no truncation artifacts
# Disadvantages: Substantial sample reduction (~94% excluded)

# ==============================================================================
# 6. METHOD 3: SIMPLE LAG/LEAD WITH NA (Rosa's approach) ----
# ==============================================================================

df_m3 <- df_analysis |>
  group_by(City_Code) |>
  arrange(City_Code, Year) |>
  mutate(
    plan_lead1 = lead(Mayor_plan, 1),
    plan_lead2 = lead(Mayor_plan, 2),
    plan_lead3 = lead(Mayor_plan, 3),
    plan_lead4 = lead(Mayor_plan, 4),
    plan_lead5 = lead(Mayor_plan, 5),
    plan = Mayor_plan,
    plan_lag1 = lag(Mayor_plan, 1),
    plan_lag2 = lag(Mayor_plan, 2),
    plan_lag3 = lag(Mayor_plan, 3),
    plan_lag4 = lag(Mayor_plan, 4)
  ) |>
  ungroup()

# NOTE: Method 3 follows Rosa's suggestion - no default values
# NA at boundaries, listwise deletion during regression
# Advantages: Explicit missing data handling, no imputation
# Disadvantages: Large sample loss (~65% excluded)

# ==============================================================================
# 7. RUN REGRESSIONS FOR APPENDIX F ----
# ==============================================================================

# Formula for Methods 0, 1, 2
f_boundary <- as.formula(paste(
  "Mayor_promotion3y ~ mpprior5 + mpprior4 + mpprior3 + mpprior2 +",
  "mpconn0 + mpconn1 + mpconn2 + mpconn3 + mpconn4 + mppost_any",
  "| Year + City_Code"
))

m0 <- felm(f_boundary, data = df_m0)
m1 <- felm(f_boundary, data = df_m1)
m2 <- felm(f_boundary, data = df_m2)

# Method 3: Lorenzo's approach using fixest
m3 <- feols(
  Mayor_promotion3y ~ plan + 
    plan_lead2 + plan_lead3 + plan_lead4 + plan_lead5 +
    plan_lag1 + plan_lag2 + plan_lag3 + plan_lag4 |
    Year + City_Code,
  data = df_m3, 
  cluster = ~City_Code
)

# NOTE: Sample sizes after estimation
# Method 0: N = 3647, Method 1: N = 3647, Method 2: N ~ 210, Method 3: N ~ 1269

# ==============================================================================
# 8. APPENDIX F TABLE: 4-METHOD COMPARISON ----
# ==============================================================================

# Variable mapping for table
vars_m012 <- c("mpprior5", "mpprior4", "mpprior3", "mpprior2",
               "mpconn0", "mpconn1", "mpconn2", "mpconn3", "mpconn4")

vars_m3 <- c("plan_lead5", "plan_lead4", "plan_lead3", "plan_lead2",
             "plan", "plan_lag1", "plan_lag2", "plan_lag3", "plan_lag4")

labels_9 <- labels_a14[1:9]

# Coefficient extraction functions
get_coef_felm <- function(model, varname) {
  coefs <- summary(model)$coefficients
  if (varname %in% rownames(coefs)) {
    return(list(est = coefs[varname, 1], se = coefs[varname, 2], pval = coefs[varname, 4]))
  }
  return(list(est = NA, se = NA, pval = NA))
}

get_coef_fixest <- function(model, varname) {
  tbl <- summary(model)$coeftable
  if (varname %in% rownames(tbl)) {
    return(list(est = tbl[varname, "Estimate"], 
                se = tbl[varname, "Std. Error"], 
                pval = tbl[varname, "Pr(>|t|)"]))
  }
  return(list(est = NA, se = NA, pval = NA))
}

format_coef <- function(info) {
  if (is.na(info$est)) return("")
  star <- ifelse(info$pval < 0.01, "***", ifelse(info$pval < 0.05, "**", ifelse(info$pval < 0.1, "*", "")))
  sprintf("%.3f%s", info$est, star)
}

format_se <- function(info) {
  if (is.na(info$se)) return("")
  sprintf("(%.3f)", info$se)
}

# Build table lines
table_f_lines <- c(
  "Table: Boundary Handling Methods Comparison (Appendix F)",
  "",
  sprintf("%-40s | %12s | %12s | %12s | %12s", "", "(1)", "(2)", "(3)", "(4)"),
  sprintf("%-40s | %12s | %12s | %12s | %12s", "", "Original", "No Binning", "Balanced", "Rosa"),
  paste(rep("-", 100), collapse = "")
)

for (i in seq_along(labels_9)) {
  c0 <- get_coef_felm(m0, vars_m012[i])
  c1 <- get_coef_felm(m1, vars_m012[i])
  c2 <- get_coef_felm(m2, vars_m012[i])
  c3 <- get_coef_fixest(m3, vars_m3[i])
  
  table_f_lines <- c(table_f_lines,
    sprintf("%-40s | %12s | %12s | %12s | %12s", labels_9[i], format_coef(c0), format_coef(c1), format_coef(c2), format_coef(c3)),
    sprintf("%-40s | %12s | %12s | %12s | %12s", "", format_se(c0), format_se(c1), format_se(c2), format_se(c3))
  )
}

table_f_lines <- c(table_f_lines,
  paste(rep("-", 100), collapse = ""),
  sprintf("%-40s | %12s | %12s | %12s | %12s", "Baseline (1 year before)", "0", "0", "0", "0"),
  sprintf("%-40s | %12s | %12s | %12s | %12s", "City FE", "Yes", "Yes", "Yes", "Yes"),
  sprintf("%-40s | %12s | %12s | %12s | %12s", "Year FE", "Yes", "Yes", "Yes", "Yes"),
  sprintf("%-40s | %12d | %12d | %12d | %12d", "Observations", m0$N, m1$N, m2$N, nobs(m3)),
  paste(rep("-", 100), collapse = ""),
  "*** p<0.01, ** p<0.05, * p<0.1"
)

# Save table
png("app_f_table1.png", height = 800, width = 1400, res = 120)
par(mar = c(0, 0, 0, 0))
plot.new()
text(0.5, 0.5, paste(table_f_lines, collapse = "\n"), cex = 0.85, family = "mono")
dev.off()

# ==============================================================================
# 9. APPENDIX F FIGURE: EVENT STUDY COMPARISON ----
# ==============================================================================

extract_plot_data <- function(model, method_name) {
  coefs <- summary(model)$coefficients
  plot_vars <- c("mpprior5", "mpprior4", "mpprior3", "mpprior2",
                 "mpconn0", "mpconn1", "mpconn2", "mpconn3", "mpconn4")
  
  df_plot <- data.frame(term = rownames(coefs), estimate = coefs[, 1], se = coefs[, 2]) |> 
    filter(term %in% plot_vars)
  df_plot <- rbind(df_plot, data.frame(term = "mpprior1", estimate = 0, se = 0))
  
  x_map <- c("mpprior5"=-5, "mpprior4"=-4, "mpprior3"=-3, "mpprior2"=-2, "mpprior1"=-1,
             "mpconn0"=0, "mpconn1"=1, "mpconn2"=2, "mpconn3"=3, "mpconn4"=4)
  df_plot$x <- x_map[df_plot$term]
  df_plot$ci95_lo <- df_plot$estimate - 1.96 * df_plot$se
  df_plot$ci95_hi <- df_plot$estimate + 1.96 * df_plot$se
  df_plot$ci90_lo <- df_plot$estimate - 1.645 * df_plot$se
  df_plot$ci90_hi <- df_plot$estimate + 1.645 * df_plot$se
  df_plot
}

create_plot <- function(model, title, subtitle = "") {
  df_plot <- extract_plot_data(model, title)
  ggplot(df_plot, aes(x = x, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = -0.5, linetype = "dashed") +
    geom_errorbar(aes(ymin = ci95_lo, ymax = ci95_hi), width = 0, linewidth = 0.5) +
    geom_errorbar(aes(ymin = ci90_lo, ymax = ci90_hi), width = 0, linewidth = 1.2) +
    geom_point(fill = "white", shape = 21, size = 3, stroke = 1) +
    scale_x_continuous(breaks = -5:4, labels = c("<=-5", "-4", "-3", "-2", "-1", "0", "1", "2", "3", ">=4")) + 
    scale_y_continuous(limits = c(-0.6, 1.4), breaks = seq(-0.4, 1.2, 0.2)) +
    labs(title = title, subtitle = subtitle, y = "Effect on Promotion", x = "Years Relative to Approval") +
    theme(plot.title = element_text(hjust = 0.5, size = 11),
          plot.subtitle = element_text(hjust = 0.5, size = 9, color = "gray40"))
}

create_lorenzo_plot <- function(model, title) {
  tbl <- summary(model)$coeftable
  coefs <- tbl[, "Estimate"]
  ses <- tbl[, "Std. Error"]
  var_names <- c("plan_lead5", "plan_lead4", "plan_lead3", "plan_lead2", 
                 "plan", "plan_lag1", "plan_lag2", "plan_lag3", "plan_lag4")
  x_vals <- c(-5, -4, -3, -2, 0, 1, 2, 3, 4)
  
  df_plot <- data.frame(term = var_names, x = x_vals, estimate = coefs[var_names], se = ses[var_names])
  df_plot <- rbind(df_plot, data.frame(term = "plan_lead1", x = -1, estimate = 0, se = 0))
  df_plot <- df_plot[order(df_plot$x), ]
  df_plot$ci95_lo <- df_plot$estimate - 1.96 * df_plot$se
  df_plot$ci95_hi <- df_plot$estimate + 1.96 * df_plot$se
  df_plot$ci90_lo <- df_plot$estimate - 1.645 * df_plot$se
  df_plot$ci90_hi <- df_plot$estimate + 1.645 * df_plot$se
  
  ggplot(df_plot, aes(x = x, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = -0.5, linetype = "dashed") +
    geom_errorbar(aes(ymin = ci95_lo, ymax = ci95_hi), width = 0, linewidth = 0.5) +
    geom_errorbar(aes(ymin = ci90_lo, ymax = ci90_hi), width = 0, linewidth = 1.2) +
    geom_point(fill = "white", shape = 21, size = 3, stroke = 1) +
    scale_x_continuous(breaks = -5:4, labels = c("<=-5", "-4", "-3", "-2", "-1", "0", "1", "2", "3", ">=4")) + 
    scale_y_continuous(limits = c(-0.6, 1.4), breaks = seq(-0.4, 1.2, 0.2)) +
    labs(title = title, subtitle = paste0("N = ", nobs(model)), y = "Effect on Promotion", x = "Years Relative to Approval") +
    theme(plot.title = element_text(hjust = 0.5, size = 11),
          plot.subtitle = element_text(hjust = 0.5, size = 9, color = "gray40"))
}

p0 <- create_plot(m0, "Method 0: Original", paste0("N = ", m0$N))
p1 <- create_plot(m1, "Method 1: No Binning", paste0("N = ", m1$N))
p2 <- create_plot(m2, "Method 2: Balanced Sample", paste0("N = ", m2$N))
p3 <- create_lorenzo_plot(m3, "Method 3: Lorenzo's Approach")

p_combined_f <- grid.arrange(p0, p1, p2, p3, ncol = 2, nrow = 2,
                             top = "Event Study: Boundary Handling Methods Comparison")

ggsave("app_f_figure1.png", p_combined_f, width = 12, height = 10)



# ==============================================================================
# PART II: APPENDIX G - CONTROL VARIABLE AND SAMPLE VARIATION
# ==============================================================================


# ==============================================================================
# 10. VERIFY SUB-PROVINCIAL CITY EXCLUSION ----
# ==============================================================================

# Cities that should be excluded but have fsj2 = 0
excluded_cities_list <- data.frame(
  City_Code = c(5101, 6101, 3201, 2301, 3702, 3302, 4201, 
                3502, 3301, 2101, 2102, 2201, 3701, 4403, 4401),
  City_Name = c("Chengdu", "Xi'an", "Nanjing", "Harbin", "Qingdao", 
                "Ningbo", "Wuhan", "Xiamen", "Hangzhou", "Shenyang", 
                "Dalian", "Changchun", "Jinan", "Shenzhen", "Guangzhou")
)

# Identify problem cities (fsj2=0 but should be excluded)
problem_cities <- df_main |>
  filter(fsj2 == 0) |>
  filter(City_Code %in% excluded_cities_list$City_Code) |>
  distinct(City_Code) |>
  left_join(excluded_cities_list, by = "City_Code")

# NOTE: Shenyang (2101) and Shenzhen (4403) have fsj2=0 but are sub-provincial cities
# These should theoretically be excluded but remain in the sample due to data coding

# ==============================================================================
# 11. CREATE THREE SAMPLES FOR APPENDIX G ----
# ==============================================================================

# Sample A: Original (fsj2 == 0 only)
df_sample_A <- df_analysis

# Sample B: Strict exclusion (remove Shenyang and Shenzhen)
problem_city_codes <- c(2101, 4403)
df_sample_B <- df_analysis |>
  filter(!(City_Code %in% problem_city_codes))

# NOTE: Sample A has 3647 obs, Sample B has 3619 obs (28 observations removed)

# ==============================================================================
# 12. CREATE EVENT STUDY VARIABLES FOR APPENDIX G ----
# ==============================================================================

# Use Method 0 approach for both samples
df_reg_A <- create_event_vars(df_sample_A)
df_reg_A <- df_reg_A |>
  mutate(
    mpprior5 = row_sum_regex(df_reg_A, "mpprior([5-9]|1[0-3])"),
    mpconn4  = row_sum_regex(df_reg_A, "mpconn([4-9]|1[0-3])"),
    mppost4  = row_sum_regex(df_reg_A, "mppost([4-9]|1[0-3])"),
    mppost_any = row_sum_regex(df_reg_A, "mppost[0-9]")
  )
df_reg_A[vars_clean][is.na(df_reg_A[vars_clean])] <- 0

df_reg_B <- create_event_vars(df_sample_B)
df_reg_B <- df_reg_B |>
  mutate(
    mpprior5 = row_sum_regex(df_reg_B, "mpprior([5-9]|1[0-3])"),
    mpconn4  = row_sum_regex(df_reg_B, "mpconn([4-9]|1[0-3])"),
    mppost4  = row_sum_regex(df_reg_B, "mppost([4-9]|1[0-3])"),
    mppost_any = row_sum_regex(df_reg_B, "mppost[0-9]")
  )
df_reg_B[vars_clean][is.na(df_reg_B[vars_clean])] <- 0

# ==============================================================================
# 13. RUN REGRESSIONS FOR APPENDIX G ----
# ==============================================================================

# Model 1: With mppost_any (Original specification)
f_with_mppost <- as.formula(paste(
  "Mayor_promotion3y ~ mpprior5 + mpprior4 + mpprior3 + mpprior2 +",
  "mpconn0 + mpconn1 + mpconn2 + mpconn3 + mpconn4 + mppost_any",
  "| Year + City_Code"
))

# Model 2: Without mppost_any
f_without_mppost <- as.formula(paste(
  "Mayor_promotion3y ~ mpprior5 + mpprior4 + mpprior3 + mpprior2 +",
  "mpconn0 + mpconn1 + mpconn2 + mpconn3 + mpconn4",
  "| Year + City_Code"
))

m1_with_mppost <- felm(f_with_mppost, data = df_reg_A)
m2_without_mppost <- felm(f_without_mppost, data = df_reg_A)
m3_strict_sample <- felm(f_with_mppost, data = df_reg_B)

# NOTE: All three models have similar N (~3647 for A, ~3619 for B)

# ==============================================================================
# 14. APPENDIX G TABLE: MPPOST_ANY AND SAMPLE VARIATION ----
# ==============================================================================

get_coef_safe <- function(model, varname) {
  coefs <- summary(model)$coefficients
  if (varname %in% rownames(coefs)) {
    est <- coefs[varname, 1]; se <- coefs[varname, 2]; pval <- coefs[varname, 4]
    star <- ifelse(pval < 0.01, "***", ifelse(pval < 0.05, "**", ifelse(pval < 0.1, "*", "")))
    return(list(est = est, se = se, pval = pval, star = star))
  }
  return(list(est = NA, se = NA, pval = NA, star = ""))
}

vars_to_report <- c("mpprior5", "mpprior4", "mpprior3", "mpprior2",
                    "mpconn0", "mpconn1", "mpconn2", "mpconn3", "mpconn4",
                    "mppost_any")

table_g_lines <- c(
  "Table: Robustness Check - mppost_any Control and Sample Variation (Appendix G)",
  "",
  sprintf("%-40s | %18s | %18s | %18s", "", "(1)", "(2)", "(3)"),
  sprintf("%-40s | %18s | %18s | %18s", "", "With mppost_any", "No mppost_any", "Strict Sample"),
  paste(rep("-", 100), collapse = "")
)

for (i in seq_along(vars_to_report)) {
  v <- vars_to_report[i]
  lab <- labels_a14[i]
  
  c1 <- get_coef_safe(m1_with_mppost, v)
  c2 <- get_coef_safe(m2_without_mppost, v)
  c3 <- get_coef_safe(m3_strict_sample, v)
  
  fmt1 <- ifelse(is.na(c1$est), "", sprintf("%.3f%s", c1$est, c1$star))
  fmt2 <- ifelse(is.na(c2$est), "", sprintf("%.3f%s", c2$est, c2$star))
  fmt3 <- ifelse(is.na(c3$est), "", sprintf("%.3f%s", c3$est, c3$star))
  
  se1 <- ifelse(is.na(c1$se), "", sprintf("(%.3f)", c1$se))
  se2 <- ifelse(is.na(c2$se), "", sprintf("(%.3f)", c2$se))
  se3 <- ifelse(is.na(c3$se), "", sprintf("(%.3f)", c3$se))
  
  table_g_lines <- c(table_g_lines,
    sprintf("%-40s | %18s | %18s | %18s", lab, fmt1, fmt2, fmt3),
    sprintf("%-40s | %18s | %18s | %18s", "", se1, se2, se3)
  )
}

table_g_lines <- c(table_g_lines,
  paste(rep("-", 100), collapse = ""),
  sprintf("%-40s | %18s | %18s | %18s", "City FE", "Yes", "Yes", "Yes"),
  sprintf("%-40s | %18s | %18s | %18s", "Year FE", "Yes", "Yes", "Yes"),
  sprintf("%-40s | %18d | %18d | %18d", "Observations", m1_with_mppost$N, m2_without_mppost$N, m3_strict_sample$N),
  paste(rep("-", 100), collapse = ""),
  "*** p<0.01, ** p<0.05, * p<0.1"
)

png("app_g_table1.png", height = 800, width = 1100, res = 120)
par(mar = c(0, 0, 0, 0))
plot.new()
text(0.5, 0.5, paste(table_g_lines, collapse = "\n"), cex = 0.85, family = "mono")
dev.off()

# ==============================================================================
# 15. APPENDIX G FIGURE: WITH VS WITHOUT MPPOST_ANY ----
# ==============================================================================

p_with <- create_plot(m1_with_mppost, "(1) With mppost_any", paste0("N = ", m1_with_mppost$N))
p_without <- create_plot(m2_without_mppost, "(2) Without mppost_any", paste0("N = ", m2_without_mppost$N))
p_strict <- create_plot(m3_strict_sample, "(3) Strict Sample", paste0("N = ", m3_strict_sample$N))

p_combined_g <- grid.arrange(p_with, p_without, p_strict, ncol = 3, nrow = 1,
                             top = "Event Study: mppost_any Control and Sample Variation")

ggsave("app_g_figure1.png", p_combined_g, width = 15, height = 5)



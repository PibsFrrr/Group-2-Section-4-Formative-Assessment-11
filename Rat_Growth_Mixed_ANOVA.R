# ==============================================================================
# Formative Assessment 11: Mixed-Design Repeated Measures ANOVA
# BIO2105 - Group 2 - Biostatistics
# ==============================================================================

#Authors: Ayap, Princess Amberlin Mae | Domingo, Margaret | Ferrer, Phoebe Anne | Gamido, Dhen Angelo | Pepito, Yvonna Roselle 
#Section 4

#November 22, 2026


# ==============================================================================
# Part 1: Data Preparation & Exploration
# ==============================================================================



# ==============================================================================
# Part 2: Descriptive Statistics & Data Visualization
# ==============================================================================



# ==============================================================================
# Part 3: Assumption Checking
# ==============================================================================

# 1. Normality: Shapiro-Wilk test per cell (trt x time) and Q-Q plots
rats %>%
  group_by(trt, time) %>%
  shapiro_test(y)

ggqqplot(rats, "y", ggtheme = theme_bw()) +
  facet_grid(time ~ trt)

# 2. Outliers Check
rats %>%
  group_by(trt, time) %>%
  identify_outliers(y)

# 3. Homogeneity of Variances: Levene's test at each measurement occasion
rats %>%
  group_by(time) %>%
  levene_test(y ~ trt)

# 4. Homogeneity of Covariance Matrices: Box's M test on wide-format data using α = 0.001
rats_wide <- rats %>%
  pivot_wider(id_cols = c(id, trt), names_from = time, values_from = y)

box_m(rats_wide[, -c(1, 2)], rats_wide$trt)

# 5. Sphericity: Mauchly's test and Greenhouse-Geisser correction
rats_stat_2WMAnova <- rstatix::anova_test(data = rats, dv = y, wid = id, between = trt, within = time)

rats_stat_2WMAnova$`Mauchly's Test for Sphericity`

# If p > 0.05, sphericity is met (no correction needed)
# Since Mauchly's test yeilded a p = 1.41e-17 (p < 0.001) < 0.05, sphericity was violated therefore below is Greenhouse-Geisser correction being applied.
rats_stat_2WMAnova$`Sphericity Corrections`


# ==============================================================================
# Part 4: Mixed-Design Repeated Measures ANOVA
# ==============================================================================



# ==============================================================================
# Part 5: Post Hoc Analysis & Interaction Decomposition
# ==============================================================================



# ==============================================================================
# Part 6: APA-Style Interpretation & Conclusion
# ==============================================================================



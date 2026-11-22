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

# 5.1 Treatment Differences per Week
# Simple One-Way ANOVA and Tukey's HSD

weeks <- levels(rats$time)

for (wk in weeks) {

  cat("\n====================================\n")
  cat("RESULTS FOR", wk, "\n")
  cat("====================================\n")

  week_data <- subset(rats, time == wk)

  model <- aov(y ~ trt, data = week_data)

  cat("\nOne-way ANOVA:\n")
  print(summary(model))

  cat("\nTukey's HSD Pairwise Comparisons:\n")
  print(TukeyHSD(model))
}

# INTERPRETATION: TREATMENT DIFFERENCES PER WEEK

# Week 0: No significant treatment differences
# were observed (p = 0.792).
#
# Week 1: No significant treatment differences
# were observed (p = 0.769).
#
# Week 2: No significant treatment differences
# were observed (p = 0.0599), although the result
# was close to the 0.05 significance level.
#
# Week 3: Significant treatment differences were
# observed (p = 0.000678). Thiouracil differed
# significantly from Control and Thyroxin.
#
# Week 4: Significant treatment differences were
# observed (p = 0.0000182). Thiouracil again
# differed significantly from Control and Thyroxin.
#
# Control and Thyroxin did not significantly
# differ during Weeks 3 and 4.


# 5.2 Time Differences per Treatment
# Repeated-Measures ANOVA and Holm-Adjusted Paired t-tests

rats <- rats[order(rats$trt, rats$id, rats$time), ]

treatments <- levels(rats$trt)

for (t in treatments) {

  cat("\n====================================\n")
  cat("RESULTS FOR", t, "\n")
  cat("====================================\n")

  treatment_data <- subset(rats, trt == t)

  cat("\nRepeated-Measures ANOVA:\n")

  model <- aov(y ~ time + Error(id/time), data = treatment_data)

  print(summary(model))

  cat("\nHolm-Adjusted Paired t-tests:\n")

  pairwise_results <- pairwise.t.test(
    x = treatment_data$y,
    g = treatment_data$time,
    paired = TRUE,
    p.adjust.method = "holm"
  )

  print(pairwise_results)
}

# INTERPRETATION: TIME DIFFERENCES PER TREATMENT

# Control:
# Significant changes were observed across time
# (F(4,36) = 398.8, p < 0.001).
# All pairwise week comparisons were significant
# after Holm adjustment.
#
# Thiouracil:
# Significant changes were observed across time
# (F(4,36) = 225.1, p < 0.001).
# All pairwise week comparisons were significant
# after Holm adjustment.
#
# Thyroxin:
# Significant changes were observed across time
# (F(4,24) = 160.9, p < 0.001).
# All pairwise week comparisons were significant
# after Holm adjustment.


# 5.3 OVERALL INTERPRETATION: INTERACTION DECOMPOSITION

# The simple main effects analyses showed that
# treatment differences were not significant
# during Weeks 0, 1, and 2 but became significant
# during Weeks 3 and 4.
#
# In particular, the Thiouracil group differed
# significantly from the Control and Thyroxin
# groups during the later observation weeks.
#
# Meanwhile, all three treatment groups showed
# significant changes over time.
#
# These findings suggest that the treatment
# trajectories diverged over the five-week period,
# particularly involving the Thiouracil group

# ==============================================================================
# Part 6: APA-Style Interpretation & Conclusion
# ==============================================================================



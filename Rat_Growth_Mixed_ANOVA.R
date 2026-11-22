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

# 1.1 Load required libraries
library(nlmm)
library(tidyverse)
library(rstatix)
library(ggpubr)

# 1.2 Load dataset
data("rats", package = "nlmm")

# 1.3 Format factor variables with clear labels
rats <- rats %>%
  mutate(
    id = factor(id),
    trt = factor(
      trt,
      levels = c(1, 2, 3),
      labels = c("Control", "Thiouracil", "Thyroxin")
    ),
    time = factor(
      time,
      levels = 0:4,
      labels = c("Week 0", "Week 1", "Week 2", "Week 3", "Week 4")
    )
  )

# 1.4 Inspect data structure
str(rats)
dim(rats)
head(rats)
head.matrix(rats, n = 135) 
table(rats$trt)

# Guide Question Answer

# - Why is the dataset in long format?
#   Answers to Guide questions: The data-set is in long format because there are repeating values found in column one, the subject identifier. ID is necessary is order to compare the changes in a participant as time passes, since the data-set aims to compare weekly changes in weight gain depending on treatment type. 

# ==============================================================================
# Part 2: Descriptive Statistics & Data Visualization
# ==============================================================================

# 2.1 Descriptive Summary
rats %>%
  group_by(trt, time) %>%
  get_summary_stats(y, type = "mean_sd")

# 2.2 Visualizations

# Boxplot across time points by treatment group
ggboxplot(
  rats,
  x = "time",
  y = "y",
  color = "trt",
  palette = "jco",
  add = "jitter"
) +
  labs(
    title = "Body Weight Distribution Across Weeks by Treatment",
    x = "Measurement Occasion",
    y = "Body Weight (grams)",
    color = "Treatment"
  )

# Interaction plot of mean body weights over time
rats %>%
  group_by(trt, time) %>%
  summarise(mean_weight = mean(y), .groups = "drop") %>%
  ggplot(aes(x = time, y = mean_weight, color = trt, group = trt)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(
    title = "Mean Growth Trajectories by Treatment Group",
    x = "Measurement Occasion",
    y = "Mean Body Weight (grams)",
    color = "Treatment"
  ) +
  theme_minimal()

# Guide Questions Answers

# - Baseline Highest Average: [Control=50kg, Thiouracil=50kg]
# - Growth Patterns per Group: [Control group had a weight gain from between 50kg to over 155kg from weeks 0 to 4, Thiouracil treatment had a weight gain spanning from 50kg to 125kg from weeks 0 to 4, Thyroxin had a weight gain spanning from 50kg to above 155kg from weeks 0 to 4]
# - Visual Evidence of Interaction: [Seen in the interacton plot that there is a positive relationship between weight and time, as time passes, weight increases steadily]
  
  
# ==============================================================================
# Part 3: Assumption Checking
# ==============================================================================

# 3.1 Normality: Shapiro-Wilk test per cell (trt x time) and Q-Q plots
rats %>%
  group_by(trt, time) %>%
  shapiro_test(y)

ggqqplot(rats, "y", ggtheme = theme_bw()) +
  facet_grid(time ~ trt)

# 3.2 Outliers Check
rats %>%
  group_by(trt, time) %>%
  identify_outliers(y)

# 3.3 Homogeneity of Variances: Levene's test at each measurement occasion
rats %>%
  group_by(time) %>%
  levene_test(y ~ trt)

# 3.4 Homogeneity of Covariance Matrices: Box's M test on wide-format data using α = 0.001
rats_wide <- rats %>%
  pivot_wider(id_cols = c(id, trt), names_from = time, values_from = y)

box_m(rats_wide[, -c(1, 2)], rats_wide$trt)

# 3.5 Sphericity: Mauchly's test and Greenhouse-Geisser correction
rats_stat_2WMAnova <- rstatix::anova_test(data = rats, dv = y, wid = id, between = trt, within = time)

rats_stat_2WMAnova$`Mauchly's Test for Sphericity`

# If p > 0.05, sphericity is met (no correction needed)
# Since Mauchly's test yeilded a p = 1.41e-17 (p < 0.001) < 0.05, sphericity was violated therefore below is Greenhouse-Geisser correction being applied.
rats_stat_2WMAnova$`Sphericity Corrections`

# 4.5 Summary of Assumptions

#   The majority of the dataset met the assumptions for a mixed-design ANOVA, although on sphericity the Greenhouse-Geisser correction was applied. Normality was met across 14 out of 15 treatment-by-time cells ($p > 0.05$), there was a single data that stood out a bit (in Treatment 3 at Week 1 ($W = 0.789, p = 0.0318$)), although since the Q-Q plots still aligned well overall and mixed ANOVA handles slight normality deviations, this assumption holds. Rat `id = 19` (Treatment 2) had two outliers at Week 1 ($y = 61$) and Week 2 ($y = 78$), although neither was tagged as extreme (`is.extreme = FALSE`), so technically no extreme outliers came from this dataset. Both homogeneity of variances (Levene's test, $p = 0.124\text{–}0.672$) and homogeneity of covariance matrices (Box's M test, $\chi^2(30) = 22.4, p = 0.839$ at $\alpha = 0.001$) were satisfied. Finally, Mauchly's test was significant ($W = 0.011, p = 1.41 \times 10^{-17} < 0.05$), which meant sphericity was violated and the Greenhouse-Geisser correction ($\varepsilon = 0.342$) was applied to the within-subjects ANOVA results.

# ==============================================================================
# Part 4: Mixed-Design Repeated Measures ANOVA
# ==============================================================================

# 4.1 Fit two-way mixed-design repeated measures ANOVA
rats_stat_2WMAnova <- rstatix::anova_test(
  data = rats,
  dv = y,
  wid = id,
  between = trt,
  within = time
)

# 4.2 Display full ANOVA output and Mauchly's Test of Sphericiity
rats_stat_2WMAnova

# 4.3 Display Mauchly's test and Sphericity corrections explicitly
rats_stat_2WMAnova$`Mauchly's Test for Sphericity`
rats_stat_2WMAnova$`Sphericity Corrections`

# 4.4 Extract Greenhouse-Geisser corrected ANOVA table
rstatix::get_anova_table(rats_stat_2WMAnova)

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


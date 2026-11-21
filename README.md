# Formative Assessment 11: Mixed-Design Repeated Measures ANOVA
## BIO2105 - Group 2 - Biostatistics

**Authors:**  Ayap, Princess Amberlin Mae | Domingo, Margaret | Ferrer, Phoebe Anne | Gamido, Dhen Angelo | Pepito, Yvonna Roselle (Section 4)

**November 22, 2026**

---


## Research Background

In an experimental study of rat growth (Box, 1950), rats were assigned to three treatment groups: **Control** (no additive), **Thiouracil**, and **Thyroxin** administered via drinking water. Body weights were recorded across five measurement occasions: **Baseline (Week 0)** through **Week 4**.

Following an accident, three rats were removed from the Thyroxin group, leaving $N = 27$ rats and 135 total observations.

## Research Objective

To determine whether rat body weight differs significantly across treatment groups and measurement occasions, and specifically to evaluate whether growth trajectories over time depend on the treatment received (**Treatment** $\times$ Time Interaction).

## Dataset Description

The dataset (`rats`) is sourced from the `nlmm` R package and originates from the growth curve experiment by Box (1950). 

* **Sample Size:** 27 rats (10 Control, 10 Thiouracil, 7 Thyroxin—three rats were removed from the Thyroxin group following an accident) totaling 135 observations.
* **Variables:**
  * `id`: Individual rat identifier (Nominal / Subject ID)
  * `trt`: Between-subjects factor (`Control`, `Thiouracil`, `Thyroxin`)
  * `time`: Within-subjects factor (`Week 0` through `Week 4`)
  * `y`: Dependent variable representing body weight in grams (Continuous)

## Repository Guide

* `README.md`: Project title, Research Objectives, Repository Guide, and exact instructions to reproduce our analysis.
* `Rat_Growth_Mixed_ANOVA.R`: Documented executable R script covering all parts of the Paper.
* `Rat_Growth_Mixed_ANOVA.Rmd`: R Markdown source document integrating code, outputs, visualizations, and APA interpretations.
* `Rat_Growth_Mixed_ANOVA.html`: Rendered HTML report generated from the R Markdown source.
* `Rat_Growth_Mixed_ANOVA.pdf`: Rendered PDF version of the report.
* `data`: Folder containing the processed data used for this report.
  * `data/Processed_Rat_Growth_Dataset.csv`: Local CSV copy of the processed dataset.
  * `data/Raw_Rat_Growth_Dataset.csv`: Raw data exported from Rstudio from the code:
    ```r
    data("rats", package = "nlmm")

## How to Reproduce the Analysis

1. Clone or download this repository to your local machine.
2. Open R or RStudio.
3. Install the required R packages by running:
   ```r
   install.packages(c("nlmm", "tidyverse", "rstatix", "ggpubr"))

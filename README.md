# Diving-Judge-Assessment

### Overview
In collaboration with the **Dive+** app, this project addresses the subjectivity and bias in diving competition scoring. 

We developed a statistical framework to objective evaluate judge performance, transforming raw, subjective scores into **"Actionable Intelligence"**—simple, interpretable metrics that help judges identify their own biases and improve fairness.

### Key Features & Methodology
Instead of using simple averages which are biased by dive difficulty, we use a **Leave-One-Out (LOO) Baseline** approach.

#### 1. The Core Logic: Judge-out Baseline
We establish a "ground truth" for every dive by calculating the consensus of the panel *excluding* the judge in question. This allows us to calculate the **Residual ($r_{jd}$)**: the deviation of a judge's score from the panel consensus.

#### 2. Performance Metrics
We aggregate these residuals to create a "6-Card Score" profile for each judge:
* **Consistency:** Measures the volatility of a judge's residuals. A lower standard deviation indicates a more predictable and stable judge.
* **Severity/Leniency:** Determines if a judge systematically scores higher (+) or lower (-) than their peers.
* **Extremeness:** Tracks how frequently a judge is the outlier (max/min) and the magnitude of their deviation (Gap Index).
* **Range Utilization:** Analyzes if a judge uses the full scoring scale or sticks to "safe" central scores.

### Interactive Dashboard
This repository includes a prototype **Shiny App** (or R Markdown report) that visualizes these metrics. 

*(Shiny App will be updated here)*

**Key Visualizations:**
* **Bland-Altman Plots:** To detect systematic bias across different score levels.
* **Judge Profiling Cards:** Summary statistics for individual judge feedback.

### Tech Stack
* **Language:** R
* **Libraries:** `tidyverse`, `shiny`, `ggplot2`, `dplyr`
* **Reporting:** R Markdown / Shiny

### Repository Structure
* `R/`: Statistical functions for baseline and residual calculations.
* `app/`: Source code for the Shiny dashboard.
* `data/`: Anonymized sample data used for demonstration.
* `docs/`: Methodological explanations and reports.

---
*Note: This project uses anonymized data for demonstration purposes.*
